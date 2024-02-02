package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.{ Def, ExprTarget, IdTarget, MatchGuard, MatchPattern, Tree }
import effekt.source.Tree.{ Query, Visit }

import scala.collection.mutable

class WFContext(var effectsInScope: List[Interface]) {
  def addEffect(i: Interface) = effectsInScope = (i :: effectsInScope).distinct
}

/**
 * Performs additional wellformed-ness checks that are easier to do on already
 * inferred and annotated trees.
 *
 * Checks:
 * - exhaustivity
 * - lexical scoping of effects
 * - escape of capabilities through types
 */
object Wellformedness extends Phase[Typechecked, Typechecked], Visit[WFContext] {

  val phaseName = "wellformedness"

  def run(input: Typechecked)(implicit C: Context) =
    val Typechecked(source, tree, mod) = input
    check(mod, tree)

    if (Context.messaging.hasErrors) { None }
    else { Some(input) }

  def check(mod: Module, tree: source.ModuleDecl)(using Context): Unit = {

    // Effects that are lexically in scope at the top level
    // We use dependencies instead of imports, since types might mention effects of transitive imports.
    val toplevelEffects = mod.dependencies.foldLeft(mod.effects) { case (effs, mod) =>
      effs ++ mod.effects
    }

    given WFContext = WFContext(toplevelEffects.distinct)

    query(tree)
  }

  override def stmt(using Context, WFContext) = {
    case stmt @ source.DefStmt(tree @ source.VarDef(id, annot, binding), rest) =>
      val tpe = Context.inferredTypeOf(rest)

      val free = freeCapture(tpe)
      val capt = tree.symbol.capture

      if free contains capt then Context.at(stmt) {
        Context.error(pp"Local variable ${id} escapes through the returned value of type ${tpe}.")
      }
  }

  override def expr(using Context, WFContext) = {

    /**
     * For handlers we check that the return type does not mention any bound capabilities
     */
    case tree @ source.TryHandle(prog, handlers) =>
      val bound = Context.annotation(Annotations.BoundCapabilities, tree).map(_.capture).toSet
      val usedEffects = Context.annotation(Annotations.InferredEffect, tree)
      val tpe = Context.inferredTypeOf(prog)

      val free = freeCapture(tpe)
      val escape = free intersect bound

      if (escape.nonEmpty) {
        Context.at(prog) {
          Context.error(pp"The return type ${tpe} of the handled statement is not allowed to refer to any of the bound capabilities, but mentions: ${CaptureSet(escape)}")
        }
      }

      // also check prog and handlers
      scoped { query(prog) }

      val typesInEffects = freeTypeVars(usedEffects)

      handlers foreach { h =>
        scoped { query(h) }

        h.clauses.foreach { cl =>
          val existentials = cl.tparams.map(_.symbol.asTypeParam)
          existentials.foreach { t =>
            if (typesInEffects.contains(t)) {
              Context.error(pp"Type variable ${t} escapes its scope as part of the effect types: ${usedEffects}")
            }
          }
        }
      }

    case tree @ source.Region(id, body) =>
      val reg = tree.symbol
      val tpe = Context.inferredTypeOf(body)

      val free = freeCapture(tpe)

      if (free contains reg.capture) {
        Context.at(body) {
          Context.error(pp"The return type ${tpe} of the statement is not allowed to refer to region ${reg}")
        }
      }

      scoped { query(body) }

    case tree @ source.Match(scrutinee, clauses, default) => Context.at(tree) {
      // TODO copy annotations from default to synthesized defaultClause (in particular positions)
      val defaultClause = default.toList.map(body => source.MatchClause(source.IgnorePattern(), Nil, body))
      checkExhaustive(scrutinee, clauses ++ defaultClause)

      query(scrutinee)
      clauses foreach { cl => scoped { query(cl) }}
      default foreach query
    }

    case tree @ source.BlockLiteral(tps, vps, bps, body) =>
      scoped { query(body) }
  }

  override def defn(using C: Context, WF: WFContext) = {
    case tree @ source.FunDef(id, tps, vps, bps, ret, body) =>
      scoped { query(body) }

    /**
     * Interface definitions bring an effect in scope that can be handled
     */
    case d @ source.InterfaceDef(id, tparams, ops, isEffect) =>
      WF.addEffect(d.symbol)
  }

  /**
   * TODO exhaustivity check should be constructive:
   * - non exhaustive pattern match should generate a list of patterns, so the IDE can insert them
   * - redundant cases should generate a list of cases that can be deleted.
   */
  def checkExhaustive(scrutinee: source.Term, cls: List[source.MatchClause])(using Context): Unit = {

    import source.MatchPattern.*

    enum Pattern {
      case Any()
      case Tag(c: Constructor, patterns: List[Pattern])
      case Literal(value: scala.Any, tpe: ValueType)
    }

    enum Condition {
      case Patterns(patterns: Map[Trace, Pattern])
      case Guard(condition: source.Term)
    }

    case class Clause(conditions: List[Condition], tree: source.MatchClause) {
      def matchesOn(scrutinee: Trace): Option[Pattern] = conditions match {
        case Condition.Patterns(patterns) :: rest =>
          patterns.get(scrutinee)
        case _ => None
      }
      def -(scrutinee: Trace): Clause = conditions match {
        case Condition.Patterns(patterns) :: rest =>
          Clause.normalized(Condition.Patterns(patterns - scrutinee) :: rest, tree)
        case _ => this
      }
      def ++(others: Map[Trace, Pattern]): Clause = conditions match {
        case Condition.Patterns(patterns) :: rest =>
          Clause.normalized(Condition.Patterns(patterns ++ others) :: rest, tree)
        case _ => Clause.normalized(Condition.Patterns(others) :: conditions, tree)
      }

      def isSatisfied: Boolean = conditions.isEmpty
    }
    object Clause {
      def normalized(conditions: List[Condition], tree: source.MatchClause): Clause =
        Clause(conditions.flatMap {
          case Condition.Patterns(ps) =>
            val norm = normalized(ps)
            if norm.isEmpty then None else Some(Condition.Patterns(norm))
          case other => Some(other)
        }, tree)
      def normalized(patterns: Map[Trace, Pattern]): Map[Trace, Pattern] = patterns collect {
        case (trace, p: Pattern.Tag) => (trace, p)
        case (trace, p: Pattern.Literal) => (trace, p)
      }
    }

    // Scrutinees are identified by tracing from the original scrutinee.
    enum Trace {
      case Root(scrutinee: source.Term)
      case Child(c: Constructor, field: Field, outer: Trace)
    }

    def preprocess(root: source.Term, cl: source.MatchClause): Clause = cl match {
      case source.MatchClause(pattern, guards, body) =>
        Clause.normalized(Condition.Patterns(Map(Trace.Root(scrutinee) -> preprocessPattern(pattern))) :: guards.map(preprocessGuard), cl)
    }
    def preprocessPattern(p: source.MatchPattern): Pattern = p match {
      case AnyPattern(id)  => Pattern.Any()
      case IgnorePattern() => Pattern.Any()
      case p @ TagPattern(id, patterns) => Pattern.Tag(p.definition, patterns.map(preprocessPattern))
      case LiteralPattern(lit) => Pattern.Literal(lit.value, lit.tpe)
      case _: OrPattern => ???
    }
    def preprocessGuard(g: source.MatchGuard): Condition = g match {
      case MatchGuard.BooleanGuard(condition) =>
        Condition.Guard(condition)
      case MatchGuard.PatternGuard(scrutinee, pattern) =>
        Condition.Patterns(Map(Trace.Root(scrutinee) -> preprocessPattern(pattern)))
    }

    // clauses are redundant if we never mark them as used during compilation
    object used {
      def use(cl: source.MatchClause): Unit = redundantClauses = redundantClauses - hashClause(cl)
      def get: List[source.MatchClause] = redundantClauses.map(_._2).toList

      private var redundantClauses: Set[(Int, source.MatchClause)] = cls.map(hashClause).toSet
      private def hashClause(cl: source.MatchClause): (Int, source.MatchClause) = (System.identityHashCode(cl), cl)

      def reportUnreachable(): Unit = get.foreach { p =>
        Context.at(p) { Context.warning(pp"Unreachable case.") }
      }
    }

    object missing {
      enum Missing {
        case Tag(constructor: Constructor, at: Trace)
        case Guard(term: source.Term)
      }

      var missingCases: List[Missing] = Nil
      def add(trace: Trace, c: Constructor) = missingCases = Missing.Tag(c, trace) :: missingCases
      def add(guard: source.Term) = missingCases = Missing.Guard(guard) :: missingCases
      def reportNonExhaustive(): Unit = missingCases.foreach {
        case Missing.Tag(ctor, trace) =>
          Context.error(pp"Non exhaustive pattern matching, missing case for ${ ctor }")
        case Missing.Guard(term) =>
          Context.at(term) {
            Context.error(pp"Non exhaustive pattern matching, guard could be false which is not covered")
          }
      }
    }

    def checkScrutinee(scrutinee: Trace, tpe: ValueType, clauses: List[Clause]): Unit = {

      /**
       * checks whether [[ clauses ]] at position [[ trace ]] cover all [[ constructors ]]
       */
      def checkDatatype(constructors: List[Constructor]): Unit = {

        def cover(ctor: Constructor): List[Clause] = clauses.flatMap { cl =>
          cl.matchesOn(scrutinee) match {
            // keep clauses that match on the same constructor
            case Some(Pattern.Tag(c, patterns)) if c == ctor =>
              val nestedPatterns  = (ctor.fields zip patterns).map { case (field, pattern) =>
                (Trace.Child(ctor, field, scrutinee) : Trace) -> pattern
              }.toMap
              Some(cl - scrutinee ++ nestedPatterns)

            // drop clauses that match on a different constructor
            case Some(Pattern.Tag(other, patterns)) => None
            // also keep if they do not match on the scrutinee at all
            case _ =>
              Some(cl)
          }
        }
        constructors.foreach { c => cover(c) match {
          // no clause covers this case: it is missing...
          case Nil => missing.add(scrutinee, c)
          case cl :: rest => process(cl, rest)
        }}
      }

      def checkLiteral(tpe: ValueType): Unit = {
        val allLiterals: List[Any] = clauses.flatMap(_.matchesOn(scrutinee)).collect { case Pattern.Literal(v, tpe) => v }.distinct

        val finite: Option[List[Any]] = tpe match {
          case builtins.TBoolean => Some(List(true, false))
          case builtins.TUnit => Some(List(()))
          case builtins.TBottom => Some(Nil)
          case _ => None
        }

        var matches  = Map.empty[Any, List[Clause]]
        val defaults = mutable.ListBuffer.empty[Clause]

        clauses.foreach { cl =>
          cl.matchesOn(scrutinee) match {
            case Some(Pattern.Literal(v, tpe)) =>
              matches = matches.updated(v, matches.getOrElse(v, Nil) :+ (cl - scrutinee))
            case _ =>
              // defaults are duplicated
              allLiterals.foreach { v => matches = matches.updated(v, matches.getOrElse(v, Nil) :+ cl) }
              defaults.addOne(cl)
          }
        }

        finite match {
          // we have a finite domain
          case Some(cases) =>
            cases.foreach { v =>
              matches.get(v) match {
                case Some(head :: tail) => process(head, tail)
                case _ => Context.error(pp"Non exhaustive pattern matching, missing case for ${v}")
              }
            }

          // clauses can never exhaust the domain (Int, String, etc.)
          case None =>
            matches.collect {
              case (_, head :: tail) => process(head, tail)
            }
            defaults.toList match {
              case Nil => Context.error(pp"Non exhaustive pattern matching, literals of type ${tpe} require a default case")
              case head :: tail => process(head, tail)
            }
        }
      }

      tpe match {
        case ValueType.ValueTypeApp(DataType(_, _, ctors), _) => checkDatatype(ctors)
        case ValueType.ValueTypeApp(Record(_, _, ctor), _)    => checkDatatype(List(ctor))
        case tpe @ (builtins.TInt | builtins.TDouble | builtins.TString | builtins.TBoolean | builtins.TUnit | builtins.TBottom) =>
          checkLiteral(tpe)

          // missing.add(trace);
        case tpe =>
          clauses match {
            case Nil => Context.error(pp"Non exhaustive pattern matching, need at least one case for scrutinee of type ${tpe}")
            // ignore scrutinee
            case head :: tail =>
              process(head, tail)
          }
      }
    }

    def process(head: Clause, alternatives: List[Clause]): Unit = head match {
      // the first clause already matches, mark it as used
      case Clause(Nil, tree) => used.use(tree)

      // we are not yet done and need to continue matching
      case Clause(Condition.Guard(term) :: rest, body) =>
        // case: guard is true
        process(Clause.normalized(rest, body), alternatives)
        // case: guard is false
        alternatives match {
          case Nil => missing.add(term)
          case head :: next => process(head, next)
        }
      case Clause(Condition.Patterns(patterns) :: rest, body) if patterns.isEmpty =>
        process(Clause.normalized(rest, body), alternatives)
      case Clause(Condition.Patterns(patterns) :: rest, body) =>
        // We have to choose the next scrutinee. We guess the approximate type by looking at the pattern it is
        // matched against.
        patterns.head match {
          case (sc, Pattern.Tag(constructor, ps)) =>
            checkScrutinee(sc, constructor.returnType, head :: alternatives)
          case (sc, Pattern.Literal(lit, tpe)) =>
            checkScrutinee(sc, tpe, head :: alternatives)
          case (sc, Pattern.Any()) =>
            Context.panic("Should not happen, since clauses are normalized.")
        }
    }

    val initialClauses: List[Clause] = cls.map(preprocess(scrutinee, _))
    checkScrutinee(Trace.Root(scrutinee), Context.inferredTypeOf(scrutinee), initialClauses)

    used.reportUnreachable()
    missing.reportNonExhaustive()
  }

  override def scoped(action: => Unit)(using C: Context, ctx: WFContext): Unit = {
    val before = ctx.effectsInScope
    action
    ctx.effectsInScope = before
  }

  override def visit[T <: Tree](t: T)(visitor: T => Unit)(using Context, WFContext): Unit = {
    // Make sure that all annotated effects are well-scoped with regard to lexical scoping
    Context.inferredEffectOption(t).foreach { effects => Context.at(t) { wellscoped(effects) } }
    visitor(t)
  }

  // TODO extend check to also check in value types
  //   (now that we have first class functions, they could mention effects).
  def wellscoped(effects: Effects)(using C: Context, WF: WFContext): Unit = {
    def checkEffect(eff: InterfaceType): Unit =
      if (!(WF.effectsInScope contains eff.typeConstructor))
        Context.abort(pp"Effect ${eff} leaves its defining lexical scope as part of the inferred type.")

    effects.toList foreach checkEffect
  }

  // Can only compute free capture on concrete sets
  def freeCapture(o: Any): Set[Capture] = o match {
    case t: symbols.Capture   => Set(t)
    case FunctionType(tparams, cparams, vparams, bparams, ret, eff) =>
      // TODO what with capabilities introduced for eff--those are bound in ret?
      freeCapture(vparams) ++ freeCapture(bparams) ++ freeCapture(ret) ++ freeCapture(eff) -- cparams.toSet
    case CaptureSet(cs) => cs
    case x: CaptUnificationVar => sys error s"Cannot compute free variables for unification variable ${x}"
    case x: UnificationVar => sys error s"Cannot compute free variables for unification variable ${x}"
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[Capture]) { case (r, t) => r ++ freeCapture(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[Capture]) { case (r, t) => r ++ freeCapture(t) }
    case _ =>
      Set.empty
  }

  def freeTypeVars(o: Any): Set[TypeVar] = o match {
    case t: symbols.TypeVar => Set(t)
    case FunctionType(tps, cps, vps, bps, ret, effs) =>
      freeTypeVars(vps) ++ freeTypeVars(bps) ++ freeTypeVars(ret) ++ freeTypeVars(effs) -- tps.toSet
    case e: Effects            => freeTypeVars(e.toList)
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case _ =>
      Set.empty
  }

  def Context(implicit ctx: Context): Context = ctx
}
