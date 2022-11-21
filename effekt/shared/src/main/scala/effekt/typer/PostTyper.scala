package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.{ Def, ExprTarget, IdTarget, MatchPattern, Tree }
import effekt.source.Tree.{ Query, Visit }

object PostTyper extends Phase[Typechecked, Typechecked] {

  val phaseName = "post-typer"

  def run(input: Typechecked)(implicit C: Context) =
    val Typechecked(source, tree, mod) = input
    Wellformedness.check(mod, tree)

    if (Context.messaging.hasErrors) { None }
    else { Some(input) }
}

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
object Wellformedness extends Visit[WFContext] {

  def check(mod: Module, tree: source.ModuleDecl)(using Context): Unit = {

    // Effects that are lexically in scope at the top level
    // We use dependencies instead of imports, since types might mention effects of transitive imports.
    val toplevelEffects = mod.dependencies.foldLeft(mod.effects) { case (effs, mod) =>
      effs ++ mod.effects
    }

    given WFContext = WFContext(toplevelEffects.distinct)

    query(tree)
  }

  override def expr(using Context, WFContext) = {

    /**
     * For handlers we check that the return type does not mention any bound capabilities
     */
    case tree @ source.TryHandle(prog, handlers) =>
      val bound = Context.annotation(Annotations.BoundCapabilities, tree).map(_.capture).toSet
      val usedEffects = Context.annotation(Annotations.InferredEffect, tree)
      val tpe = Context.inferredTypeOf(prog)
      val selfRegion = Context.getSelfRegion(tree)

      val free = freeCapture(tpe)
      val escape = free intersect bound

      if (escape.nonEmpty) {
        Context.at(prog) {
          Context.error(pp"The return type ${tpe} of the handled statement is not allowed to refer to any of the bound capabilities, but mentions: ${CaptureSet(escape)}")
        }
      }

      if (free contains selfRegion) {
        Context.at(prog) {
          Context.error(pp"The return type ${tpe} of the handler body must not mention the self region.")
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

    case tree @ source.Match(scrutinee, clauses) => Context.at(tree) {
      val tpe = Context.inferredTypeOf(scrutinee)
      checkExhaustive(tpe, clauses)

      query(scrutinee)
      clauses foreach { cl => scoped { query(cl) }}
    }

    case tree @ source.BlockLiteral(tps, vps, bps, body) =>
      val selfRegion = Context.getSelfRegion(tree)
      val returnType = Context.inferredTypeOf(body)

      if (freeCapture(returnType) contains selfRegion) {
        Context.at(body) {
          Context.error(s"The return type ${returnType} of the function body must not mention the region of the function itself.")
        }
      }

      scoped { query(body) }
  }

  override def defn(using C: Context, WF: WFContext) = {
    /**
     * For functions we check that the self region does not leave as part of the return type.
     */
    case tree @ source.FunDef(id, tps, vps, bps, ret, body) =>
      val selfRegion = Context.getSelfRegion(tree)
      val returnType = Context.inferredTypeOf(body)

      if (freeCapture(returnType) contains selfRegion) {
        Context.at(body) {
          Context.error(pp"The return type ${returnType} of the function body must not mention the region of the function itself.")
        }
      }

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
  def checkExhaustive(scrutinee: ValueType, cls: List[source.MatchClause])(using Context): Unit = {

    import source.MatchPattern.*

    case class Clause(patterns: Map[Trace, TagPattern], tree: source.MatchClause)

    enum Trace {
      case Root
      case Child(c: Constructor, field: Symbol, outer: Trace)
    }

    val initialClauses: List[Clause] = cls.map { c => c.pattern match {
      case _: AnyPattern | _: IgnorePattern => Clause(Map.empty, c)
      case t: TagPattern => Clause(Map(Trace.Root -> t), c)
      case _: LiteralPattern => Context.abort("Literal patterns not supported at the moment.")
    }}


    object redundant {
      def use(cl: source.MatchClause): Unit = redundantClauses = redundantClauses - hashClause(cl)
      def get: List[source.MatchClause] = redundantClauses.map(_._2).toList

      private var redundantClauses: Set[(Int, source.MatchClause)] = cls.map(hashClause).toSet
      private def hashClause(cl: source.MatchClause): (Int, source.MatchClause) = (System.identityHashCode(cl), cl)
    }

    object missing {
      var missingCases: List[(Constructor, Trace)] = Nil
      def add(trace: Trace, c: Constructor) = missingCases = (c, trace) :: missingCases
      def get: List[(Constructor, Trace)] = missingCases
    }

    def splitType(trace: Trace, scrutinee: ValueType, clauses: List[Clause]): Unit = {
      val tpe = scrutinee match {
        case ValueType.ValueTypeApp(t, _) => t
        // TODO maybe report error here???
        case _ => return // missing.add(trace);
      }
      split(trace, tpe, clauses)
    }

    def split(trace: Trace, scrutinee: TypeConstructor, clauses: List[Clause]): Unit = {
      val constructors = scrutinee match {
        case t: TypeConstructor.DataType => t.constructors
        case t: TypeConstructor.Record => List(t.constructor)
        case _ => return // missing.add(trace);
      }

      def splitOn(c: Constructor): List[Clause] = clauses.flatMap {
        case cl @ Clause(patterns, tree) => patterns.get(trace) match {
          // Only keep clause if it matches on the same constructor
          case Some(t) if t.definition == c =>
            val fieldPatterns = (c.fields zip t.patterns).collect {
              case (field, t: TagPattern) => (Trace.Child(c, field, trace) -> t)
            }
            Some(Clause(patterns - trace ++ fieldPatterns.toMap, tree))

          // wrong constructor
          case Some(_) => None

          // not matching on this path, so it is a valid clause
          case None => Some(cl)
        }
      }

      constructors.foreach { c => splitOn(c) match {
        case Nil => missing.add(trace, c)
        case cl :: rest => work(cl, rest)
      }}
    }

    def work(next: Clause, rest: List[Clause]): Unit = next.patterns.headOption match {
      // Successful match
      case None => redundant.use(next.tree)

      // Work to do
      case Some((nextTrace, nextConstructor)) =>
        val tpe = nextConstructor.definition.tpe
        split(nextTrace, tpe, next :: rest)
    }

    splitType(Trace.Root, scrutinee, initialClauses)

    redundant.get.foreach { p =>
      Context.at(p) { Context.warning(pp"Unreachable case.") }
    }

    missing.get.foreach { (ctor, trace) =>
      Context.error(pp"Non exhaustive pattern matching, missing case for ${ ctor }")
    }
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
