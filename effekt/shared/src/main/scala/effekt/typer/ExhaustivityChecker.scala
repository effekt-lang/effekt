package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.{ Def, ExprTarget, IdTarget, MatchGuard, MatchPattern, Tree }
import effekt.source.Tree.{ Query, Visit }
import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR }

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Like the [[ effekt.core.PatternMatchingCompiler ]], the exhaustivity checker first
 * translates clauses into an internal representation, made up of:
 *
 * - [[ Clause ]]s as list of conditions and a pointer to the original clause
 * - [[ Condition ]]s that can either be pattern matches or guards
 * - [[ Pattern ]] consisting of already resolved symbols so that the exhaustivity checker does not rely on [[ Context ]]
 *
 * Scrutinees are identified by [[ Trace ]]s from the original scrutinee. For example:
 *
 * {{{
 *   e match { case Cons(Some(n), _) =>  ... }
 *                            ^
 * }}}
 *
 * is identified by the trace
 *
 * {{{
 *    Child(Some, value, Child(Cons, head, Root(e)))
 * }}}
 *
 * Exhaustivity checking is performed by two mutually recursive functions that correspond to **two modes**:
 *
 * - [[ checkScrutinee ]] inspects the scrutinee and tries to cover the different values the scrutinee can take.
 *   For example, if it is of type `Option`, then we will check whether `Some` and `None` are covered.
 * - [[ matchClauses ]]   is more similar to [[ effekt.core.PatternMatchingCompiler.compile ]] in that it follows the
 *   structure of the clauses top-to-bottom, left-to-right.
 *
 * Most of the work is performed in [[ checkScrutinee ]], [[ matchClauses ]] mostly searches for the next scrutinee
 * and while doing this also handles guards.
 */
object ExhaustivityChecker {

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
    case Root(scrutinees: source.Term)
    case Child(c: Constructor, field: Field, outer: Trace)
  }

  // Standard cartesian product used to expand OR-patterns nested inside constructors
  // and across multiple guard positions.
  private def cartesianProduct[A](lists: List[List[A]]): List[List[A]] =
    lists.foldRight(List(List.empty[A])) { (heads, tails) =>
      for { h <- heads; t <- tails } yield h :: t
    }

  /**
   * Returns all OR-expanded alternatives of a pattern as a flat list.
   * For example, `A | B` yields `List(A, B)`, and `Ctor(A | B, C | D)` yields
   * `List(Ctor(A,C), Ctor(A,D), Ctor(B,C), Ctor(B,D))`.
   */
  def preprocessPatternAlts(p: source.MatchPattern)(using Context): List[Pattern] = p match {
    case AnyPattern(_, _) | IgnorePattern(_) => List(Pattern.Any())
    case p @ TagPattern(id, patterns, _) =>
      // Cartesian product: each field may itself be an OR, so we expand all combinations.
      val fieldAlts: List[List[Pattern]] = patterns.map(preprocessPatternAlts)
      cartesianProduct(fieldAlts).map(fields => Pattern.Tag(p.definition, fields))
    case LiteralPattern(lit, _) => List(Pattern.Literal(lit.value, lit.tpe))
    case OrPattern(patterns, _) => patterns.flatMap(preprocessPatternAlts)
    case MultiPattern(_, _) => Context.panic("Nested MultiPattern in preprocessPatternAlts")
  }

  private def preprocessPattern(p: source.MatchPattern)(using Context): Pattern = p match {
    case AnyPattern(id, _)  => Pattern.Any()
    case IgnorePattern(_) => Pattern.Any()
    case p @ TagPattern(id, patterns, _) => Pattern.Tag(p.definition, patterns.map(preprocessPattern))
    case LiteralPattern(lit, _) => Pattern.Literal(lit.value, lit.tpe)
    case MultiPattern(patterns, _) =>
      Context.panic("Multi-pattern should have been split in preprocess already / nested MultiPattern")
    case OrPattern(patterns, _) =>
      Context.panic("OR pattern should have been expanded via preprocessPatternAlts")
  }

  /**
   * Returns all OR-expanded alternatives of a guard as a flat list of Conditions.
   * A BooleanGuard always yields exactly one Condition.Guard.
   * A PatternGuard with an OR-pattern yields one Condition.Patterns per alternative.
   */
  def preprocessGuardAlts(g: source.MatchGuard)(using Context): List[Condition] = g match {
    case MatchGuard.BooleanGuard(condition, _) =>
      List(Condition.Guard(condition))
    case MatchGuard.PatternGuard(scrutinee, pattern, _) =>
      preprocessPatternAlts(pattern).map { p =>
        Condition.Patterns(Map(Trace.Root(scrutinee) -> p))
      }
  }

  /**
   * Expands OR-patterns (including those nested inside constructors and guards) by returning
   * a List[Clause] — one per combination of alternatives — all pointing to the same
   * source.MatchClause so that redundancy tracking remains correct.
   */
  def preprocess(roots: List[source.Term], cl: source.MatchClause)(using Context): List[Clause] = {

    // Fold guards into the accumulator of partial clause condition lists.
    // Each guard position may itself expand into multiple alternatives (OR in PatternGuard),
    // so we take a cartesian product across all guard positions.
    def withGuards(baseConds: List[Condition], guards: List[source.MatchGuard]): List[List[Condition]] =
      guards.foldLeft(List(baseConds)) { (accClauses, guard) =>
        val guardAlts = preprocessGuardAlts(guard)
        accClauses.flatMap(conds => guardAlts.map(g => conds :+ g))
      }

    (roots, cl) match {
      case (List(root), source.MatchClause(pattern, guards, body, _)) =>
        for {
          p     <- preprocessPatternAlts(pattern)
          base   = Condition.Patterns(Map(Trace.Root(root) -> p))
          conds <- withGuards(List(base), guards)
        } yield Clause.normalized(conds, cl)

      case (roots, source.MatchClause(MultiPattern(patterns, _), guards, body, _)) =>
        val perRootAlts: List[List[(Trace, Pattern)]] =
          (roots zip patterns).map { case (root, pat) =>
            preprocessPatternAlts(pat).map(p => Trace.Root(root) -> p)
          }
        for {
          pairs <- cartesianProduct(perRootAlts)
          base   = Condition.Patterns(pairs.toMap)
          conds <- withGuards(List(base), guards)
        } yield Clause.normalized(conds, cl)

      case _ =>
        List(Context.abort("Malformed multi-match"))
    }
  }

  /**
   * Gathers and reports information about redundant and missing clauses
   *
   * TODO exhaustivity check should be constructive:
   * - non exhaustive pattern match should generate a list of patterns, so the IDE can insert them
   * - redundant cases should generate a list of cases that can be deleted.
   */
  class Exhaustivity(allClauses: List[source.MatchClause], originalScrutinees: List[source.Term]) {

    // Redundancy Information
    // ----------------------
    // clauses are redundant if we never mark them as used during compilation
    private var redundantClauses: Set[(Int, source.MatchClause)] = allClauses.map(hashClause).toSet
    private def hashClause(cl: source.MatchClause): (Int, source.MatchClause) = (System.identityHashCode(cl), cl)
    private def redundant: List[source.MatchClause] = redundantClauses.map(_._2).toList

    def use(cl: source.MatchClause): Unit = redundantClauses = redundantClauses - hashClause(cl)

    // Missing Clauses
    // ---------------
    enum Missing {
      case Tag(constructor: Constructor, at: Trace)
      case Guard(term: source.Term)
      case Literal(value: Any, tpe: ValueType, at: Trace)
      case Default(tpe: ValueType, at: Trace)
    }
    private var missingCases: List[Missing] = Nil

    def missingConstructor(c: Constructor, at: Trace) = missingCases = Missing.Tag(c, at) :: missingCases
    def missingGuard(guard: source.Term) = missingCases = Missing.Guard(guard) :: missingCases
    def missingDefault(tpe: ValueType, at: Trace) = missingCases = Missing.Default(tpe, at) :: missingCases
    def missingLiteral(value: Any, tpe: ValueType, at: Trace) = missingCases = Missing.Literal(value, tpe, at) :: missingCases

    // Error Reporting
    // ---------------
    def reportNonExhaustive()(using C: ErrorReporter): Unit = {
      @tailrec
      def traceToCase(at: Trace, acc: String): String = at match {
        case Trace.Root(_) if originalScrutinees.length == 1 => acc
        case Trace.Root(e) => originalScrutinees.map { f => if e == f then acc else "_" }.mkString(", ")
        case Trace.Child(childCtor, field, outer) =>
          val newAcc = s"${childCtor.name}(${childCtor.fields.map { f => if f == field then acc else "_" }.mkString(", ")})"
          traceToCase(outer, newAcc)
      }

      missingCases.foreach {
        case Missing.Tag(ctor, at) =>
          val missingSubcase = ctor.fields match
            case Nil => s"${ctor.name}()"
            case _ => s"${ctor.name}(${ctor.fields.map { _f => "_" }.mkString(", ")})"
          val missingCase = traceToCase(at, missingSubcase)

          C.error(pp"Non-exhaustive pattern matching, missing case ${missingCase}")
        case Missing.Guard(term) =>
          C.at(term) {
            C.error(pp"Non-exhaustive pattern matching, guard could be false which is not covered")
          }
        case Missing.Literal(value, tpe, at) =>
          val missingCase = traceToCase(at, pp"${value}")

          C.error(pp"Non-exhaustive pattern matching, missing case $missingCase")
        case Missing.Default(tpe, at) =>
          val missingCase = traceToCase(at, "_")

          C.error(pp"Non-exhaustive pattern matching, scrutinees of type ${tpe} require a default case ${missingCase}")
      }
    }

    def reportRedundant()(using C: ErrorReporter): Unit = redundant.foreach { p =>
      C.at(p) { C.warning(pp"Unreachable case.") }
    }

    def report()(using C: ErrorReporter): Unit = {
      reportNonExhaustive()
      reportRedundant()
    }
  }

  def checkExhaustive(scrutinees: List[source.Term], cls: List[source.MatchClause])(using C: Context): Unit = {
    // each source clause may expand into multiple internal clauses due to the use of OR patterns
    val initialClauses: List[Clause] = cls.flatMap(preprocess(scrutinees, _))
    given E: Exhaustivity = new Exhaustivity(cls, scrutinees)
    checkScrutinees(scrutinees.map(Trace.Root(_)), scrutinees.map{ scrutinee => Context.inferredTypeOf(scrutinee) }, initialClauses)
    E.report()
  }

  def checkScrutinees(scrutinees: List[Trace], tpes: List[ValueType], clauses: List[Clause])(using E: Exhaustivity): Unit = {
    (scrutinees, tpes) match {
      case (List(scrutinee), List(tpe)) => checkScrutinee(scrutinee, tpe, clauses)
      case _ =>
        clauses match {
          case Nil => E.missingDefault(tpes.head, scrutinees.head)
          case head :: tail => matchClauses(head, tail)
        }
    }
  }

  def checkScrutinee(scrutinee: Trace, tpe: ValueType, clauses: List[Clause])(using E: Exhaustivity): Unit = {

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
        case Nil => E.missingConstructor(c, scrutinee)
        case cl :: rest => matchClauses(cl, rest)
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
              case Some(head :: tail) => matchClauses(head, tail)
              case _ => defaults.toList match {
                case Nil => E.missingLiteral(v, tpe, scrutinee)
                case head :: tail => matchClauses(head, tail)
              }
            }
          }

        // clauses can never exhaust the domain (Int, String, etc.)
        case None =>
          matches.collect {
            case (_, head :: tail) => matchClauses(head, tail)
          }
          defaults.toList match {
            case Nil => E.missingDefault(tpe, scrutinee)
            case head :: tail => matchClauses(head, tail)
          }
      }
    }

    tpe match {
      case ValueType.ValueTypeApp(DataType(_, _, ctors, _), _) => checkDatatype(ctors)
      case ValueType.ValueTypeApp(Record(_, _, ctor, _), _)    => checkDatatype(List(ctor))
      case tpe @ (builtins.TInt | builtins.TDouble | builtins.TString | builtins.TBoolean | builtins.TUnit | builtins.TBottom | builtins.TChar | builtins.TByte) =>
        checkLiteral(tpe)

      case tpe =>
        clauses match {
          case Nil => E.missingDefault(tpe, scrutinee)
          // ignore scrutinee
          case head :: tail =>
            matchClauses(head, tail)
        }
    }
  }

  def matchClauses(head: Clause, alternatives: List[Clause])(using E: Exhaustivity): Unit = head match {
    // the first clause already matches, mark it as used
    case Clause(Nil, tree) => E.use(tree)

    // we are not yet done and need to continue matching
    case Clause(Condition.Guard(term) :: rest, body) =>
      // case: guard is true
      matchClauses(Clause.normalized(rest, body), alternatives)
      // case: guard is false
      alternatives match {
        case Nil => E.missingGuard(term)
        case head :: next => matchClauses(head, next)
      }
    case Clause(Condition.Patterns(patterns) :: rest, body) if patterns.isEmpty =>
      matchClauses(Clause.normalized(rest, body), alternatives)
    case Clause(Condition.Patterns(patterns) :: rest, body) =>
      // We have to choose the next scrutinee. We guess the approximate type by looking at the pattern it is
      // matched against.
      patterns.head match {
        case (sc, Pattern.Tag(constructor, ps)) =>
          checkScrutinee(sc, constructor.appliedDatatype, head :: alternatives)
        case (sc, Pattern.Literal(lit, tpe)) =>
          checkScrutinee(sc, tpe, head :: alternatives)
        case (sc, Pattern.Any()) =>
          INTERNAL_ERROR("Should not happen, since clauses are normalized.")
      }
  }
}