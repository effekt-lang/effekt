package effekt
package core

import effekt.context.Context
import effekt.core.substitutions.Substitution
import effekt.symbols.TmpValue
import effekt.util.messages.ErrorReporter

// Pattern Matching Compiler
// -------------------------
// The implementation of the match compiler follows closely the short paper:
//   Jules Jacobs
//   How to compile pattern matching
//   https://julesjacobs.com/notes/patternmatching/patternmatching.pdf
//
// There also is a more advanced Rust implementation that we could look at:
//   https://gitlab.com/yorickpeterse/pattern-matching-in-rust/-/tree/main/jacobs2021
object PatternMatchingCompiler {

  /**
   * The conditions need to be met in sequence before the block at [[label]] can be evaluated with given [[args]].
   */
  case class Clause(conditions: List[Condition], label: BlockVar, args: List[ValueVar])

  enum Condition {
    // all of the patterns need to match for this condition to be met
    case Patterns(patterns: Map[ValueVar, Pattern])
    // a boolean predicate that needs to be branched on at runtime
    case Predicate(pred: Pure)
    // a predicate trivially met by running and binding the statement
    case Val(x: Id, binding: Stmt)
    case Let(x: Id, binding: Expr)
  }

  enum Pattern {
    // sub-patterns are annotated with the inferred type of the scrutinee at this point
    // i.e. Cons(Some(x : TInt): Option[Int], xs: List[Option[Int]])
    case Tag(id: Id, patterns: List[(Pattern, ValueType)])
    case Ignore()
    case Any(id: Id)
    //      case OrPattern(patterns: List[Pattern])
    //      case LiteralPattern()
  }

  /**
   * The match compiler works with
   * - a sequence of clauses that represent alternatives (disjunction)
   * - each sequence contains a list of patterns that all have to match (conjunction).
   */
  def compile(clauses: List[Clause])(using ErrorReporter): core.Stmt = {
    // matching on void will result in this case
    if (clauses.isEmpty) return core.Hole()

    val normalizedClauses = clauses.map(normalize)

    def jumpToBranch(target: BlockVar, vargs: List[ValueVar]) =
      core.App(target, Nil, vargs, Nil)

    val Clause(conditions, target, args) = normalizedClauses.head

    // (1) Check the first clause to be matched
    val Condition.Patterns(remainingPatterns) :: rest = conditions match {
      // The top-most clause already matches successfully
      case Nil =>
        return jumpToBranch(target, args)
      // We need to perform a computation
      case Condition.Val(x, binding) :: rest =>
        return core.Val(x, binding, compile(Clause(rest, target, args) :: normalizedClauses.tail))
      // We need to perform a computation
      case Condition.Let(x, binding) :: rest =>
        return core.Let(x, binding, compile(Clause(rest, target, args) :: normalizedClauses.tail))
      // We need to check a predicate
      case Condition.Predicate(pred) :: rest =>
        return core.If(pred,
          compile(Clause(rest, target, args) :: normalizedClauses.tail),
          compile(normalizedClauses.tail)
        )
      case p :: rest => p :: rest
    } : @unchecked


    // (2) Choose the variable to split on (we use it implicitly in the following)
    val splitVar: ValueVar = branchingHeuristic(remainingPatterns)

    object Split {
      def unapply(c: List[Condition]): Option[(Pattern.Tag, Map[ValueVar, Pattern], List[Condition])] = c match {
        case Condition.Patterns(patterns) :: rest => patterns.get(splitVar).collect {
          case p: Pattern.Tag => (p, patterns - splitVar, rest)
        }
        case _ => None
      }
    }

    def matchesOn(c: Clause): Option[Pattern.Tag] = c match {
      case Clause(Split(p, _, _), _, _) => Some(p)
      case _ => None
    }

    // (3) separate clauses into those that require a pattern match and those that require a predicate or other things

    // collect all variants that are mentioned in the clauses
    def variants: List[Id] = normalizedClauses.flatMap(matchesOn).map(_.id).distinct

    val clausesFor = collection.mutable.Map.empty[Id, List[Clause]]
    def addClause(c: Id, cl: Clause): Unit =
      val clauses = clausesFor.getOrElse(c, List.empty)
      clausesFor.update(c, clauses :+ cl)

    var defaults = List.empty[Clause]
    def addDefault(cl: Clause): Unit =
      defaults = defaults :+ cl

    // used to make up new scrutinees
    var varsFor: Map[Id, List[ValueVar]] = Map.empty
    def fieldVarsFor(constructor: Id, fieldTypes: List[ValueType]): List[ValueVar] =
      varsFor.getOrElse(constructor, {
        val newVars: List[ValueVar] = fieldTypes.map { tpe => ValueVar(TmpValue(), tpe) }
        varsFor = varsFor.updated(constructor, newVars)
        newVars
      })

    normalizedClauses.foreach {
      case Clause(Split(Pattern.Tag(constructor, patternsAndTypes), restPatterns, restConds), label, args) =>
        val fieldVars = fieldVarsFor(constructor, patternsAndTypes.map(_._2))
        val nestedMatches = fieldVars.zip(patternsAndTypes.map(_._1))
        addClause(constructor, Clause(Condition.Patterns(restPatterns ++ nestedMatches) :: restConds, label, args))

      case c =>
        // Clauses that don't match on that var are duplicated.
        // So we want to choose our branching heuristic to minimize this
        addDefault(c)
        // THIS ONE IS NOT LINEAR
        variants.foreach { v => addClause(v, c) }
    }


    // (4) assemble syntax tree for the pattern match
    val branches = variants.map { v =>
      val body = compile(clausesFor.getOrElse(v, Nil))
      val params = varsFor(v).map { case ValueVar(id, tpe) => core.ValueParam(id, tpe): core.ValueParam }
      val blockLit: BlockLit = BlockLit(Nil, Nil, params, Nil, body)
      (v, blockLit)
    }

    val default = if defaults.isEmpty then None else Some(compile(defaults))
    core.Match(splitVar, branches, default)
  }


  /**
   * TODO implement
   */
  def branchingHeuristic(patterns: Map[ValueVar, Pattern]): ValueVar = patterns.head._1

  /**
   * Substitutes AnyPattern and removes wildcards.
   */
  def normalize(clause: Clause): Clause = clause match {
    case Clause(conditions, label, args) =>
      val (normalized, substitution) = normalize(Map.empty, conditions, Map.empty)
      Clause(normalized, label, args.map(v => substitution.getOrElse(v.id, v)))
  }


  /**
   * 1) merges all subsequent Pattern(ps) :: Pattern(qs) :: ... into Pattern(ps ::: qs) :: ...
   * 2) drops all Ignore and Any patterns and substitutes them in the remaining patterns
   * 3) returns the substitution that needs to be applied to the body of the clause
   *
   * case a is x; x is Cons => f(a, x)
   *   ~=
   * case a is Cons => f(a, a)
   *
   * TODO in the future we could "pull" pattern matches across bindings that are not free in scrutinees.
   *   this way, we would potentially exhaust more matches before performing computation
   */
  def normalize(
    patterns: Map[ValueVar, Pattern],
    conditions: List[Condition],
    substitution: Map[Id, ValueVar]
  ): (List[Condition], Map[Id, ValueVar]) = {

    val subst = Substitution(Map.empty, Map.empty, substitution, Map.empty)

    def prefix(p: Map[ValueVar, Pattern], cs: List[Condition]): List[Condition] =
      if p.isEmpty then cs else Condition.Patterns(p) :: cs

    def substitute(pattern: (ValueVar, Pattern)) = pattern match {
      case (sc, p) => substitution.getOrElse(sc.id, sc) -> p
    }

    conditions match {
      case Condition.Patterns(other) :: rest =>
        val substituted = other.map(substitute)
        val additionalSubst = substituted.collect { case (sc, Pattern.Any(id)) => id -> sc }
        val filtered = substituted.collect {
          case (sc, p: Pattern.Tag) => sc -> p
        }
        normalize(patterns ++ filtered, rest, substitution ++ additionalSubst)

      case Condition.Val(x, binding) :: rest =>
        val substitutedBinding = core.substitutions.substitute(binding)(using subst)
        val (resCond, resSubst) = normalize(Map.empty, rest, substitution)
        (prefix(patterns, Condition.Val(x, substitutedBinding) :: resCond), resSubst)

      case Condition.Let(x, binding) :: rest =>
        val substitutedBinding = core.substitutions.substitute(binding)(using subst)
        val (resCond, resSubst) = normalize(Map.empty, rest, substitution)
        (prefix(patterns, Condition.Let(x, substitutedBinding) :: resCond), resSubst)

      case Condition.Predicate(p) :: rest =>
        val substitutedPredicate = core.substitutions.substitute(p)(using subst)
        val (resCond, resSubst) = normalize(Map.empty, rest, substitution)
        (prefix(patterns, Condition.Predicate(substitutedPredicate) :: resCond), resSubst)

      case Nil =>
        (prefix(patterns, Nil), substitution)
    }
  }

  // For development and debugging
  // -----------------------------

  def show(cl: Clause): String = cl match {
    case Clause(conditions, label, args) =>
      s"case ${conditions.map(show).mkString("; ")} => ${util.show(label.id)}${args.map(x => util.show(x)).mkString("(", ", ", ")")}"
  }

  def show(c: Condition): String = c match {
    case Condition.Patterns(patterns) => patterns.map { case (v, p) => s"${util.show(v)} is ${show(p)}" }.mkString(", ")
    case Condition.Predicate(pred) => util.show(pred) + "?"
    case Condition.Val(x, binding) => s"val ${util.show(x)} = ${util.show(binding)}"
    case Condition.Let(x, binding) => s"let ${util.show(x)} = ${util.show(binding)}"
  }

  def show(p: Pattern): String = p match {
    case Pattern.Tag(id, patterns) => util.show(id) + patterns.map { case (p, tpe) => show(p) }.mkString("(", ", ", ")")
    case Pattern.Ignore() => "_"
    case Pattern.Any(id) => util.show(id)
    //      case source.OrPattern(patterns) => patterns.map(show).mkString(", ")
    //      case source.LiteralPattern(lit, show) => lit.value.toString
  }
}
