package effekt
package core

import effekt.core.substitutions.Substitution

import scala.collection.mutable


/**
 * Pattern Matching Compiler
 * -------------------------
 * The implementation of the match compiler follows closely the short paper:
 *
 *   Jules Jacobs
 *   How to compile pattern matching
 *   https://julesjacobs.com/notes/patternmatching/patternmatching.pdf
 &
 * A match is represented as a list of [[ Clause ]]s, e.g.
 *
 *    case a is Some(x) => j1(x)
 *    case a is None    => j2()
 *
 * Each clause represents one disjunct. That is, one of the clauses needs to match. We compile them
 * to match in order (from top-to-bottom).
 *
 * We generalize Jacob's original draft, by also supporting literal patterns, multiple guards,
 * and evaluating expressions.
 *
 * That is, one clause is the conjunction of multiple [[ Condition ]]s, e.g.:
 *
 *    case a is Some(x); val tmp = comp(x); pred(tmp)? => j(x, tmp)
 *
 * Here, we compile conditions to be evaluated left-to-right.
 *
 * In general, [[ core.Transformer.preprocess ]] takes a source pattern match and translates
 * it into an internal representation (defined below, [[ Clause ]], [[ Condition ]], etc.).
 * This internal representation is designed to have all information readily available to
 * immediately generate the [[ core.Stmt ]] corresponding to the compiled pattern.
 *
 * This way, we can also write [[ core.PatternMatchingTests ]] independent of a concrete source
 * pattern.
 *
 * TODO test matching on the same scrutinee multiple times in a row Maps are the wrong datatype to represent disjunctions
 *
 * While exhaustivity checking could be integrated in the compiler, we decided to have a separate implementation
 * in [[ effekt.typer.ExhaustivityChecker ]], which is run during the [[ effekt.typer.Wellformedness ]] phase.
 *
 * Changes here need to be synchronized with the checker, sadly.
 *
 * @see https://github.com/effekt-lang/effekt/issues/383
 */
object PatternMatchingCompiler {

  /**
   * The conditions need to be met in sequence before the block at [[label]] can be evaluated with given [[targs]] and [[args]].
   */
  case class Clause(conditions: List[Condition], label: BlockVar, targs: List[ValueType], args: List[ValueVar])

  enum Condition {
    // all of the patterns need to match for this condition to be met
    case Patterns(patterns: Map[ValueVar, Pattern])
    // a boolean predicate that needs to be branched on at runtime
    case Predicate(pred: Pure)
    // a predicate trivially met by running and binding the statement
    case Val(x: Id, tpe: core.ValueType, binding: Stmt)
    case Let(x: Id, tpe: core.ValueType, binding: Expr)
  }

  enum Pattern {
    // The pattern matching compiler requires some information:
    // - type of the scrutinee on subpatterns,
    //   i.e. Cons(Some(x : TInt): Option[Int], xs: List[Option[Int]])
    // - the variants of the data type
    //   i.e. Tag("Red", ..., List("Red", "Green", "Blue"), ...)
    case Tag(id: Id, tparams: List[Id], variants: List[Id], patterns: List[(Pattern, ValueType)])
    case Ignore()
    case Any(id: Id)
    case Or(patterns: List[Pattern])
    case Literal(l: core.Literal, equals: (core.Pure, core.Pure) => core.Pure)
  }

  /**
   * The match compiler works with
   * - a sequence of clauses that represent alternatives (disjunction)
   * - each sequence contains a list of conditions that all have to match (conjunction).
   */
  def compile(clauses: List[Clause]): core.Stmt = {
    // This shouldn't be reachable anymore since we specialize matching on void before calling compile
    if (clauses.isEmpty) return core.Hole()

    // (0) normalize clauses
    val normalized @ (headClause :: remainingClauses) = clauses.map(normalize) : @unchecked

    // (1) Check the first clause to be matched (we can immediately handle non-pattern cases)
    val patterns = headClause match {
      // - The top-most clause already matches successfully
      case Clause(Nil, target, targs, args) =>
        return core.App(target, targs, args, Nil)
      // - We need to perform a computation
      case Clause(Condition.Val(x, tpe, binding) :: rest, target, targs, args) =>
        return core.Val(x, tpe, binding, compile(Clause(rest, target, targs, args) :: remainingClauses))
      // - We need to perform a computation
      case Clause(Condition.Let(x, tpe, binding) :: rest, target, targs, args) =>
        return core.Let(x, tpe, binding, compile(Clause(rest, target, targs, args) :: remainingClauses))
      // - We need to check a predicate
      case Clause(Condition.Predicate(pred) :: rest, target, targs, args) =>
        return core.If(pred,
          compile(Clause(rest, target, targs, args) :: remainingClauses),
          compile(remainingClauses)
        )
      case Clause(Condition.Patterns(patterns) :: rest, target, targs, args) =>
        patterns
    }

    // (2) Choose the variable to split on
    val scrutinee: ValueVar = branchingHeuristic(patterns, normalized)

    object Split {
      def unapply(c: List[Condition]): Option[(Pattern, Map[ValueVar, Pattern], List[Condition])] =
        c match {
          case Condition.Patterns(patterns) :: rest =>
            patterns.get(scrutinee).map { p => (p, patterns - scrutinee, rest) }
          case _ => None
        }
    }

    // (3a) Match on a literal
    def splitOnLiteral(lit: Literal, equals: (Pure, Pure) => Pure): core.Stmt = {
      // the different literal values that we match on
      val variants: List[core.Literal] = normalized.collect {
        case Clause(Split(Pattern.Literal(lit, _), _, _), _, _, _) => lit
      }.distinct

      // for each literal, we collect the clauses that match it correctly
      val clausesFor = mutable.Map.empty[core.Literal, List[Clause]]
      def addClause(c: core.Literal, cl: Clause): Unit =
        clausesFor.update(c, clausesFor.getOrElse(c, List.empty) :+ cl)

      // default clauses always match with respect to the current scrutinee
      var defaults = List.empty[Clause]
      def addDefault(cl: Clause): Unit =
        defaults = defaults :+ cl



      normalized.foreach {
        case Clause(Split(Pattern.Literal(lit, _), restPatterns, restConds), label, targs, args) =>
          addClause(lit, Clause(Condition.Patterns(restPatterns) :: restConds, label, targs, args))

        case c =>
          val finite: Option[List[Any]] = lit.annotatedType match {
            case core.Type.TBoolean => Some(List(true, false))
            case core.Type.TUnit => Some(List(()))
            case core.Type.TBottom => Some(Nil)
            case _ => None
          }

          finite match {
            case Some(values) =>
              if values.exists(b => !variants.map(_.value).contains(b)) then
                addDefault(c)
              variants.foreach { v => addClause(v, c) }
            case None =>
              addDefault(c)
              variants.foreach { v => addClause(v, c) }
          }
      }

      // (4) assemble syntax tree for the pattern match
      variants.foldRight(compile(defaults)) {
        case (lit, elsStmt) =>
          val thnStmt = compile(clausesFor.getOrElse(lit, Nil))
          lit.value match {
            case () => thnStmt
            case true =>
              core.If(scrutinee, thnStmt, elsStmt)
            case false =>
              core.If(scrutinee, elsStmt, thnStmt)
            case _ =>
              core.If(equals(scrutinee, lit), thnStmt, elsStmt)
          }
      }
    }

    // (3b) Match on a data type constructor
    def splitOnTag(id: Id, allVariants: List[Id]) = {
      // collect all variants that are mentioned in the clauses
      val variants: List[Id] = normalized.collect {
        case Clause(Split(p: Pattern.Tag, _, _), _, _, _) => p.id
      }.distinct

      // for each tag, we collect the clauses that match it correctly
      val clausesFor = mutable.Map.empty[Id, List[Clause]]
      def addClause(c: Id, cl: Clause): Unit =
        clausesFor.update(c, clausesFor.getOrElse(c, List.empty) :+ cl)

      // default clauses always match with respect to the current scrutinee
      var defaults = List.empty[Clause]
      def addDefault(cl: Clause): Unit =
        defaults = defaults :+ cl

      // used to make up new scrutinees
      val varsFor = mutable.Map.empty[Id, List[ValueVar]]
      val tvarsFor = mutable.Map.empty[Id, List[Id]]
      def fieldVarsFor(constructor: Id, tparams: List[Id], fieldInfo: List[((Pattern, ValueType), String)]): List[ValueVar] =
        tvarsFor.getOrElseUpdate(constructor, tparams)
        varsFor.getOrElseUpdate(
          constructor,
          fieldInfo.map {
            // if it's a pattern named by the user in the program, use the supplied name
            case ((Pattern.Any(id), tpe),         _) => ValueVar(Id(id.name.name), tpe)
            // otherwise, use the field name of the given field
            case ((_,               tpe), fieldName) => ValueVar(Id(fieldName),    tpe)
          }
        )

      normalized.foreach {
        case Clause(Split(Pattern.Tag(constructor, tparams, variants, patternsAndTypes), restPatterns, restConds), label, targs, args) =>
          // NOTE: Ideally, we would use a `DeclarationContext` here, but we cannot: we're currently in the Source->Core transformer, so we do not have all of the details yet.
          val fieldNames: List[String] = constructor match {
            case c: symbols.Constructor => c.fields.map(_.name.name)
            case _ => List.fill(patternsAndTypes.size) { "y" } // NOTE: Only reached in PatternMatchingTests
          }
          val fieldVars = fieldVarsFor(constructor, tparams, patternsAndTypes.zip(fieldNames))
          val nestedMatches = fieldVars.zip(patternsAndTypes.map { case (pat, tpe) => pat }).toMap
          addClause(constructor,
            // it is important to add nested matches first, since they might include substitutions for the rest.
            Clause(Condition.Patterns(nestedMatches) :: Condition.Patterns(restPatterns) :: restConds, label, targs, args))

        case c =>
          // Clauses that don't match on that var are duplicated.
          // So we want to choose our branching heuristic to minimize this

          // here we duplicate clauses!
          variants.foreach { v => addClause(v, c) }

          // only if the variants do not cover the data type, add a default!
          if allVariants.exists(v => !variants.contains(v)) then
            addDefault(c)
      }

      // (4) assemble syntax tree for the pattern match
      val branches = variants.map { v =>
        val body = compile(clausesFor.getOrElse(v, Nil))
        val tparams = tvarsFor(v)
        val params = varsFor(v).map { case ValueVar(id, tpe) => core.ValueParam(id, tpe): core.ValueParam }
        val blockLit: BlockLit = BlockLit(tparams, Nil, params, Nil, body)
        (v, blockLit)
      }

      val default = if defaults.isEmpty then None else Some(compile(defaults))
      core.Match(scrutinee, branches, default)
    }

    patterns(scrutinee) match {
      case Pattern.Literal(lit, equals) => splitOnLiteral(lit, equals)
      case Pattern.Tag(id, tparams, variants, patterns) => splitOnTag(id, variants)
      case _ => ???
    }
  }

  def branchingHeuristic(patterns: Map[ValueVar, Pattern], clauses: List[Clause]): ValueVar =
    patterns.keys.maxBy(v => clauses.count {
      case Clause(ps, _, _, _) => ps.contains(v)
    })

  /**
   * Substitutes AnyPattern and removes wildcards.
   */
  def normalize(clause: Clause): Clause = clause match {
    case Clause(conditions, label, targs, args) =>
      val (normalized, substitution) = normalize(Map.empty, conditions, Map.empty)
      // TODO also substitute types?
      Clause(normalized, label, targs, args.map(v => substitution.getOrElse(v.id, v)))
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
          case (sc, p: Pattern.Literal) => sc -> p
        }
        normalize(patterns ++ filtered, rest, substitution ++ additionalSubst)

      case Condition.Val(x, tpe, binding) :: rest =>
        val substitutedBinding = core.substitutions.substitute(binding)(using subst)
        val substitutedType = core.substitutions.substitute(tpe)(using subst)
        val (resCond, resSubst) = normalize(Map.empty, rest, substitution)
        val substituted = Condition.Val(x, substitutedType, substitutedBinding)
        (prefix(patterns, substituted :: resCond), resSubst)

      case Condition.Let(x, tpe, binding) :: rest =>
        val substitutedBinding = core.substitutions.substitute(binding)(using subst)
        val (resCond, resSubst) = normalize(Map.empty, rest, substitution)
        (prefix(patterns, Condition.Let(x, tpe, substitutedBinding) :: resCond), resSubst)

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
    case Clause(conditions, label, targs, args) =>
      s"case ${conditions.map(show).mkString("; ")} => ${util.show(label.id)}${targs.map(x => util.show(x))}${args.map(x => util.show(x)).mkString("(", ", ", ")")}"
  }

  def show(c: Condition): String = c match {
    case Condition.Patterns(patterns) => patterns.map { case (v, p) => s"${util.show(v)} is ${show(p)}" }.mkString(", ")
    case Condition.Predicate(pred) => util.show(pred) + "?"
    case Condition.Val(x, tpe,  binding) => s"val ${util.show(x)} = ${util.show(binding)}"
    case Condition.Let(x, tpe, binding) => s"let ${util.show(x)} = ${util.show(binding)}"
  }

  def show(p: Pattern): String = p match {
    case Pattern.Tag(id, tparams, variants, patterns) =>
      util.show(id) + tparams.map(util.show).mkString("[", ",", "]") + patterns.map { case (p, tpe) => show(p) }.mkString("(", ", ", ")")
    case Pattern.Ignore() => "_"
    case Pattern.Any(id) => util.show(id)
    case Pattern.Or(patterns) => patterns.map(show).mkString(" | ")
    case Pattern.Literal(lit, equals) => util.show(lit.value)
  }
}
