package effekt

import effekt.context.Context
import effekt.symbols.{ FunctionType, InterfaceType, BoxedType, BlockType, UnificationVar, Type, TypeApp, TypeVar, ValueType, CaptureVar, CaptureSet }
import effekt.symbols.builtins.THole
import effekt.util.messages.ErrorReporter

object substitutions {

  sealed trait TypeConstraint

  // TODO add some metadata to this constraint: aka. provenance of the types, source position of the constraint, expected / got?
  case class Eq(tpe1: ValueType, tpe2: ValueType) extends TypeConstraint
  case class EqBlock(tpe1: BlockType, tpe2: BlockType) extends TypeConstraint

  private var scopeId: Int = 0
  /**
   * A substitution scope -- every fresh unification variable is associated with a scope.
   */
  class UnificationScope { self =>

    val id = { scopeId += 1; scopeId }

    def fresh(underlying: TypeVar): UnificationVar = {
      val x = UnificationVar(underlying, this)
      skolems = x :: skolems
      x
    }

    var skolems: List[UnificationVar] = Nil

    var constraints: List[TypeConstraint] = Nil

    def requireEqual(t1: ValueType, t2: ValueType): Unit = constraints = Eq(t1, t2) :: constraints

    def requireEqual(t1: BlockType, t2: BlockType) = constraints = EqBlock(t1, t2) :: constraints

    def addAll(cs: List[TypeConstraint]): Unit = constraints = constraints ++ cs

    // TODO factor into sumtype that's easier to test -- also try to get rid of Context -- we only need to for positioning and error reporting
    def solve(implicit C: ErrorReporter): (Substitutions, List[TypeConstraint]) = {
      var cs = constraints
      var cache: List[TypeConstraint] = Nil
      var typeSubst: Substitutions = Map.empty

      var residual: List[TypeConstraint] = Nil

      object comparer extends TypeComparer {
        def scope = self
        def defer(t1: ValueType, t2: ValueType): Unit = residual = Eq(t1, t2) :: residual
        def abort(msg: String) = C.abort(msg)
        def learn(x: UnificationVar, tpe: ValueType) = {
          typeSubst.get(x).foreach { v2 => push(Eq(v2, tpe)) }

          // Use new substitution binding to refine right-hand-sides of existing substitutions.
          // Do we need an occurs check?
          val newSubst: Substitutions = Map(x -> tpe)
          val improvedSubst: Substitutions = typeSubst.map { case (rigid, tpe) => (rigid, newSubst substitute tpe) }
          typeSubst = improvedSubst + (x -> improvedSubst.substitute(tpe))
        }
      }

      def push(c: Eq): Unit = cs = c :: cs
      def pop() = { val el = cs.head; cs = cs.tail; el }

      while (cs.nonEmpty) pop() match {
        case c @ Eq(x, y) if !cache.contains(c) =>
          cache = c :: cache
          comparer.unifyValueTypes(x, y)
        case c @ EqBlock(x, y) if !cache.contains(c) =>
          cache = c :: cache
          comparer.unifyBlockTypes(x, y)
        case _ => ()
      }

      val undefined = skolems filterNot { x => typeSubst.isDefinedAt(x) }

      if (undefined.size > 0) { C.abort(s"Cannot infer type for ${undefined.mkString(" and ")}") }

      val substitutedResiduals = residual map { typeSubst.substitute }

      (typeSubst, substitutedResiduals)
    }

    override def toString = s"Scope$id"
  }

  type Substitutions = Map[TypeVar, ValueType]

  // TODO add back, but as inequalities
  trait RegionEq

  // Substitution is independent of the unifier
  implicit class SubstitutionOps(substitutions: Map[TypeVar, ValueType]) {
    def substitute(t: ValueType): ValueType = t match {
      case x: TypeVar =>
        substitutions.getOrElse(x, x)
      case TypeApp(t, args) =>
        TypeApp(t, args.map { substitute })
      case BoxedType(tpe) =>
        BoxedType(substitute(tpe))
      case other => other
    }

    def substitute(t: BlockType): BlockType = t match {
      // TODO for now substitution doesn't do anything on capability types.
      case b: InterfaceType => b
      case b: FunctionType  => substitute(b)
    }

    def substitute(t: FunctionType): FunctionType = t match {
      case FunctionType(tps, vps, bps, ret) =>
        // do not substitute with types parameters bound by this function!
        val substWithout = substitutions.filterNot { case (t, _) => tps.contains(t) }
        FunctionType(tps, vps map substWithout.substitute, bps map substWithout.substitute, substWithout.substitute(ret))
    }

    def substitute(c: TypeConstraint): TypeConstraint = c match {
      case Eq(t1, t2)      => Eq(substitute(t1), substitute(t2))
      case EqBlock(t1, t2) => EqBlock(substitute(t1), substitute(t2))
    }
  }

  trait TypeComparer {

    // "unification effects"
    def learn(x: UnificationVar, tpe: ValueType): Unit
    def abort(msg: String): Nothing
    def scope: UnificationScope
    def defer(t1: ValueType, t2: ValueType): Unit

    def unifyValueTypes(tpe1: ValueType, tpe2: ValueType): Unit = (tpe1, tpe2) match {
      case (t, s) if t == s => ()
      case (s: UnificationVar, t: ValueType) if s.scope == scope => learn(s, t)

      // occurs for example when checking the first argument of `(1 + 2) == 3` against expected
      // type `?R` (since `==: [R] (R, R) => Boolean`)
      case (s: ValueType, t: UnificationVar) if t.scope == scope => learn(t, s)

      // we defer unification of variables introduced in other scopes
      case (s: UnificationVar, t: ValueType) => defer(s, t)
      case (s: ValueType, t: UnificationVar) => defer(s, t)

      case (TypeApp(t1, args1), TypeApp(t2, args2)) if t1 == t2 =>
        if (args1.size != args2.size)
          abort(s"Argument count does not match $t1 vs. $t2")

        (args1 zip args2) foreach { case (t1, t2) => unifyValueTypes(t1, t2) }

      case (THole, _) | (_, THole) => ()
      case (BoxedType(tpe1), BoxedType(tpe2)) => unifyBlockTypes(tpe1, tpe2)

      case (t, s) => abort(s"Expected ${t}, but got ${s}")
    }

    def unifyBlockTypes(tpe1: BlockType, tpe2: BlockType): Unit = (tpe1, tpe2) match {
      case (t: FunctionType, s: FunctionType) => unifyFunctionTypes(t, s)
      case (t: InterfaceType, s: InterfaceType) => unifyInterfaceTypes(t, s)
      case (t, s) => abort(s"Expected ${t}, but got ${s}")
    }

    def unifyInterfaceTypes(tpe1: InterfaceType, tpe2: InterfaceType): Unit =
      // TODO implement properly
      if (tpe1 != tpe2) abort(s"Expected ${tpe1}, but got ${tpe2}")

    def unifyFunctionTypes(tpe1: FunctionType, tpe2: FunctionType): Unit = (tpe1, tpe2) match {
      case (f1 @ FunctionType(targs1, vargs1, bargs1, ret1), f2 @ FunctionType(targs2, vargs2, bargs2, ret2)) =>

        if (targs1.size != targs2.size)
          abort(s"Type argument count does not match $f1 vs. $f2")

        if (vargs1.size != vargs2.size)
          abort(s"Value argument count does not match $f1 vs. $f2")

        if (bargs1.size != bargs2.size)
          abort(s"Block argument count does not match $f1 vs. $f2")

        val (rigids1, FunctionType(_, substVargs1, substBargs1, substRet1)) = instantiate(f1)
        val (rigids2, FunctionType(_, substVargs2, substBargs2, substRet2)) = instantiate(f2)

        unifyValueTypes(substRet1, substRet2)

        (rigids1 zip rigids2) foreach { case (t1, t2) => unifyValueTypes(t1, t2) }
        (substVargs1 zip substVargs2) foreach { case (t1, t2) => unifyValueTypes(t1, t2) }
        (substBargs1 zip substBargs2) foreach { case (t1, t2) => unifyBlockTypes(t1, t2) }
    }

    def instantiate(tpe: FunctionType): (List[UnificationVar], FunctionType) = {
      val FunctionType(tparams, vparams, bparams, ret) = tpe
      val subst = tparams.map { p => p -> scope.fresh(p) }.toMap
      val rigids = subst.values.toList

      val substitutedVparams = vparams map subst.substitute
      val substitutedBparams = bparams map subst.substitute
      val substitutedReturn = subst.substitute(ret)
      (rigids, FunctionType(Nil, substitutedVparams, substitutedBparams, substitutedReturn))
    }
  }

}
