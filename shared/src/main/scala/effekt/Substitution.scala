package effekt

import effekt.context.Context
import effekt.source.{ Tree }
import effekt.symbols.{ BlockTypeApp, BlockType, BoxedType, CaptureSet, CaptureVar, FunctionType, InterfaceType, Type, TypeVar, UnificationVar, ValueType, ValueTypeApp, Interface }
import effekt.symbols.builtins.THole
import effekt.util.messages.ErrorReporter

object substitutions {

  sealed trait TypeConstraint

  // TODO add some metadata to this constraint: aka. provenance of the types, source position of the constraint, expected / got?
  case class Eq(tpe1: ValueType, tpe2: ValueType, position: Tree) extends TypeConstraint
  case class EqBlock(tpe1: BlockType, tpe2: BlockType, position: Tree) extends TypeConstraint

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

    /**
     * These are the unification variables introduced in the current scope
     */
    private var skolems: List[UnificationVar] = Nil

    /**
     * These are the constraints introduced in the current scope
     */
    private var constraints: List[TypeConstraint] = Nil

    def requireEqual(t1: ValueType, t2: ValueType)(implicit C: Context) = constraints = Eq(t1, t2, C.focus) :: constraints

    def requireEqual(t1: BlockType, t2: BlockType)(implicit C: Context) = constraints = EqBlock(t1, t2, C.focus) :: constraints

    def addAll(cs: List[TypeConstraint]): Unit = constraints = constraints ++ cs

    def instantiate(tpe: FunctionType): (List[UnificationVar], FunctionType) = {
      val FunctionType(tparams, vparams, bparams, ret) = tpe
      val subst = tparams.map { p => p -> fresh(p) }.toMap
      val rigids = subst.values.toList

      val substitutedVparams = vparams map subst.substitute
      val substitutedBparams = bparams map subst.substitute
      val substitutedReturn = subst.substitute(ret)
      (rigids, FunctionType(Nil, substitutedVparams, substitutedBparams, substitutedReturn))
    }

    // TODO factor into sumtype that's easier to test -- also try to get rid of Context -- we only need to for positioning and error reporting
    def solve(implicit C: Context): (Substitutions, List[TypeConstraint]) = {
      var cs = constraints
      var cache: List[TypeConstraint] = Nil

      var residual: List[TypeConstraint] = Nil

      // This implements a simple union-find -- not very efficient
      object equivalences {
        import scala.collection.mutable

        val classes: mutable.Map[UnificationVar, mutable.Set[ValueType]] = mutable.Map.empty
        def find(t: UnificationVar): mutable.Set[ValueType] = classes.getOrElseUpdate(t, mutable.Set(t))
        def union(t1: UnificationVar, t2: UnificationVar): Unit = {
          val s1 = find(t1)
          val s2 = find(t2)

          // they are already the same equivalence class, nothing to do
          if (s1 eq s2) return ;

          val (smaller, larger) = if (s1.size < s2.size) (s1, s2) else (s2, s1)
          larger.addAll(smaller)
          smaller.foreach {
            case t: UnificationVar if t.scope == self => classes.update(t, larger)
            // what to do with "concrete" types?
            case _ => ()
          }
        }
        def add(x: UnificationVar, t: ValueType): Unit = t match {
          case y: UnificationVar if y.scope == self => union(x, y)
          case _ => find(x).add(t)
        }
        def solutions(x: UnificationVar): List[ValueType] = find(x).filter {
          case y: UnificationVar => y.scope != self
          case tpe               => true
        }.toList
      }

      object comparer extends TypeComparer {
        def scope = self
        def defer(t1: ValueType, t2: ValueType): Unit = residual = Eq(t1, t2, C.focus) :: residual
        def abort(msg: String) = C.abort(msg)
        def learn(x: UnificationVar, tpe: ValueType) = {
          // all existing solutions have to be compatible with the new one
          equivalences.solutions(x).foreach { s => push(Eq(tpe, s, C.focus)) }
          equivalences.add(x, tpe)
        }
      }

      def push(c: Eq): Unit = cs = c :: cs
      def pop() = { val el = cs.head; cs = cs.tail; el }

      while (cs.nonEmpty) pop() match {
        case c @ Eq(x, y, pos) if !cache.contains(c) => C.at(pos) {
          cache = c :: cache
          comparer.unifyValueTypes(x, y)
        }
        case c @ EqBlock(x, y, pos) if !cache.contains(c) => C.at(pos) {
          cache = c :: cache
          comparer.unifyBlockTypes(x, y)
        }
        case _ => ()
      }

      // check whether all unification variables have a concrete solution
      val undefined = skolems filter { x => equivalences.solutions(x).isEmpty }

      if (undefined.size > 0) { C.abort(s"Cannot infer type for ${undefined.mkString(" and ")}") }

      def computeTypeSubstitution: Substitutions = {

        var subst: Substitutions = Map.empty

        def computeSubstitutionFor(x: UnificationVar): ValueType = subst.getOrElse(x, {
          val candidates = equivalences.solutions(x).map(substitutedValueType)
          // TODO check and error if multiple candidates
          subst += (x -> candidates.head)
          candidates.head
        })

        def substitutedValueType(tpe: ValueType): ValueType = tpe match {
          case t: UnificationVar if t.scope == self => computeSubstitutionFor(t)
          case ValueTypeApp(c, args) => ValueTypeApp(c, args map substitutedValueType)
          case THole => THole
          case BoxedType(btpe) => BoxedType(substitutedBlockType(btpe))
          case _ => tpe
        }
        def substitutedBlockType(tpe: BlockType): BlockType = tpe match {
          case FunctionType(tparams, vps, bps, ret) =>
            // tparams are always disjoint from skolems!
            FunctionType(tparams, vps map substitutedValueType, bps map substitutedBlockType, substitutedValueType(ret))
          case BlockTypeApp(c, args) => BlockTypeApp(c, args map substitutedValueType)
          case i: Interface          => i
        }

        skolems.foreach(computeSubstitutionFor)

        subst
      }

      // compute type substitution from equivalence classes
      val typeSubst: Substitutions = computeTypeSubstitution

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
      case ValueTypeApp(t, args) =>
        ValueTypeApp(t, args.map { substitute })
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
      case Eq(t1, t2, pos)      => Eq(substitute(t1), substitute(t2), pos)
      case EqBlock(t1, t2, pos) => EqBlock(substitute(t1), substitute(t2), pos)
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

      case (ValueTypeApp(t1, args1), ValueTypeApp(t2, args2)) if t1 == t2 =>
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
      case (f1 @ FunctionType(tparams1, vparams1, bparams1, ret1), f2 @ FunctionType(tparams2, vparams2, bparams2, ret2)) =>

        if (tparams1.size != tparams2.size)
          abort(s"Type parameter count does not match $f1 vs. $f2")

        if (vparams1.size != vparams2.size)
          abort(s"Value parameter count does not match $f1 vs. $f2")

        if (bparams1.size != bparams2.size)
          abort(s"Block parameter count does not match $f1 vs. $f2")

        val (rigids2, FunctionType(_, substVparams2, substBparams2, substRet2)) = scope.instantiate(f2)

        unifyValueTypes(ret1, substRet2)

        (tparams1 zip rigids2) foreach { case (t1, t2) => unifyValueTypes(t2, t1) }
        (vparams1 zip substVparams2) foreach { case (t1, t2) => unifyValueTypes(t2, t1) }
        (bparams1 zip substBparams2) foreach { case (t1, t2) => unifyBlockTypes(t2, t1) }
    }
  }

}
