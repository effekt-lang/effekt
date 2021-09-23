package effekt

import effekt.context.Context
import effekt.source.{ Tree }
import effekt.symbols.{ BlockTypeApp, BlockType, BoxedType, CaptureSet, CaptureUnificationVar, Capture, CaptureParam, CaptureOf, FunctionType, InterfaceType, Type, TypeVar, UnificationVar, ValueType, ValueTypeApp, Interface }
import effekt.symbols.builtins.THole
import effekt.util.messages.ErrorReporter

object substitutions {

  sealed trait TypeConstraint

  // TODO add some metadata to this constraint: aka. provenance of the types, source position of the constraint, expected / got?
  case class Eq(tpe1: ValueType, tpe2: ValueType, position: Tree) extends TypeConstraint {
    override def toString = s"$tpe1 =:= $tpe2"
  }
  case class EqBlock(tpe1: BlockType, tpe2: BlockType, position: Tree) extends TypeConstraint {
    override def toString = s"$tpe1 =:= $tpe2"
  }

  sealed trait CaptureConstraint
  case class Sub(capt1: CaptureSet, capt2: CaptureSet, position: Tree) extends CaptureConstraint {
    override def toString = s"$capt1 <:< $capt2"
  }
  case class EqCapt(capt1: CaptureSet, capt2: CaptureSet, position: Tree) extends CaptureConstraint {
    override def toString = s"$capt1 =:= $capt2"
  }

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

    def freshCaptVar(underlying: Capture): CaptureUnificationVar = {
      val x = CaptureUnificationVar(underlying, this)
      capture_skolems = x :: capture_skolems
      x
    }

    /**
     * These are the unification variables introduced in the current scope
     */
    private var skolems: List[UnificationVar] = Nil
    private var capture_skolems: List[CaptureUnificationVar] = Nil

    /**
     * These are the constraints introduced in the current scope
     */
    private var constraints: List[TypeConstraint] = Nil
    private var capture_constraints: List[CaptureConstraint] = Nil

    def requireEqual(t1: ValueType, t2: ValueType)(implicit C: Context) = constraints = Eq(t1, t2, C.focus) :: constraints

    def requireEqual(t1: BlockType, t2: BlockType)(implicit C: Context) = constraints = EqBlock(t1, t2, C.focus) :: constraints

    def requireEqual(capt1: CaptureSet, capt2: CaptureSet)(implicit C: Context) = capture_constraints = EqCapt(capt1, capt2, C.focus) :: capture_constraints

    def requireSub(capt1: CaptureSet, capt2: CaptureSet)(implicit C: Context) = capture_constraints = Sub(capt1, capt2, C.focus) :: capture_constraints

    def addAllType(cs: List[TypeConstraint]): Unit = constraints = constraints ++ cs
    def addAllCapt(cs: List[CaptureConstraint]): Unit = capture_constraints = capture_constraints ++ cs

    def instantiate(tpe: FunctionType): (List[UnificationVar], List[CaptureUnificationVar], FunctionType) = {
      val FunctionType(tparams, cparams, vparams, bparams, ret) = tpe
      val typeSubst = tparams.map { p => p -> fresh(p) }.toMap

      val captRigids = cparams map freshCaptVar
      val captSubst = (cparams zip captRigids).map { case (p, r) => p -> CaptureSet(Set(r)) }.toMap

      val typeRigids = typeSubst.values.toList

      val substitutedVparams = vparams map typeSubst.substitute
      val substitutedBparams = bparams map typeSubst.substitute
      val substitutedReturn = captSubst.substitute(typeSubst.substitute(ret))
      /** TODO: do something about capture metavariables on a function type */
      (typeRigids, captRigids, FunctionType(Nil, Nil, substitutedVparams, substitutedBparams, substitutedReturn))
    }

    // TODO factor into sumtype that's easier to test -- also try to get rid of Context -- we only need to for positioning and error reporting
    def solve(implicit C: Context): (Substitutions, Map[Capture, CaptureSet], List[TypeConstraint], List[EqCapt]) = {
      var tcs = constraints
      var ccs = capture_constraints
      var cache: List[TypeConstraint] = Nil

      var residual: List[TypeConstraint] = Nil

      // an experimental set solver
      // this is still experimental since:
      // - we don't guarantee it terminates
      // - it doesn't account for interaction with type constraint solving (while solving types, we might encounter new
      //   set constraints, but NOT vice versa).
      def setsolver(cs: List[EqCapt]): (Map[Capture, CaptureSet], List[EqCapt]) = {
        var unsolved = cs
        var cache: List[EqCapt] = Nil
        var subst: Map[Capture, CaptureSet] = Map.empty
        var residual: List[EqCapt] = Nil

        def pop() = { val c = unsolved.head; unsolved = unsolved.tail; c }
        def push(eq: EqCapt) = unsolved = unsolved :+ eq
        def unify(c1: Set[Capture], c2: Set[Capture], pos: Tree) = {
          val (concrete1, unification1) = c1.partition { c => !c.isInstanceOf[CaptureUnificationVar] }
          val (concrete2, unification2) = c2.partition { c => !c.isInstanceOf[CaptureUnificationVar] }

          if (unification1.isEmpty && unification2.isEmpty && concrete1 != concrete2) {
            println(s"comparing: $concrete1 and $concrete2")
            C.at(pos) { C.error(s"Capture set $concrete1 is not equal to $concrete2") }
          }
          residual = EqCapt(CaptureSet(c1), CaptureSet(c2), pos) :: residual
        }

        def learn(x: CaptureUnificationVar, c: Set[Capture]): Unit = {
          val newSubst = Map[Capture, CaptureSet](x -> CaptureSet(c))
          subst = newSubst ++ subst.view.mapValues(c => newSubst.substitute(c)).toMap
          unsolved = unsolved.map { case EqCapt(c1, c2, pos) => EqCapt(subst.substitute(c1), subst.substitute(c2), pos) }
          residual = residual.map { case EqCapt(c1, c2, pos) => EqCapt(subst.substitute(c1), subst.substitute(c2), pos) }
        }

        while (unsolved.nonEmpty) pop() match {
          // drop empty constraints
          case Components(Nil, Nil, Nil, Nil, pos) => ()

          // {?C} =:= ...
          case Components(List(x), Nil, vars2, con2, pos) =>
            // recursive: ?C =:= ?C union {c, a, b} union ?D
            //              ?C !-> {c, a, b} union ?D
            learn(x, vars2.toSet - x ++ con2.toSet)

          // ... =:= {?C}
          case Components(vars1, con1, List(x), Nil, pos) =>
            learn(x, vars1.toSet - x ++ con1.toSet)

          // {a, b, c} =:= {c, a, b}
          case Components(Nil, con1, Nil, con2, pos) => unify(con1.toSet, con2.toSet, pos)

          // ?C union {a, c, b} =:= ?C union {a, b, c}
          case Components(List(x), con1, List(y), con2, pos) if x == y => unify(con1.toSet, con2.toSet, pos)

          // ?C union {a, b} =:= {a, b, c}
          case Components(List(x), con1, Nil, con2, pos) => learn(x, con2.toSet -- con1.toSet)

          // {a, b, c} =:= ?C union {a, b}
          case Components(Nil, con1, List(x), con2, pos) => learn(x, con1.toSet -- con2.toSet)

          // cannot solve eq right now
          // TODO make sure we terminate by keeping a generation and a seen set
          case eq => push(eq)
        }

        (subst, residual)
      }

      object Components {
        def unapply(eq: EqCapt): Option[(List[CaptureUnificationVar], List[Capture], List[CaptureUnificationVar], List[Capture], Tree)] = eq match {
          case EqCapt(CaptureSet(c1), CaptureSet(c2), pos) =>
            val vars1 = c1.collect { case c: CaptureUnificationVar if c.scope == self => c }
            val con1 = c1 -- vars1
            val vars2 = c2.collect { case c: CaptureUnificationVar if c.scope == self => c }
            val con2 = c2 -- vars2
            Some((vars1.toList, con1.toList, vars2.toList, con2.toList, pos))
        }

      }

      // Notes on Constraints
      // --------------------
      // Set[Constraint], alreadySeen: Set[Constraint], Map[EquiUniVar, (List[CaptureSet], List[CaptureSet])]
      //
      // {exc} <: ?C
      // ?C <: {amb}
      //
      // ?C =:= ?D // this amounts to a substitution
      // ?C <: ?D /\ ?D <: ?C
      //
      // [?D, exc] <: ?C <: [amb]
      // {exc} <: {amb}
      // ?D <: {amb}

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

        def unify(c1: CaptureSet, c2: CaptureSet): Unit = ccs = EqCapt(c1, c2, C.focus) :: ccs

        def abort(msg: String) = C.abort(msg)
        def learn(x: UnificationVar, tpe: ValueType) = {
          // all existing solutions have to be compatible with the new one
          equivalences.solutions(x).foreach { s => push(Eq(tpe, s, C.focus)) }
          equivalences.add(x, tpe)
        }
      }

      def push(c: Eq): Unit = tcs = c :: tcs
      def pop() = { val el = tcs.head; tcs = tcs.tail; el }

      while (tcs.nonEmpty) pop() match {
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
          // if (candidates.size > 1) { C.error(s"Type variable $x cannot be instantiated with both ${candidates.mkString(" and ")}") }
          // multiple candidates should already be constrained to be equal, so we can pick arbitrary
          subst += (x -> candidates.head)
          candidates.head
        })

        def substitutedValueType(tpe: ValueType): ValueType = tpe match {
          case t: UnificationVar if t.scope == self => computeSubstitutionFor(t)
          case ValueTypeApp(c, args) => ValueTypeApp(c, args map substitutedValueType)
          case THole => THole
          case BoxedType(btpe, capt) => BoxedType(substitutedBlockType(btpe), capt)
          case _ => tpe
        }
        def substitutedBlockType(tpe: BlockType): BlockType = tpe match {
          case FunctionType(tparams, cparams, vps, bps, ret) =>
            // tparams are always disjoint from skolems!
            // TODO: is this the right thing for capture parameters?
            FunctionType(tparams, cparams, vps map substitutedValueType, bps map substitutedBlockType, substitutedValueType(ret))
          case BlockTypeApp(c, args) => BlockTypeApp(c, args map substitutedValueType)
          case i: Interface          => i
        }

        skolems.foreach(computeSubstitutionFor)

        subst
      }

      // compute type substitution from equivalence classes
      val typeSubst: Substitutions = computeTypeSubstitution

      // also solve set constraints
      val (captSubst, residualCapts) = setsolver(ccs.collect { case e: EqCapt => e })

      // TODO check that subsumption constraints hold!

      // update type substitution with capture sets
      val updatedTypeSubst = typeSubst.view.mapValues { t => captSubst.substitute(t) }.toMap

      val substitutedResiduals = residual map { t => captSubst.substitute(updatedTypeSubst.substitute(t)) }

      (updatedTypeSubst, captSubst, substitutedResiduals, residualCapts)
    }

    override def toString = s"Scope$id"
  }

  type Substitutions = Map[TypeVar, ValueType]

  // TODO add back, but as inequalities
  trait RegionEq

  // Substitution is independent of the unifier
  implicit class ValueSubstitutionOps(substitutions: Map[TypeVar, ValueType]) {
    def substitute(t: ValueType): ValueType = t match {
      case x: TypeVar =>
        substitutions.getOrElse(x, x)
      case ValueTypeApp(t, args) =>
        ValueTypeApp(t, args.map { substitute })
      case BoxedType(tpe, capt) =>
        BoxedType(substitute(tpe), capt)
      case other => other
    }

    def substitute(t: BlockType): BlockType = t match {
      // TODO for now substitution doesn't do anything on capability types.
      case b: InterfaceType => b
      case b: FunctionType  => substitute(b)
    }

    def substitute(t: FunctionType): FunctionType = t match {
      case FunctionType(tps, cps, vps, bps, ret) =>
        // do not substitute with types parameters bound by this function!
        val substWithout = substitutions.filterNot { case (t, _) => tps.contains(t) }
        // TODO: check capture parameters
        FunctionType(tps, cps, vps map substWithout.substitute, bps map substWithout.substitute, substWithout.substitute(ret))
    }

    def substitute(c: TypeConstraint): TypeConstraint = c match {
      case Eq(t1, t2, pos)      => Eq(substitute(t1), substitute(t2), pos)
      case EqBlock(t1, t2, pos) => EqBlock(substitute(t1), substitute(t2), pos)
    }
  }

  // TODO generalize to a bi-substitution Map[Capture, (CaptureSet, CaptureSet)]
  implicit class CaptureSubstitutionOps(substitutions: Map[Capture, CaptureSet]) {

    def substitute(c: CaptureSet): CaptureSet = CaptureSet(c.captures.flatMap {
      case c => substitutions.getOrElse(c, CaptureSet(Set(c))).captures
    })

    def substitute(t: ValueType): ValueType = t match {
      case x: TypeVar            => x
      case ValueTypeApp(t, args) => ValueTypeApp(t, args.map { substitute })
      case BoxedType(tpe, capt)  => BoxedType(substitute(tpe), substitute(capt))
      case other                 => other
    }

    def substitute(t: BlockType): BlockType = t match {
      // TODO for now substitution doesn't do anything on capability types.
      case b: InterfaceType => b
      case b: FunctionType  => substitute(b)
    }

    def substitute(t: FunctionType): FunctionType = t match {
      case FunctionType(tps, cps, vps, bps, ret) =>
        // do not substitute with capture parameters bound by this function!
        val substWithout = substitutions.filterNot { case (t, _) => cps.contains(t) }
        FunctionType(tps, cps, vps map substitute, bps map substitute, substWithout.substitute(ret))
    }

    def substitute(c: TypeConstraint): TypeConstraint = c match {
      case Eq(t1, t2, pos)      => Eq(substitute(t1), substitute(t2), pos)
      case EqBlock(t1, t2, pos) => EqBlock(substitute(t1), substitute(t2), pos)
    }

    def substitute(c: CaptureConstraint): CaptureConstraint = c match {
      case Sub(c1, c2, pos)    => Sub(substitute(c1), substitute(c2), pos)
      case EqCapt(c1, c2, pos) => EqCapt(substitute(c1), substitute(c2), pos)
    }
  }

  trait TypeComparer {

    // "unification effects"
    def learn(x: UnificationVar, tpe: ValueType): Unit
    def abort(msg: String): Nothing
    def scope: UnificationScope
    def defer(t1: ValueType, t2: ValueType): Unit
    def unify(c1: CaptureSet, c2: CaptureSet): Unit

    def unify(c1: Capture, c2: Capture): Unit = unify(CaptureSet(Set(c1)), CaptureSet(Set(c2)))

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
      case (BoxedType(tpe1, capt1), BoxedType(tpe2, capt2)) =>
        unifyBlockTypes(tpe1, tpe2)
        unify(capt1, capt2)

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
      /** TODO: check if right thing for capture set parameters */
      case (f1 @ FunctionType(tparams1, cparams1, vparams1, bparams1, ret1), f2 @ FunctionType(tparams2, cparams2, vparams2, bparams2, ret2)) =>

        if (tparams1.size != tparams2.size)
          abort(s"Type parameter count does not match $f1 vs. $f2")

        if (vparams1.size != vparams2.size)
          abort(s"Value parameter count does not match $f1 vs. $f2")

        if (bparams1.size != bparams2.size)
          abort(s"Block parameter count does not match $f1 vs. $f2")

        val (trigids2, crigids2, FunctionType(_, _, substVparams2, substBparams2, substRet2)) = scope.instantiate(f2)

        unifyValueTypes(ret1, substRet2)

        (tparams1 zip trigids2) foreach { case (t1, t2) => unifyValueTypes(t2, t1) }
        (cparams1 zip crigids2) foreach { case (t1, t2) => unify(t2, t1) }
        (vparams1 zip substVparams2) foreach { case (t1, t2) => unifyValueTypes(t2, t1) }
        (bparams1 zip substBparams2) foreach { case (t1, t2) => unifyBlockTypes(t2, t1) }
    }
  }

}
