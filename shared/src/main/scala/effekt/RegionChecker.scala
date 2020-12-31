package effekt
package regions

import effekt.source._
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.subtitutions.RegionEq
import effekt.symbols.{ BlockSymbol, Symbol, ValueSymbol, Effectful }

import effekt.context.assertions.SymbolAssertions

sealed trait Region {

  /**
   * Is this region a concrete region set? That is, will `asRegionSet`
   * be successful without errors?
   */
  def isInstantiated: Boolean

  /**
   * View this region as a concrete region set
   */
  def asRegionSet(implicit C: Context): RegionSet

  /**
   * View this region as a region variable
   */
  def asRegionVar(implicit C: Context): RegionVar

  def isEmpty: Boolean
}

/**
 * A region variable introduced by unification
 *
 * source is the tree at which position this region variable has been introduced
 * we use it mainly for error reporting
 */
class RegionVar(val id: Int, val source: Tree) extends Region {

  private var _region: Option[Region] = None

  def isInstantiated: Boolean = _region.map { r => r.isInstantiated }.getOrElse(false)

  // we approximate emptiness conservatively
  def isEmpty = _region.map { r => r.isEmpty }.getOrElse(false)

  /**
   * Once we know the actual region this variable represents, we
   * can instantiate it (this way we do not need to implement substitution).
   */
  def instantiate(r: Region): Unit = _region = Some(r)

  override def toString = _region match {
    case None    => s"?r${id}"
    case Some(r) => r.toString
  }

  override def equals(a: Any): Boolean = a match {
    case r: RegionVar => id == r.id
    case _            => false
  }
  override def asRegionSet(implicit C: Context): RegionSet =
    _region.map { r => r.asRegionSet } getOrElse {
      C.panic(s"Cannot find region for unification variable ${this}")
    }
  override def asRegionVar(implicit C: Context): RegionVar = this
}

/**
 * Conceptually: A set of region variables
 *
 * { x, y, z, ?r1, ?r2 } = {x, y, z} u ?r1 u ?r2
 */
class RegionSet(val regions: Set[Symbol]) extends Region {

  def isInstantiated = true

  def contains(r: Symbol): Boolean = regions.contains(r)

  def ++(other: RegionSet): RegionSet = new RegionSet(regions ++ other.regions)
  def --(other: RegionSet): RegionSet = new RegionSet(regions -- other.regions)

  override def equals(other: Any): Boolean = other match {
    case r: RegionSet => r.regions == regions
    case _            => false
  }

  def subsetOf(other: RegionSet): Boolean = regions.subsetOf(other.regions)

  def substitute(x: Symbol, r: RegionSet): RegionSet =
    if (contains(x)) { new RegionSet((regions - x) ++ r.regions) } else this

  def isEmpty: Boolean = regions.isEmpty
  def nonEmpty: Boolean = regions.nonEmpty
  def intersect(other: RegionSet): RegionSet = new RegionSet(regions.intersect(other.regions))

  override def toString = s"{${regions.map(_.toString).mkString(", ")}}"

  override def asRegionSet(implicit C: Context): RegionSet = this
  override def asRegionVar(implicit C: Context): RegionVar =
    C.panic(s"Required a region variable, but got: ${this}")
}

object Region {
  // user facing, we use lists
  def apply(regs: List[Symbol]): RegionSet = new RegionSet(regs.toSet)
  def unapply(regs: RegionSet): Option[List[Symbol]] = Some(regs.regions.toList)

  val empty: RegionSet = Region(Nil)
  def apply(s: Symbol): RegionSet = Region(List(s))

  private var lastId = 0

  def fresh(source: Tree): RegionVar = {
    lastId += 1
    new RegionVar(lastId, source)
  }
}

class RegionChecker extends Phase[ModuleDecl, ModuleDecl] {

  def run(input: ModuleDecl)(implicit C: Context): Option[ModuleDecl] = {
    Context.initRegionstate()
    val subst = C.regionUnifier(C.constraints.toList)
    log(s"Got the following constraints: $subst")
    log(C.constraints.toString)
    check(input)
    Some(input)
  }

  def log(msg: String)(implicit C: Context) = () // C.info(msg + s"\n ${C.constraints}") // println(msg)

  // A traversal with the side effect to annotate all functions with their region
  // it returns a _concrete_ region set, all region variables have to be resolved
  // at that point.
  def checkTree(implicit C: Context): PartialFunction[Tree, RegionSet] = {

    case FunDef(id, tparams, params, ret, body) =>
      val sym = id.symbol

      // Since this function might be (mutally) recursive, annotate it with the current region
      // before checking.
      //
      // This is a conservative approximation that can be refined, later, potentially
      // by using region variables and collecting constraints.
      Context.annotateRegions(sym, C.staticRegion)

      // regions of parameters introduced by this function
      val boundRegions: RegionSet = bindRegions(params)

      val selfRegion = Region(sym)
      val bodyRegion = Context.inDynamicRegion(selfRegion) { check(body) }
      val reg = bodyRegion -- boundRegions

      log(s"inferred region for function ${id}: ${reg}")

      // check that the self region (used by resume and variables) does not escape the scope
      val tpe = Context.blockTypeOf(sym)
      val escapes = freeRegionVariables(tpe.ret) intersect selfRegion
      if (escapes.nonEmpty) {
        Context.abort(s"Continuation escaping current scope")
      }

      // safe inferred region on the function symbol
      Context.annotateRegions(id.symbol, reg)
      reg

    case l @ Lambda(id, params, body) =>
      val sym = l.symbol
      // annotated by typer
      val myRegion = Context.regionOf(sym).asRegionVar
      val boundRegions: RegionSet = bindRegions(params)

      val selfRegion = Region(sym)
      val bodyRegion = Context.inDynamicRegion(selfRegion) { check(body) }

      val inferredReg = bodyRegion -- boundRegions

      // check that myRegion >: inferredReg
      val reg = Context.constrainedRegion(myRegion) match {
        case Some(allowed) =>
          if (!inferredReg.subsetOf(allowed)) {
            Context.abort(s"Region not allowed here: ${inferredReg}")
          }
          allowed
        case None => inferredReg
      }

      // check that the self region does not escape as part of the lambdas type
      val tpe = Context.blockTypeOf(sym)
      val escapes = freeRegionVariables(tpe.ret) intersect selfRegion
      if (escapes.nonEmpty) {
        // TODO better error messages
        Context.abort(s"Continuation escaping from lambda")
      }

      // safe inferred region on the function symbol
      Context.annotateRegions(sym, reg)
      Context.instantiate(myRegion, reg)
      reg

    case TryHandle(body, handlers) => {

      // regions for all the capabilities
      val caps = handlers.flatMap { h => h.capability }
      val boundRegions = bindRegions(caps)

      val bodyRegion = Context.inRegion(boundRegions) { check(body) }

      var reg = bodyRegion -- boundRegions

      // check that boundRegions do not escape as part of an inferred type
      val Effectful(tpe, _) = C.inferredTypeOf(body)

      val escapes = freeRegionVariables(tpe) intersect boundRegions
      if (escapes.nonEmpty) {
        Context.abort(s"The following regions leave their defining scope ${escapes}")
      }

      handlers.foreach {
        case Handler(id, cap, clauses) => clauses.foreach {
          case OpClause(id, params, body, resumeId) =>
            val resumeSym = resumeId.symbol
            val resumeReg = Context.dynamicRegion
            Context.annotateRegions(resumeSym, resumeReg)
            reg ++= check(body)
        }
      }
      reg
    }

    // capability call
    case MemberTarget(cap, op) =>
      Context.regionOf(cap.symbol).asRegionSet

    case tgt @ IdTarget(id) => id.symbol match {
      case b: BlockSymbol =>
        Context.regionOf(b).asRegionSet
      case t: ValueSymbol =>
        val symbols.FunType(tpe, reg) = Context.valueTypeOf(t)
        reg.asRegionSet
    }

    case VarDef(id, _, binding) =>
      Context.annotateRegions(id.symbol, Context.dynamicRegion)
      val reg = check(binding)
      // associate the mutable variable binding with the current scope
      reg

    case Var(id) if id.symbol.isInstanceOf[symbols.VarBinder] =>
      Context.regionOf(id.symbol).asRegionSet

    case Assign(id, expr) =>
      check(expr) ++ Context.regionOf(id.symbol).asRegionSet

    // TODO implement
    case ExprTarget(e) =>
      Context.staticRegion // should be annotated in the type!

    // TODO eventually we want to change the representation of block types to admit region polymorphism
    // everywhere where blocktypes are allowed. For now, we only support region polymorphism on known functions.
    // This restriction is fine, since we do only allow second-order blocks anyway.

    // calls to known functions
    case c @ Call(id: IdTarget, _, args) if id.definition.isInstanceOf[symbols.Fun] =>
      var reg = check(id)
      val fun = id.definition.asFun
      val Effectful(tpe, _) = Context.inferredTypeOf(c)

      (fun.params zip args).foreach {
        case (param, arg: ValueArgs) => reg ++= check(args)
        case (List(param: symbols.BlockParam), arg: BlockArg) =>
          val argReg = check(arg)
          reg ++= argReg

          // here we substitute the inferred region for the block parameter in the return type.
          substitute(param, argReg, tpe)
        case (param, arg: CapabilityArg) => ()
      }
      reg

    // calls to unknown functions (block arguments, lambdas, etc.)
    case Call(target, _, args) =>
      args.foldLeft(check(target)) { case (reg, arg) => reg ++ check(arg) }

    case e: ExternFun =>
      Context.annotateRegions(e.symbol, Region.empty)
      Region.empty
  }

  def bindRegions(params: List[ParamSection])(implicit C: Context): RegionSet = {
    var regs: RegionSet = Region.empty
    params.foreach {
      case b: BlockParam =>
        val sym = b.symbol
        val reg = Region(sym)
        Context.annotateRegions(sym, reg)
        regs ++= reg
      case b: CapabilityParam =>
        val sym = b.symbol
        val reg = Region(sym)
        Context.annotateRegions(sym, reg)
        regs ++= reg
      case v: ValueParams => ()
    }
    regs
  }

  def check(obj: Any)(implicit C: Context): RegionSet = obj match {
    case t: Tree if checkTree.isDefinedAt(t) =>
      C.at(t) { checkTree(C)(t) }
    case t: Iterable[t] =>
      t.foldLeft(Region.empty) { case (r, t) => r ++ check(t) }
    case p: Product =>
      p.productIterator.foldLeft(Region.empty) { case (r, t) => r ++ check(t) }
    case leaf =>
      Region.empty
  }

  /**
   * A generic traversal to collects all free region variables
   */
  def freeRegionVariables(o: Any)(implicit C: Context): RegionSet = o match {
    case symbols.FunType(tpe, reg) =>
      freeRegionVariables(tpe) ++ reg.asRegionSet
    case t: Iterable[t] =>
      t.foldLeft(Region.empty) { case (r, t) => r ++ freeRegionVariables(t) }
    case p: Product =>
      p.productIterator.foldLeft(Region.empty) { case (r, t) => r ++ freeRegionVariables(t) }
    case _ =>
      Region.empty
  }

  def substitute(x: Symbol, r: RegionSet, o: Any)(implicit C: Context): Unit = o match {
    case y: RegionVar =>
      val reg = y.asRegionSet.substitute(x, r)

      //      val reg = if (y.isInstantiated) {
      //        y.asRegionSet.substitute(x, r)
      //      } else {
      //        C.constrainedRegion(y)
      //          .getOrElse { C.panic(s"Region should be instantiated ${y}") }
      //          .substitute(x, r)
      //      }
      // overwrite instantiation with substituted set
      y.instantiate(reg)
    case t: Iterable[t] =>
      t.foreach { t => substitute(x, r, t) }
    case p: Product =>
      p.productIterator.foreach { t => substitute(x, r, t) }
    case _ =>
      ()
  }

}

trait RegionCheckerOps extends ContextOps { self: Context =>

  // the current lexical region
  private[regions] var staticRegion: RegionSet = Region.empty

  // the current dynamical region (as approximated by the owner handler / lambda / function symbol )
  // only used for continuations!
  private[regions] var dynamicRegion: RegionSet = Region.empty

  private[regions] var constraints: List[RegionEq] = Nil

  def initRegionstate(): Unit = {
    staticRegion = Region.empty
    constraints = annotation(Annotations.Unifier, module).constraints
  }

  def inRegion[T](r: RegionSet)(block: => T): T = {
    val staticBefore = staticRegion
    val dynamicBefore = dynamicRegion
    staticRegion = r
    dynamicRegion = r
    val res = block
    staticRegion = staticBefore
    dynamicRegion = dynamicBefore
    res
  }

  def inDynamicRegion[T](r: RegionSet)(block: => T): T = {
    val dynamicBefore = dynamicRegion
    dynamicRegion = r
    val res = block
    dynamicRegion = dynamicBefore
    res
  }

  def instantiate(x: RegionVar, r: RegionSet): Unit = {
    x.instantiate(r)
    simplifyConstraints()
  }

  def constrainedRegion(r: RegionVar): Option[RegionSet] =
    regionUnifier(constraints.toList).get(r) match {
      case Some(r: RegionVar) if r.isInstantiated => Some(r.asRegionSet)
      case Some(r: RegionSet) => Some(r)
      case _ => None
    }

  /**
   * Simplifies constraints and replaces the constraint set with the simplified one
   */
  def simplifyConstraints(): Unit =
    constraints = simplifyConstraints(constraints)

  /**
   * Simplify the constraints and replace region variables by their region set if
   * instantiated.
   */
  def simplifyConstraints(cs: List[RegionEq]): List[RegionEq] = cs match {

    case RegionEq(x: RegionVar, y: RegionVar) :: rest if x.isInstantiated && y.isInstantiated =>
      simplifyConstraints(RegionEq(x.asRegionSet, y.asRegionSet) :: rest)

    case RegionEq(r1: Region, r2: Region) :: rest if r1 == r2 =>
      simplifyConstraints(rest)

    case (c @ RegionEq(x: RegionVar, r)) :: rest if !x.isInstantiated =>
      c :: simplifyConstraints(substConstraints(x, r, rest))

    case (c @ RegionEq(r, x: RegionVar)) :: rest if !x.isInstantiated =>
      c :: simplifyConstraints(substConstraints(x, r, rest))

    case RegionEq(r1: RegionSet, r2: RegionSet) :: rest if (r1 != r2) =>
      abort(s"Region mismatch: $r1 is not equal to $r2")

    case RegionEq(r1: RegionSet, r2: RegionSet) :: rest =>
      simplifyConstraints(rest)

    case Nil => Nil
  }

  def regionUnifier(cs: List[RegionEq]): Map[RegionVar, Region] =
    simplifyConstraints(cs).map {
      case RegionEq(x: RegionVar, r) if !x.isInstantiated => (x -> r)
      case RegionEq(r, x: RegionVar) if !x.isInstantiated => (x -> r)
      case _ => panic("wrong constraint")
    }.toMap

  private def substConstraints(x: RegionVar, r: Region, cs: List[RegionEq]): List[RegionEq] =
    cs.map {
      case RegionEq(r1, r2) => RegionEq(if (r1 == x) r else r1, if (r2 == x) r else r2)
    }
}
