package effekt
package regions

import effekt.source._
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.subtitutions.RegionEq
import effekt.symbols.{ BlockSymbol, Symbol, ValueSymbol, Effectful }

sealed trait Region {

  /**
   * Is this region a concrete region set? That is, will `asRegionSet`
   * be successful without errors?
   */
  def isConcrete: Boolean

  /**
   * View this region as a concrete region set
   */
  def asRegionSet(implicit C: Context): RegionSet

  /**
   * View this region as a region variable
   */
  def asRegionVar(implicit C: Context): RegionVar
}

/**
 * A region variable introduced by unification
 *
 * source is the tree at which position this region variable has been introduced
 * we use it mainly for error reporting
 */
class RegionVar(val id: Int, val source: Tree) extends Region {

  private var _region: Option[RegionSet] = None

  def isInstantiated: Boolean = _region.isDefined

  def isConcrete = isInstantiated

  /**
   * Once we know the actual region this variable represents, we
   * can instantiate it (this way we do not need to implement substitution).
   */
  def instantiate(r: RegionSet): Unit = _region = Some(r)

  override def toString = _region match {
    case None    => s"?r${id}"
    case Some(r) => r.toString
  }

  override def equals(a: Any): Boolean = a match {
    case r: RegionVar => id == r.id
    case _            => false
  }
  override def asRegionSet(implicit C: Context): RegionSet =
    _region.getOrElse {
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

  def isConcrete = true

  def contains(r: Symbol): Boolean = regions.contains(r)

  def ++(other: RegionSet): RegionSet = new RegionSet(regions ++ other.regions)
  def --(other: RegionSet): RegionSet = new RegionSet(regions -- other.regions)

  override def equals(other: Any): Boolean = other match {
    case r: RegionSet => r.regions == regions
    case _            => false
  }

  def subsetOf(other: RegionSet): Boolean = regions.subsetOf(other.regions)

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

  def log(msg: String)(implicit C: Context) = () // C.info(msg) // println(msg)

  // A traversal with the side effect to annotate all functions with their region
  // it returns a _concrete_ region set, all region variables have to be resolved
  // at that point.
  def checkTree(implicit C: Context): PartialFunction[Tree, RegionSet] = {

    case FunDef(id, tparams, params, ret, body) =>

      // Since this function might be (mutally) recursive, annotate it with the current region
      // before checking.
      //
      // This is a conservative approximation that can be refined, later, potentially
      // by using region variables and collecting constraints.
      Context.annotateRegions(id.symbol, C.currentRegion)

      // regions of parameters introduced by this function
      val boundRegions: RegionSet = bindRegions(params)

      val bodyRegion = check(body)
      val reg = bodyRegion -- boundRegions

      log(s"inferred region for function ${id}: ${reg}")

      // safe inferred region on the function symbol
      Context.annotateRegions(id.symbol, reg)
      reg

    case Lambda(id, params, body) =>
      // annotated by typer
      val myRegion = Context.regionOf(id.symbol).asRegionVar
      val boundRegions: RegionSet = bindRegions(params)
      val bodyRegion = check(body)

      val inferredReg = bodyRegion -- boundRegions

      // check that myRegion >: inferredReg
      val reg = Context.constraintRegion(myRegion) match {
        case Some(allowed) =>
          if (!inferredReg.subsetOf(allowed)) {
            Context.abort(s"Region not allowed here: ${inferredReg}")
          }
          allowed
        case None => inferredReg
      }

      // safe inferred region on the function symbol
      Context.annotateRegions(id.symbol, reg)
      Context.instantiate(myRegion, reg)
      reg

    case TryHandle(body, handlers) => {

      // regions for all the capabilities
      val caps = handlers.flatMap { h => h.capability }
      val boundRegions = bindRegions(caps)

      val bodyRegion = Context.inRegion(boundRegions) { check(body) }

      val reg = bodyRegion -- boundRegions

      // check that boundRegions do not escape as part of an inferred type
      val Effectful(tpe, _) = C.inferredTypeOf(body)

      val escapes = freeRegionVariables(tpe) intersect boundRegions
      if (escapes.nonEmpty) {
        Context.abort(s"The following regions leave their defining scope ${escapes}")
      }

      // CHECK that boundRegions do not appear in tpe OR in region unification variables that
      // COULD unify with boundRegions.

      reg
    }

    // capability call
    case MemberTarget(cap, op) =>
      Context.regionOf(cap.symbol).asRegionSet

    case IdTarget(id) => id.symbol match {
      case b: BlockSymbol => Context.regionOf(b).asRegionSet
      case t: ValueSymbol => Context.currentRegion // should be annotated in the type!
    }

    case ExprTarget(e) =>
      Context.currentRegion // should be annotated in the type!

    case Call(target, _, args) =>
      val reg = check(target)

      args.map { arg => check(arg) }

      // after checking the args, we need to run unification again to see
      // whether there are now any conflicts.
      log(C.constraints.toString)
      C.regionUnifier(C.constraints.toList)

      // Zip params and args, infer region for block args and substitute
      reg

    case e: ExternFun =>
      Context.annotateRegions(e.symbol, Region.empty)
      Region.empty

    // TODO also special case state
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
}

// 1) Constraints introduced by typer:
//    {?r1} =:= {?r2}
//    {?r1} =:= {}
//    {}    =:= {}
//    {?r1} =:= { x, y, z }
//    NOT {?r1} =:= { x, ?r2, z }

// EQUALITY constraints collected by typer (between singleton unification sets AND annotated regions)
//

trait RegionCheckerOps extends ContextOps { self: Context =>

  private[regions] var currentRegion: RegionSet = Region.empty
  private[regions] var constraints: List[RegionEq] = Nil

  def initRegionstate(): Unit = {
    currentRegion = Region.empty
    constraints = annotation(Annotations.Unifier, module).constraints
  }

  def inRegion[T](r: RegionSet)(block: => T): T = {
    val before = currentRegion
    currentRegion = r
    val res = block
    currentRegion = before
    res
  }

  def instantiate(x: RegionVar, r: RegionSet): Unit = {
    x.instantiate(r)
    simplifyConstraints()
  }

  def constraintRegion(r: RegionVar): Option[RegionSet] =
    regionUnifier(constraints.toList).get(r) match {
      case Some(r: RegionVar) if r.isInstantiated => Some(r.asRegionSet)
      case Some(r: RegionSet) => Some(r)
      case _ => None
    }

  /**
   * Simplifies onstraints and replaces the constraint set with the simplified one
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
