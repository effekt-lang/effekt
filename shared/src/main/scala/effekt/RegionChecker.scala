package effekt
package regions

import effekt.source._
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.subtitutions.RegionEq
import effekt.symbols.{ BlockSymbol, Symbol, ValueSymbol, Effectful }

sealed trait Region {
  def asRegionSet(implicit C: Context): RegionSet
  def asRegionVar(implicit C: Context): RegionVar
}

/**
 * source is the tree at which position this region variable has been introduced
 * we use it mainly for error reporting
 *
 * TODO maybe make RegionVar a subtype of Regions, not Region. In this case we would need
 * to substitute before taking the union.
 */
case class RegionVar(id: Int, source: Tree) extends Region {

  private var _region: Option[RegionSet] = None

  def region(implicit C: Context): RegionSet = _region match {
    case None    => ??? // lookup in Context and use substitution
    case Some(r) => r
  }

  def isDefined: Boolean = _region.isDefined

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
    case RegionVar(otherId, _) => id == otherId
    case _                     => false
  }
  override def asRegionSet(implicit C: Context): RegionSet =
    C.panic(s"Required a concrete region set, but got: ${this}")
  override def asRegionVar(implicit C: Context): RegionVar = this
}

/**
 * Conceptually: A set of region variables
 *
 * { x, y, z, ?r1, ?r2 } = {x, y, z} u ?r1 u ?r2
 */
class RegionSet(val regions: Set[Symbol]) extends Region {

  def contains(r: Symbol): Boolean = regions.contains(r)

  def ++(other: RegionSet): RegionSet = new RegionSet(regions ++ other.regions)
  def --(other: RegionSet): RegionSet = new RegionSet(regions -- other.regions)

  //  def subst(unifier: Map[RegionVar, RegionSet]): RegionSet =
  //    new RegionSet(regions.flatMap { r => unifier.getOrElse(r, RegionSet(r)).regions
  //      case r: RegionVar    => unifier.getOrElse(r, RegionSet(r)).regions
  //      case s: SymbolRegion => Set(s)
  //    })

  override def equals(other: Any): Boolean = other match {
    case r: RegionSet => r.regions == regions
    case _            => false
  }

  def subsetOf(other: RegionSet): Boolean = regions.subsetOf(other.regions)

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
    RegionVar(lastId, source)
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
      val reg = Context.regionInstantation(myRegion) match {
        case Some(allowed) =>
          if (!inferredReg.subsetOf(allowed)) {
            Context.abort(s"Region not allowed here: ${inferredReg}")
          }
          allowed
        case None => inferredReg
      }

      // safe inferred region on the function symbol
      Context.annotateRegions(id.symbol, reg)
      myRegion.instantiate(reg)
      reg

    case TryHandle(body, handlers) => {

      // regions for all the capabilities
      val caps = handlers.flatMap { h => h.capability }
      val boundRegions = bindRegions(caps)

      val bodyRegion = Context.inRegion(boundRegions) { check(body) }

      val reg = bodyRegion -- boundRegions
      // TODO check that boundRegions do not escape as part of an inferred type

      val Effectful(tpe, _) = C.inferredTypeOf(body)

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

  def initRegionstate(): Unit =
    currentRegion = Region.empty

  def inRegion[T](r: RegionSet)(block: => T): T = {
    val before = currentRegion
    currentRegion = r
    val res = block
    currentRegion = before
    res
  }

  def regionInstantation(r: RegionVar): Option[RegionSet] =
    regionUnifier(constraints.toList).get(r) match {
      case Some(r: RegionVar) if r.isDefined => Some(r.region)
      case Some(r: RegionSet) => Some(r)
      case _ => None
    }

  def constraints: Set[RegionEq] =
    annotation(Annotations.Unifier, module).constraints

  // TODO just a first quick draft...
  // Union find?
  def regionUnifier(cs: List[RegionEq]): Map[RegionVar, Region] = cs match {

    case RegionEq(x: RegionVar, y: RegionVar) :: rest if x.isDefined && y.isDefined =>
      if (x.region != y.region) {
        abort(s"Region mismatch: ${x.region} is not equal to ${y.region}")
      } else {
        regionUnifier(rest)
      }

    case RegionEq(r1: Region, r2: Region) :: rest if r1 == r2 =>
      regionUnifier(rest)

    case RegionEq(x: RegionVar, r) :: rest if !x.isDefined =>
      val remainder = regionUnifier(substConstraints(x, r, rest))
      remainder + (x -> r)

    case RegionEq(r, x: RegionVar) :: rest if !x.isDefined =>
      val remainder = regionUnifier(substConstraints(x, r, rest))
      remainder + (x -> r)

    case RegionEq(r1: RegionSet, r2: RegionSet) :: rest if (r1 != r2) =>
      abort(s"Region mismatch: $r1 is not equal to $r2")

    case RegionEq(r1: RegionSet, r2: RegionSet) :: rest =>
      regionUnifier(rest)

    case Nil => Map.empty
  }

  private def substConstraints(x: RegionVar, r: Region, cs: List[RegionEq]): List[RegionEq] =
    cs.map {
      case RegionEq(r1, r2) => RegionEq(if (r1 == x) r else r1, if (r2 == x) r else r2)
    }
}
