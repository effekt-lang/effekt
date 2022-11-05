package effekt
package symbols

import effekt.context.Context

package object kinds {

  def wellformed(tpe: Type)(implicit C: Context): Unit = tpe match {
    case t: ValueType => wellformed(t)
    case t: BlockType => wellformed(t)
  }

  def wellformed(effs: Effects)(implicit C: Context): Unit =
    val effects = effs.toList
    if (effects.size != effects.distinct.size) {
      C.panic("Compiler invariant violated: duplicate effects.")
    }
    effs.toList foreach { wellformedEffect }

  def wellformed(tpe: ValueType)(implicit C: Context): Unit = wellformedType(tpe) match {
    case Kind.VType => ()
    case Kind.Fun(args, Kind.VType) => C.abort(s"${tpe} needs to be applied to ${args.size} type arguments")
    case _ => C.abort(s"Expected a value type but got ${tpe}")
  }

  def wellformed(tpe: BlockType)(implicit C: Context): Unit = tpe match {
    case b: FunctionType  => wellformed(b)
    case c: InterfaceType => wellformed(c)
  }

  def wellformedEffect(eff: InterfaceType)(implicit C: Context): Unit = eff match {
    case i: InterfaceType          => wellformed(i)
  }

  def wellformed(eff: InterfaceType)(implicit C: Context): Unit = wellformedInterfaceType(eff) match {
    case Kind.BType => ()
    case Kind.Fun(args, Kind.BType) => C.abort(s"${eff} needs to be applied to ${args.size} type arguments")
    case o => C.abort(s"Expected a block type but got a type ${eff} of kind ${o}")
  }

  def wellformed(b: FunctionType)(implicit C: Context): Unit = b match {
    case FunctionType(tps, cps, vps, bps, res, effs) =>
      // TODO we could also check whether the same type variable shows up twice
      if (cps.size != (bps.size + effs.controlEffects.size)) {
        C.panic(s"Compiler invariant violated: different size of capture parameters and block parameters: ${b}")
      }
      vps.foreach { tpe => wellformed(tpe) }
      bps.foreach { tpe => wellformed(tpe) }
      wellformed(res)
      wellformed(effs)
  }

  private sealed trait Kind
  private object Kind {
    case object VType extends Kind
    case object BType extends Kind
    case class Fun(params: List[Kind], res: Kind) extends Kind
    def VFun(params: List[Kind]): Kind =
      if (params.isEmpty) Kind.VType else Kind.Fun(params, Kind.VType)
    def BFun(params: List[Kind]): Kind =
      if (params.isEmpty) Kind.BType else Kind.Fun(params, Kind.BType)
  }

  private def wellformedType(tpe: ValueType)(implicit C: Context): Kind = tpe match {
    case BoxedType(tpe, region) =>
      wellformed(tpe); Kind.VType
    case _: TypeVar => Kind.VType
    case ValueTypeApp(tpe, args) =>
      val Kind.Fun(params, res) = wellformedType(tpe) match {
        case t: Kind.Fun => t
        case _           => C.abort(s"Expected a type constructor, but got: ${tpe}")
      }
      if (args.size != params.size) {
        C.abort(s"Wrong type constructor arity. Type constructor ${tpe} expects ${params.size} parameters, but got ${args.size} arguments.")
      }
      args foreach { a => wellformedType(a) }
      res

    case DataType(_, tparams, _)  => Kind.VFun(tparams map { p => Kind.VType })
    case Record(_, tparams, _, _) => Kind.VFun(tparams map { p => Kind.VType })
    case BuiltinType(_, tparams)  => Kind.VFun(tparams map { p => Kind.VType })
  }

  private def wellformedInterfaceType(e: InterfaceType)(implicit C: Context): Kind = e match {
    case BlockTypeApp(eff, args) =>
      val Kind.Fun(params, res) = wellformedInterfaceType(eff) match {
        case t: Kind.Fun => t
        case _           => C.abort(s"Expected an effect that takes type parameters, but got: ${eff}")
      }
      if (args.size != params.size) {
        C.abort(s"Wrong number of type arguments. Effect ${eff} expects ${params.size} parameters, but got ${args.size} arguments.")
      }
      args foreach { a => wellformed(a) }
      res
    case Interface(_, tparams, _) =>
      Kind.BFun(tparams map { p => Kind.VType })
  }

  private implicit class ValueTypeWellformedOps[T <: ValueType](tpe: T) {
    def wellformed(implicit C: Context): T = {
      kinds.wellformed(tpe)
      tpe
    }
  }
  private implicit class EffectWellformedOps[T <: InterfaceType](eff: T) {
    def wellformed(implicit C: Context): T = {
      kinds.wellformed(eff)
      eff
    }
  }
  private implicit class EffectsWellformedOps(effs: Effects) {
    def wellformed(implicit C: Context): Effects = {
      kinds.wellformed(effs)
      effs
    }
  }
}
