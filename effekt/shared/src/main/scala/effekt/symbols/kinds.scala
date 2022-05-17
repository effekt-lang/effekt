package effekt
package symbols

import effekt.context.Context

package object kinds {

  def wellformed(tpe: Type)(implicit C: Context): Unit = tpe match {
    case t: ValueType => wellformed(t)
    case t: BlockType => wellformed(t)
  }

  def wellformed(effs: Effects)(implicit C: Context): Unit = effs.toList foreach { wellformedEffect }

  def wellformed(tpe: ValueType)(implicit C: Context): Unit = wellformedType(tpe) match {
    case Kind.Type => ()
    case Kind.Fun(args, Kind.Type) => C.abort(s"${tpe} needs to be applied to ${args.size} type arguments")
    case _ => C.abort(s"Expected a type but got ${tpe}")
  }

  def wellformedEffect(eff: Effect)(implicit C: Context): Unit = eff match {
    case EffectAlias(_, tparams, effs) =>
      wellformed(effs)
      Kind.EffectFun(tparams map { p => Kind.Type })

    case BuiltinEffect(_, tparams) =>
      Kind.EffectFun(tparams map { p => Kind.Type })

    case i: InterfaceType => wellformed(i)
  }

  def wellformed(eff: InterfaceType)(implicit C: Context): Unit = wellformedInterfaceType(eff) match {
    case Kind.Effect => ()
    case Kind.Fun(args, Kind.Effect) => C.abort(s"${eff} needs to be applied to ${args.size} type arguments")
    case _ => C.abort(s"Expected an effect but got a type ${eff}")
  }

  def wellformed(tpe: BlockType)(implicit C: Context): Unit = tpe match {
    case b: FunctionType => wellformed(b)
    //case c: CapabilityType => ()
    case _               => ???
  }

  def wellformed(b: FunctionType)(implicit C: Context): Unit = b match {
    case FunctionType(tps, cps, vps, bps, ret, effs) =>
      vps.foreach { tpe => wellformed(tpe) }
      bps.foreach { tpe => wellformed(tpe) }
      wellformed(ret)
      wellformed(effs)
  }

  private sealed trait Kind
  private object Kind {
    case object Type extends Kind
    case object Effect extends Kind
    case class Fun(params: List[Kind], res: Kind) extends Kind
    def TypeFun(params: List[Kind]): Kind =
      if (params.isEmpty) Kind.Type else Kind.Fun(params, Kind.Type)
    def EffectFun(params: List[Kind]): Kind =
      if (params.isEmpty) Kind.Effect else Kind.Fun(params, Kind.Effect)
  }

  private def wellformedType(tpe: ValueType)(implicit C: Context): Kind = tpe match {
    case BoxedType(tpe, region) =>
      wellformed(tpe); Kind.Type
    case _: TypeVar => Kind.Type
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

    case TypeAlias(_, tparams, tpe) =>
      wellformed(tpe)
      Kind.TypeFun(tparams map { p => Kind.Type })
    case DataType(_, tparams, _)  => Kind.TypeFun(tparams map { p => Kind.Type })
    case Record(_, tparams, _, _) => Kind.TypeFun(tparams map { p => Kind.Type })
    case BuiltinType(_, tparams)  => Kind.TypeFun(tparams map { p => Kind.Type })
    case TypeUnion(_)             => Kind.Type
    case TypeIntersection(_)      => Kind.Type
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
      Kind.EffectFun(tparams map { p => Kind.Type })
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
