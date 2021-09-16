package effekt
package symbols

import effekt.context.Context

package object kinds {

  def wellformed(tpe: Type)(implicit C: Context): Unit = tpe match {
    case t: ValueType => wellformed(t)
    case t: BlockType => wellformed(t)
  }

  def wellformed(tpe: ValueType)(implicit C: Context): Unit = wellformedType(tpe) match {
    case Kind.VType => ()
    case Kind.Fun(args, Kind.VType) => C.abort(s"${tpe} needs to be applied to ${args.size} type arguments")
    case _ => C.abort(s"Expected a type but got ${tpe}")
  }

  def wellformed(tpe: BlockType)(implicit C: Context): Unit = wellformedType(tpe) match {
    case Kind.BType => ()
    case Kind.Fun(args, Kind.BType) => C.abort(s"${tpe} needs to be applied to ${args.size} type arguments")
    case _ => C.abort(s"Expected a block type but got ${tpe}")
  }

  private sealed trait Kind
  private object Kind {
    case object VType extends Kind
    case object BType extends Kind
    case class Fun(params: List[Kind], res: Kind) extends Kind
    def ValueTypeFun(params: List[Kind]): Kind =
      if (params.isEmpty) Kind.VType else Kind.Fun(params, Kind.VType)
    def BlockTypeFun(params: List[Kind]): Kind =
      if (params.isEmpty) Kind.VType else Kind.Fun(params, Kind.BType)
  }

  private def wellformedType(tpe: ValueType)(implicit C: Context): Kind = tpe match {
    case BoxedType(tpe) =>
      wellformedType(tpe) match {
        case Kind.BType => Kind.VType
        case t          => C.abort(s"Expected a block type, but got: ${t}")
      }
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
    case DataType(_, tparams, _)  => Kind.ValueTypeFun(tparams map { p => Kind.VType })
    case Record(_, tparams, _, _) => Kind.ValueTypeFun(tparams map { p => Kind.VType })
    case BuiltinType(_, tparams)  => Kind.ValueTypeFun(tparams map { p => Kind.VType })
  }

  private def wellformedType(tpe: BlockType)(implicit C: Context): Kind = tpe match {
    case FunctionType(tparams, vparams, bparams, ret) =>
      vparams.foreach { tpe => wellformed(tpe) }
      bparams.foreach { tpe => wellformed(tpe) }
      wellformed(ret)
      Kind.BType

    case Interface(_, tparams, ops) =>
      Kind.BlockTypeFun(tparams map { p => Kind.VType })

    case BlockTypeApp(c, targs) => ???
      //      val Kind.Fun(params, res) = wellformedEffect(eff) match {
  //        case t: Kind.Fun => t
  //        case _           => C.abort(s"Expected an effect that takes type parameters, but got: ${eff}")
  //      }
  //      if (args.size != params.size) {
  //        C.abort(s"Wrong number of type arguments. Effect ${eff} expects ${params.size} parameters, but got ${args.size} arguments.")
  //      }
  //      args foreach { a => wellformed(a) }
  //      res
  //    case Us
  }

  private implicit class ValueTypeWellformedOps[T <: ValueType](tpe: T) {
    def wellformed(implicit C: Context): T = {
      kinds.wellformed(tpe)
      tpe
    }
  }
  //  private implicit class EffectWellformedOps[T <: Effect](eff: T) {
  //    def wellformed(implicit C: Context): T = {
  //      kinds.wellformed(eff)
  //      eff
  //    }
  //  }
  //  private implicit class EffectsWellformedOps(effs: Effects) {
  //    def wellformed(implicit C: Context): Effects = {
  //      kinds.wellformed(effs)
  //      effs
  //    }
  //  }
}
