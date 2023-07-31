package effekt
package symbols

import effekt.context.Context

package object kinds {

  def wellformed(tpe: Type)(using Context): Unit = tpe match {
    case t: ValueType => wellformed(t)
    case t: BlockType => wellformed(t)
  }

  def wellformed(effs: Effects)(using C: Context): Unit =
    val effects = effs.toList
    if (effects.size != effects.distinct.size) {
      C.panic("Compiler invariant violated: duplicate effects.")
    }
    effs.toList foreach { wellformed }

  def wellformed(tpe: ValueType)(using C: Context): Unit = wellformedType(tpe) match {
    case Kind.VType => ()
    case Kind.Fun(args, Kind.VType) => C.abort(s"${tpe} needs to be applied to ${args.size} type arguments")
    case _ => C.abort(s"Expected a value type but got ${tpe}")
  }

  def wellformed(tpe: BlockType)(using Context): Unit = tpe match {
    case BlockTypeRef(x) => ()
    case b: FunctionType  => wellformed(b)
    case c: InterfaceType => wellformed(c)
  }

  def wellformed(eff: EffectsOrRef)(using C: Context): Unit = eff match {
    case x: Effects => wellformed(x)
    case x: EffectRef => ()
  }

  def wellformed(eff: InterfaceType)(using C: Context): Unit = wellformedInterfaceType(eff) match {
    case Kind.BType => ()
    case Kind.Fun(args, Kind.BType) => C.abort(s"${eff} needs to be applied to ${args.size} type arguments")
    case o => C.abort(s"Expected a block type but got a type ${eff} of kind ${o}")
  }

  def wellformed(b: FunctionType)(using C: Context): Unit = b match {
    case FunctionType(tps, cps, vps, bps, res, effs) =>
      // TODO we could also check whether the same type variable shows up twice
      if (cps.size != (bps.size + effs.canonical.size)) {
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
  }

  private def wellformedTypeConstructor(tpe: TypeConstructor)(using C: Context): Kind.Fun = tpe match {
    case DataType(_, tparams, _)  => Kind.Fun(tparams map { p => Kind.VType }, Kind.VType)
    case Record(_, tparams, _) => Kind.Fun(tparams map { p => Kind.VType }, Kind.VType)
    case ExternType(_, tparams)  => Kind.Fun(tparams map { p => Kind.VType }, Kind.VType)
  }

  private def wellformedType(tpe: ValueType)(using C: Context): Kind = tpe match {
    case BoxedType(tpe, region) => wellformed(tpe); Kind.VType
    case _: ValueTypeRef => Kind.VType
    case ValueTypeApp(tpe, args) => wellformedTypeConstructor(tpe) match {
      case Kind.Fun(params, res) if params.isEmpty && args.nonEmpty =>
        C.abort(s"Cannot apply type ${tpe}. Type ${ tpe } does not expect any arguments, but is applied to ${ args.size }.")
      case Kind.Fun(params, res) if (args.size != params.size) =>
        C.abort(s"Wrong type constructor arity. Type constructor ${tpe} expects ${params.size} parameters, but got ${args.size} arguments.")
      case Kind.Fun(params, res) =>
        args foreach { a => wellformedType(a) };
        Kind.VType
    }
  }

  private def wellformedInterfaceType(e: InterfaceType)(using C: Context): Kind = e match {
    case InterfaceType(eff, args) => wellformedBlockTypeConstructor(eff) match {
      case Kind.Fun(params, res) if args.isEmpty && params.nonEmpty =>
        C.abort(s"Wrong number of type arguments. Interface ${eff} expects ${params.size} parameters, but no arguments were provided.")
      case Kind.Fun(params, res) if args.size != params.size =>
        C.abort(s"Wrong number of type arguments. Interface ${ eff } expects ${ params.size } parameters, but got ${ args.size } arguments.")
      case Kind.Fun(params, res) =>
        args foreach { a => wellformed(a) }
        Kind.BType
    }
  }

  private def wellformedBlockTypeConstructor(e: BlockTypeConstructor)(using Context): Kind.Fun = e match {
    case Interface(_, tparams, _) => Kind.Fun(tparams map { p => Kind.VType }, Kind.BType)
    case ExternInterface(_, tparams) => Kind.Fun(tparams map { p => Kind.VType }, Kind.BType)
  }
}
