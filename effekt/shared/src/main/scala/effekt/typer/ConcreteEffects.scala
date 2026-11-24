package effekt
package typer

import effekt.context.Context
import effekt.symbols._
import effekt.util.messages.ErrorMessageReifier


val Pure: Effects = Effects.empty

private[typer] def assertConcreteFunction(id: source.Id, tpe: BlockType)(using C: Context): Unit =
  unknowns(tpe) match {
    case us if us.nonEmpty =>
      C.abort(pretty"Cannot fully infer type for ${id}: ${tpe}")
    case _ => ()
  }

private[typer] type Unknown = CaptUnificationVar | UnificationVar

private[typer] def unknowns(tpe: ValueType): Set[Unknown] = tpe match {
  case ValueTypeRef(x) => unknowns(x)
  case ValueTypeApp(tpe, args) => args.flatMap(unknowns).toSet
  case BoxedType(tpe, capture) => unknowns(tpe) ++ unknowns(capture)
}

private def unknowns(tpe: TypeVar): Set[Unknown] = tpe match {
  case x: UnificationVar => Set(x)
  case x: TypeVar => Set.empty
}

private def unknowns(tpe: BlockType): Set[Unknown] = tpe match {
  case FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
    vparams.flatMap(unknowns).toSet ++ bparams.flatMap(unknowns).toSet ++ unknowns(result) ++ unknowns(effects)
  case InterfaceType(tpe, args) => args.flatMap(unknowns).toSet
}

private def unknowns(capt: Captures): Set[Unknown] = capt match {
  case x @ CaptUnificationVar(role) => Set(x)
  case CaptureSet(captures) => Set.empty
}

private def unknowns(effs: Effects): Set[Unknown] = effs.toList.flatMap(unknowns).toSet
