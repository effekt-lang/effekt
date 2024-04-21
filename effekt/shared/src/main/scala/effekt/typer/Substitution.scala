package effekt
package typer

import effekt.symbols._


/**
 * Substitutions not only have unification variables as keys, since we also use the same mechanics to
 * compare two types with each other (see [[TypeComparer.subFunctionType]]).
 *
 * We should **not** use these Substitutions for instantiation anymore, since this introduces fresh Unification variables.
 */
case class Substitutions(
  values: Map[TypeVar, ValueType],
  // invariant: we alway only map
  //   - a single CaptureParam -> CaptureParam
  //   - a CaptUnificationVar -> Captures
  captures: Map[CaptVar, Captures]
)
