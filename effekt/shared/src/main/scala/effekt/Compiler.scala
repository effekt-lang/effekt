package effekt

import kiama.util.Source
import effekt.context.Context
import kiama.output.PrettyPrinterTypes.Document


enum PhaseResult {

  val source: Source

  case Parsed(source: Source)
  case NameResolved(source: Source, mod: symbols.Module)
  case Typechecked(source: Source, mod: symbols.Module)

}
export PhaseResult.*
