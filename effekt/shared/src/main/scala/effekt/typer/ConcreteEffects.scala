package effekt
package typer

import effekt.context.Context
import effekt.symbols._
import effekt.util.messages.ErrorMessageReifier


/**
 * All effects inferred by Typer are required to be concrete and dealiased.
 *
 * This way, we can easily compare them for equality.
 */
class ConcreteEffects private[typer] (protected val effects: List[InterfaceType])
