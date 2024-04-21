package effekt
package typer

sealed trait ErrorContext

object ErrorContext {
  // TODO defer rendering of error messages to Context
  def explainMismatch(tpe1: symbols.Type, tpe2: symbols.Type, outerCtx: ErrorContext): String = ""

  def explainInContext(msg: String, ctx: ErrorContext): String =
    msg
}
