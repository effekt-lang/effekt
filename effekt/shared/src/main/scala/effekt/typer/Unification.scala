package effekt
package typer

import effekt.util.messages.ErrorReporter


class Unification(using C: ErrorReporter) extends TypeInstantiator { self =>

  def requireSubtype(): Unit = ()

}

trait TypeInstantiator { self: Unification =>

}
