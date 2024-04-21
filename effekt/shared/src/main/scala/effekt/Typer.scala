package effekt
package typer


import effekt.context.{Annotation, Annotations, Context, ContextOps}

// This import is also NECESSARY for the cyclic error
import effekt.source.{ resolve }


import effekt.symbols.*
import effekt.util.messages.*

import scala.language.implicitConversions

case class Result[+T](tpe: T, effects: ConcreteEffects)
object Typer extends Phase[NameResolved, Typechecked] {

  val phaseName = "typer"

  def run(input: NameResolved)(using Context): Option[Typechecked] = ???
}

trait TyperOps extends ContextOps { self: Context =>

  // passing `this` as ErrorReporter here is also NECESSARY for the cyclic error
  private[typer] val unification = new Unification(using this)

  // this export is NECESSARY for the cyclic error
  export unification.{ requireSubtype }

}





