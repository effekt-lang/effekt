package effekt
package namer


import effekt.context.{ Context, ContextOps }

object Namer extends Phase[Parsed, NameResolved] {

  val phaseName = "namer"

  def run(input: Parsed)(using Context): Option[NameResolved] = None
}

trait NamerOps extends ContextOps { Context: Context =>

}
