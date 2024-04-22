package effekt

import effekt.context.Context

trait Phase[-In, +Out] { curr =>

  val phaseName: String

  def run(input: In)(using C: Context): Option[Out]

  def apply(input: In)(using C: Context): Option[Out] = None

}
