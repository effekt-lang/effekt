package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{Annotation, Annotations, Context, ContextOps}
import effekt.context.assertions.*
import effekt.source.{ AnyPattern, Def, Effectful, IgnorePattern, MatchPattern, MatchGuard, ModuleDecl, Stmt, TagPattern, Term, Tree, resolve, symbol }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.symbols.kinds.*
import effekt.util.messages.*
import effekt.util.foreachAborting

import scala.language.implicitConversions

case class Result[+T](tpe: T, effects: ConcreteEffects)
object Typer extends Phase[NameResolved, Typechecked] {

  val phaseName = "typer"

  def run(input: NameResolved)(using Context): Option[Typechecked] = ???

}

trait TyperOps extends ContextOps { self: Context =>

  // passing `this` as ErrorReporter here is also necessary for the cyclic error
  private[typer] val unification = new Unification(using this)

  // this export is NECESSARY for the cyclic error
  export unification.{ requireSubtype }

}





