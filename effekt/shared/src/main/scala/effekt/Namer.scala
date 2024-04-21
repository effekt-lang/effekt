package effekt
package namer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.context.assertions.*
import effekt.typer.Substitutions
import effekt.source.{ Def, Id, IdDef, IdRef, MatchGuard, ModuleDecl, Tree }
import effekt.symbols.*
import effekt.util.messages.ErrorMessageReifier
import effekt.symbols.scopes.*

/**
 * The output of this phase: a mapping from source identifier to symbol
 *
 * It contains both, TermSymbols and TypeSymbols
 *
 * There is an important distinction between:
 *   - resolving: that is looking up symbols (might include storing the result into the symbolTable)
 *   - binding: that is adding a binding to the environment (lexical.Scope)
 *
 *
 * TODO we could split resolution in three phases
 * 1. only look at the declaration head of definitions in one scope
 * 2. look at the bodies of effect declarations and type definitions
 * 3. look into the bodies of functions
 */
object Namer extends Phase[Parsed, NameResolved] {

  val phaseName = "namer"

  def run(input: Parsed)(using Context): Option[NameResolved] = None
}

/**
 * Environment Utils -- we use a mutable cell to express adding definitions more easily
 */
trait NamerOps extends ContextOps { Context: Context =>

}
