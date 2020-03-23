package effekt
package context

import effekt.source.{ Id, IdDef }
import effekt.symbols.Symbol
import org.bitbucket.inkytonik.kiama.util.Memoiser

/**
 * The *global* symbol database (across modules)
 */
trait SymbolsDB {

  private val symbols: Memoiser[Id, Symbol] = Memoiser.makeIdMemoiser

  // for reverse lookup in LSP server
  private val sources: Memoiser[Symbol, IdDef] = Memoiser.makeIdMemoiser

  def assignSymbol(id: Id, d: Symbol): Unit = id match {
    case id: IdDef =>
      sources.put(d, id)
      symbols.put(id, d)
    case _ =>
      symbols.put(id, d)
  }

  def symbolOf(id: Id): Symbol = symbols(id)
  def symbolOption(id: Id): Option[Symbol] = symbols.get(id)

  // Searching the defitions for a Reference
  // =======================================
  // this one can fail.
  def symbolOf(tree: source.Reference): tree.symbol =
    symbols(tree.id).asInstanceOf[tree.symbol]

  // Searching the symbol for a definition
  // =====================================
  // these lookups should not fail (except there is a bug in the compiler)
  def symbolOf(tree: source.Definition): tree.symbol =
    symbols(tree.id).asInstanceOf[tree.symbol]

  // Searching the definition for a symbol
  // =====================================
  def definitionTreeOf(s: Symbol): Option[IdDef] = sources.get(s)
}