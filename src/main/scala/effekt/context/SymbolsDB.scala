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

  def put(id: Id, d: Symbol) = id match {
    case id: IdDef =>
      sources.put(d, id)
      symbols.put(id, d)
    case _ =>
      symbols.put(id, d)
  }

  // Searching the defitions for a Reference
  // =======================================
  // these lookups might fail
  def lookup(id: Id): Symbol = symbols(id)
  def get(id: Id): Option[Symbol] = symbols.get(id)

  // this one can fail!
  def definition(tree: source.Reference): tree.symbol =
    symbols(tree.id).asInstanceOf[tree.symbol]

  def get(tree: source.Reference): tree.symbol =
    symbols(tree.id).asInstanceOf[tree.symbol]


  // Searching the symbol for a definition
  // =====================================
  // these lookups should not fail (except there is a bug in the compiler)
  def symbol(tree: source.Definition): tree.symbol = get(tree)

  def get(tree: source.Definition): tree.symbol =
    symbols(tree.id).asInstanceOf[tree.symbol]

  def getGeneric(d: source.Definition): Symbol = symbols(d.id)

  // Searching the definition for a symbol
  // =====================================
  def getDefinitionTree(s: Symbol): Option[IdDef] = sources.get(s)
}