package effekt
package symbols

import source.Id

import org.bitbucket.inkytonik.kiama.util.Memoiser

/**
 * The *global* symbol database (across modules)
 */
trait SymbolsDB {

  given Assertions

  private val symbols = Memoiser.makeIdMemoiser[Id, Symbol]

  def put(id: Id, d: Symbol) = symbols.put(id, d)

  // Searching the defitions for a Reference
  // =======================================
  // these lookups might fail
  def lookup(id: Id): Symbol = symbols(id)

  // this one can fail!
  def (tree: source.Reference) definition: tree.symbol =
    symbols(tree.id).asInstanceOf[tree.symbol]

  def get(tree: source.Reference): tree.symbol =
    symbols(tree.id).asInstanceOf[tree.symbol]


  // Searching the symbol for a definition
  // =====================================
  // these lookups should not fail (except there is a bug in the compiler)
  def (tree: source.Definition) symbol: tree.symbol = get(tree)

  def get(tree: source.Definition): tree.symbol =
    symbols(tree.id).asInstanceOf[tree.symbol]

  def getGeneric(d: source.Definition): Symbol = symbols(d.id)
}