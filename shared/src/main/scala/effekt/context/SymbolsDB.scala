package effekt
package context

import effekt.source.{ Id, IdDef, IdRef, Reference, FunDef }
import effekt.symbols.{ Symbol, Toplevel, Effectful }
import org.bitbucket.inkytonik.kiama.util.Memoiser

/**
 * The *global* symbol database (across modules)
 */
trait SymbolsDB { self: Context =>

  val symbols: Memoiser[Id, Symbol] = Memoiser.makeIdMemoiser()

  // the module a symbol is defined in
  val modules: Memoiser[Symbol, Toplevel] = Memoiser.makeIdMemoiser()

  // Databases used by the language server

  // for reverse lookup in LSP server
  // TODO maybe store the whole definition tree instead of the name, which requries refactoring of assignSymbol
  val sources: Memoiser[Symbol, IdDef] = Memoiser.makeIdMemoiser()

  val references: Memoiser[Symbol, List[Reference]] = Memoiser.makeIdMemoiser()

  def assignSymbol(id: Id, d: Symbol): Unit = id match {
    case id: IdDef =>
      sources.put(d, id)
      symbols.put(id, d)
      modules.put(d, module)
    case _ =>
      symbols.put(id, d)
      modules.put(d, module)
  }

  def symbolOf(id: Id): Symbol = symbolOption(id) getOrElse {
    abort(s"Internal Compiler Error: Cannot find symbol for ${id}")
  }
  def symbolOption(id: Id): Option[Symbol] = symbols.get(id)

  def owner(sym: Symbol): Toplevel = modules(sym)

  // Searching the defitions for a Reference
  // =======================================
  // this one can fail.
  def symbolOf(tree: source.Reference): tree.symbol = {
    val sym = symbolOf(tree.id).asInstanceOf[tree.symbol]
    val refs = references.getOrDefault(sym, Nil)
    references.put(sym, tree :: refs)
    sym
  }

  // Searching the symbol for a definition
  // =====================================
  // these lookups should not fail (except there is a bug in the compiler)
  def symbolOf(tree: source.Definition): tree.symbol =
    symbolOf(tree.id).asInstanceOf[tree.symbol]

  // Searching the definition for a symbol
  // =====================================
  def definitionTreeOf(s: Symbol): Option[IdDef] = sources.get(s)
}
