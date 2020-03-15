package effekt
package symbols

import source.Id

import org.bitbucket.inkytonik.kiama.util.Memoiser

class SymbolsDB {

  given Assertions

  val symbols = Memoiser.makeIdMemoiser[Id, Symbol]

  def (id: Id) symbol: Symbol = symbols(id)

  // looks up the symbols for a list of source value parameters
  def (ps: List[source.ValueParam]) allSymbols: List[ValueParam] =
    ps.map(_.id.symbol.asValueParam)

  def put(id: Id, d: Symbol) = symbols.put(id, d)

  def apply(id: Id) = symbols(id)
}