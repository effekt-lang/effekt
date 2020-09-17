package effekt
package context

import effekt.source.Tree
import effekt.symbols.{ BlockSymbol, BlockType, BuiltinFunction, Effectful, Fun, Symbol, ValueSymbol, ValueType, Type }
import effekt.util.messages.ErrorReporter
import org.bitbucket.inkytonik.kiama.util.Memoiser

trait TypesDB { self: ErrorReporter =>

  // Assignment of Types to Value- and BlockSymbols
  // ==============================================
  private val values: Memoiser[ValueSymbol, ValueType] = Memoiser.makeIdMemoiser()
  private val blocks: Memoiser[BlockSymbol, BlockType] = Memoiser.makeIdMemoiser()

  def assignType(s: Symbol, tpe: BlockType): Unit = s match {
    case b: BlockSymbol => blocks.put(b, tpe)
    case _              => abort(s"Trying to store a block type for non block '${s}'")
  }

  def assignType(s: Symbol, tpe: ValueType): Unit = s match {
    case b: ValueSymbol => values.put(b, tpe)
    case _              => abort(s"Trying to store a value type for non value '${s}'")
  }

  def typeOf(s: Symbol): Type = s match {
    case s: ValueSymbol => valueTypeOf(s)
    case s: BlockSymbol => blockTypeOf(s)
    case _              => abort(s"Cannot find a type for symbol '${s}'")
  }

  def blockTypeOf(s: Symbol): BlockType =
    blockTypeOption(s) getOrElse { abort(s"Cannot find type for block '${s}'") }

  def blockTypeOption(s: Symbol): Option[BlockType] =
    s match {
      case b: BlockSymbol => blocks.get(b)
      case _              => abort(s"Trying to find a block type for non block '${s}'")
    }

  def valueTypeOf(s: Symbol): ValueType =
    valueTypeOption(s) getOrElse { abort(s"Cannot find value binder for ${s}") }

  def valueTypeOption(s: Symbol): Option[ValueType] = s match {
    case s: ValueSymbol => values.get(s)
    case _              => abort(s"Trying to find a value type for non-value '${s}'")
  }

  // Assignment of Types to Trees
  // ============================
  // Important for finding the types of temporary variables introduced by transformation
  // Can also be used by LSP server to display type information for type-checked trees
  private val types: Memoiser[Tree, Effectful] = Memoiser.makeIdMemoiser()

  def assignType(t: Tree, eff: Effectful): Unit = types.put(t, eff)
  def typeOf(t: Tree): Option[Effectful] = types.get(t)
}
