package effekt
package symbols

import effekt.util.messages.{ ErrorReporter }
import org.bitbucket.inkytonik.kiama.util.Memoiser

trait TypesDB { self: ErrorReporter =>
  private val values = Memoiser.makeIdMemoiser[ValueSymbol, ValueType]()
  private val blocks = Memoiser.makeIdMemoiser[BlockSymbol, BlockType]()

  def blockType(s: Symbol): BlockType =
    blockTypeOrDefault(s, abort(s"Cannot find type for block '${s}'"))

  def blockTypeOrDefault(s: Symbol, default: => BlockType): BlockType =
    s match {
      case b: BlockSymbol => blocks.getOrDefault(b, default)
      case _ => abort(s"Trying to find a block type for non block '${s}'")
    }

  def valueType(s: Symbol): ValueType =
    valueTypeOrDefault(s, abort(s"Cannot find value binder for ${s}"))

  def valueTypeOrDefault(s: Symbol, default: => ValueType): ValueType = s match {
    case s: ValueSymbol => values.getOrDefault(s, default)
    case _ => abort(s"Trying to find a value type for non-value '${s}'")
  }

  def putBlock(s: Symbol, tpe: BlockType) = s match {
    case b: BlockSymbol => blocks.put(b, tpe)
    case _ => abort(s"Trying to store a block type for non block '${s}'")
  }

  def putValue(s: Symbol, tpe: ValueType) = s match {
    case b: ValueSymbol => values.put(b, tpe)
    case _ => abort(s"Trying to store a value type for non value '${s}'")
  }

  def (f: Fun) returnType: Effectful = f.ret match {
    case Some(t) => t
    case None => blockTypeOrDefault(f,
      abort(s"Result type of recursive function ${f.name} needs to be annotated")).ret
  }

  def populate(builtins: Iterable[BuiltinFunction]): TypesDB = {
    builtins.foreach { b => putBlock(b, b.toType) }
    this
  }
}