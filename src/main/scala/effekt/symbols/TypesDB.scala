package effekt
package symbols

import effekt.util.messages.{ ErrorReporter, NoErrorReporter }
import org.bitbucket.inkytonik.kiama.util.Memoiser

class TypesDB {
  val values = Memoiser.makeIdMemoiser[ValueSymbol, ValueType]()
  val blocks = Memoiser.makeIdMemoiser[BlockSymbol, BlockType]()

  def blockType(s: Symbol)(given report: ErrorReporter): BlockType = s match {
    case b: BlockSymbol => blocks.getOrDefault(b,
      sys.error(s"Cannot find type for block '${s}'"))
    case _ => report.abort(s"Trying to find a block type for non block '${s}'")
  }

  def put(s: Symbol, tpe: BlockType)(given report: ErrorReporter) = s match {
    case b: BlockSymbol => blocks.put(b, tpe)
    case _ => report.abort(s"Trying to store a block type for non block '${s}'")
  }

  def populate(builtins: Iterable[BuiltinFunction]): TypesDB = {
    builtins.foreach { b => put(b, b.toType)(given NoErrorReporter) }
    this
  }
}