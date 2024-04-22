package effekt
package source


import effekt.context.Context
import effekt.symbols.Symbol

sealed trait Tree
sealed trait ValueType
sealed trait BlockType
sealed trait BlockTypeTree
sealed trait CaptureSet

type Id = String


// MOVE TO NAMER
object Resolvable {

  // Value Types
  // -----------
  extension (t: ValueType) {
    def resolve(using C: Context): symbols.ValueType = ???
  }

  // BLock Types
  // -----------
  // we need to avoid widening, so here we define BlockType as a sum without a common parent
  // (see https://github.com/lampepfl/dotty/issues/16299)
  type BlockTypes = BlockTypeTree

  type Resolved[T <: BlockTypes] = T match {
    case BlockTypeTree => symbols.BlockType
  }

  extension [T <: BlockTypes] (t: T) {
    def resolve(using C: Context): Resolved[T] = ???
  }

  // Capture Sets
  // ------------
  extension (capt: source.CaptureSet) {
    def resolve(using C: Context): symbols.Captures = ???
  }
}
export Resolvable.resolve
