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

  type BlockTypes = BlockTypeTree

  type Resolved[T <: BlockTypes] = T match {
    case BlockTypeTree => symbols.BlockType
  }

  extension [T <: BlockTypes] (t: T) {
    def resolve(using C: Context): Resolved[T] = ???
  }

  extension (capt: source.CaptureSet) {
    def resolve(using C: Context): symbols.Captures = ???
  }
}
export Resolvable.resolve
