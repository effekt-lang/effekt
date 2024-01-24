package effekt
package generator
package jit

import scala.collection.mutable
import effekt.symbols.{Symbol, ValueSymbol, BlockSymbol}

object BlockNumbering {

  def numberBlocks(numbers: Map[String, BlockIndex], prog: Program): Program = {
    Program(prog.blocks.map(numberBlocks(numbers)), prog.datatypes, prog.codatatypes, prog.frameSize)
  }
  def numberBlocks(blockIndices: Map[String, BlockIndex])(block: BasicBlock): BasicBlock = block match {
    case BasicBlock(id, frameDescriptor, instructions, terminator) =>
      BasicBlock(id, frameDescriptor,
        instructions.map(numberBlocks(blockIndices)),
        numberBlocks(blockIndices)(terminator))
  }
  def numberBlocks(blockIndices: Map[String, BlockIndex])(instruction: Instruction): Instruction = {
    instruction match
      case Push(BlockName(target), args) => Push(blockIndices(target), args)
      case IfZero(arg, Clause(args, BlockName(target))) => IfZero(arg, Clause(args, blockIndices(target)))
      case _ => instruction
  }
  def numberBlocks(blockIndices: Map[String, BlockIndex])(terminator: Terminator): Terminator = {
    terminator match
      case Jump(BlockName(target)) => Jump(blockIndices(target))
      case Match(adt_type, scrutinee, clauses, default) => Match(adt_type, scrutinee, clauses.map({
          case Clause(args, BlockName(target)) => Clause(args, blockIndices(target))
          case c => c
        }),
        default match {
          case Clause(args, BlockName(target)) => Clause(args, blockIndices(target))
          case c => c
        })
      case _ => terminator
  }
}
