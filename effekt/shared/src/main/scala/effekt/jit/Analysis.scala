package effekt
package jit

import scala.collection.mutable.{HashMap, ListBuffer}
import effekt.symbols.{Symbol, ValueSymbol, BlockSymbol}

object Analysis {

  extension[T](b: ListBuffer[T])
    /**
     * Get the index of the given element or insert at end
     *
     * @return The index of `el` in the `ListBuffer`
     */
    def indexOfOrInsert(el: T): Int = {
      var index = b.indexOf(el);
      if (index == -1) {
        index = b.length;
        b.addOne(el);
      }
      index
    }

  def numberBlocks(prog: Program): Program = {
    var blockIndices: HashMap[BlockLabel, BlockIndex] = HashMap();
    for((BasicBlock(id, _, _, _), index) <- prog.blocks.zipWithIndex){
      blockIndices.addOne((id, BlockIndex(index)))
    };
    Program(prog.blocks.map(numberBlocks(blockIndices)))
  }
  def numberBlocks(blockIndices: HashMap[BlockLabel, BlockIndex])(block: BasicBlock): BasicBlock = block match {
    case BasicBlock(id, frameDescriptor, instructions, terminator) =>
      BasicBlock(id, frameDescriptor,
        instructions.map(numberBlocks(blockIndices)),
        numberBlocks(blockIndices)(terminator))
  }
  def numberBlocks(blockIndices: HashMap[BlockLabel, BlockIndex])(instruction: Instruction): Instruction = {
    instruction match
      case Push(target, args) => Push(blockIndices(target), args)
      case IfZero(arg, Clause(args, target)) => IfZero(arg, Clause(args, blockIndices(target)))
      case _ => instruction
  }
  def numberBlocks(blockIndices: HashMap[BlockLabel, BlockIndex])(terminator: Terminator): Terminator = {
    terminator match
      case Jump(target) => Jump(blockIndices(target))
      case Match(adt_type, scrutinee, clauses) => Match(adt_type, scrutinee, clauses.map({
        case Clause(args, target) => Clause(args, blockIndices(target))
      }))
      case _ => terminator
  }
}
