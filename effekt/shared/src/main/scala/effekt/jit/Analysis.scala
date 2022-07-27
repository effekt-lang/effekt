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

  object RegisterAllocation {
    class BlockContext() {
      var intRegs: ListBuffer[Register] = ListBuffer();
      var contRegs: ListBuffer[Register] = ListBuffer();
      var datatypeRegs: ListBuffer[Register] = ListBuffer();
    }

    def getRegisterIndex(reg: Register, typ: Type)(using BC: BlockContext): Register = {
      typ match
        case Type.Unit => ErasedRegister()
        case Type.Integer => RegisterIndex(BC.intRegs.indexOfOrInsert(reg))
        case Type.Continuation => RegisterIndex(BC.contRegs.indexOfOrInsert(reg))
        case Type.Datatype => RegisterIndex(BC.datatypeRegs.indexOfOrInsert(reg))
    }
    def getRegisterIndices(regList: RegList)(using BC: BlockContext): RegList = {
      val RegList(intRegs, contRegs, datatypeRegs) = regList;
      RegList(
        intRegs.map(r => RegisterIndex(BC.intRegs.indexOfOrInsert(r))),
        contRegs.map(r => RegisterIndex(BC.contRegs.indexOfOrInsert(r))),
        datatypeRegs.map(r => RegisterIndex(BC.datatypeRegs.indexOfOrInsert(r)))
      )
    }

    def transform(prog: Program): Program = prog match
      case Program(blocks) => {
        Program(blocks.map(transform))
      }
    def transform(block: BasicBlock): BasicBlock = block match
      case BasicBlock(id, frameDescriptor, instructions, terminator) => {
        implicit val BC: BlockContext = RegisterAllocation.BlockContext();

        for (VariableDescriptor(typ, name) <- frameDescriptor) {getRegisterIndex(name, typ)};

        val newInstructions = instructions.map(transform);
        val newTerminator = transform(terminator);
        BasicBlock(id, frameDescriptor , newInstructions, newTerminator)
      }
    def transform(instruction: Instruction)(using BlockContext): Instruction = instruction match {
      case Const(out, value) => Const(getRegisterIndex(out, Type.Integer), value)
      case PrimOp(name, out, in) => PrimOp(name, getRegisterIndices(out), getRegisterIndices(in))
      case Add(out, in1, in2) => Add(getRegisterIndex(out, Type.Integer), getRegisterIndex(in1, Type.Integer), getRegisterIndex(in2, Type.Integer))
      case Mul(out, in1, in2) => Mul(getRegisterIndex(out, Type.Integer), getRegisterIndex(in1, Type.Integer), getRegisterIndex(in2, Type.Integer))
      case Push(target, args) => Push(target, getRegisterIndices(args))
      case Shift(out, n) => Shift(getRegisterIndex(out, Type.Continuation), n)
      case Reset() => Reset()
      case Print(arg) => Print(getRegisterIndex(arg, Type.Integer))
      case IfZero(arg, Clause(args, target)) => IfZero(getRegisterIndex(arg, Type.Integer),
        Clause(getRegisterIndices(args), target))
      case IsZero(out, arg) => IsZero(getRegisterIndex(out, Type.Integer), getRegisterIndex(arg, Type.Integer))
      case Subst(args) => Subst(getRegisterIndices(args))
      case Construct(out, adt_type, tag, args) => Construct(getRegisterIndex(out, Type.Datatype),
        adt_type, tag, getRegisterIndices(args))
    }
    def transform(terminator: Terminator)(using BlockContext): Terminator = terminator match {
      case Return(args) => Return(getRegisterIndices(args))
      case Jump(target) => Jump(target)
      case Resume(cont) => Resume(getRegisterIndex(cont, Type.Continuation))
      case Match(adt_type, scrutinee, clauses) => Match(adt_type, getRegisterIndex(scrutinee, Type.Datatype),
        clauses.map({case Clause(args, target) => Clause(getRegisterIndices(args), target)}))
    }
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
