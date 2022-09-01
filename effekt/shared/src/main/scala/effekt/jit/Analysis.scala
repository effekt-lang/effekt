package effekt
package jit

import scala.collection.mutable.{HashMap, ListBuffer}
import effekt.symbols.{Symbol, ValueSymbol, BlockSymbol}

object Analysis {

  def allocateRegisters(prog: Program): Program = prog match
    case Program(blocks) => {
      Program(blocks.map(allocateRegisters))
    }
  def allocateRegisters(block: BasicBlock): BasicBlock = block match
    case BasicBlock(id, frameDescriptor, instructions, terminator) => {
      var regs: Map[Type, ListBuffer[Register]] = Type.values.map(ty => (ty, ListBuffer[Register]())).toMap;
      for (ty <- Type.values){
        regs(ty).addAll(frameDescriptor
          .filter({case VariableDescriptor(ty2, _) => ty == ty2})
          .map({case VariableDescriptor(_, v) => v}))
      };
      def getRegisterIndex(reg: Register, typ: Type): Register ={
        typ match {
          case Type.Unit => ErasedRegister()
          case _ => {
            var res = regs(typ).indexWhere({curId => curId == reg});
            if (res == -1) {
              res = regs(typ).length;
              regs(typ).addOne(reg);
            }
            RegisterIndex(res)
          }
        }
      }
      def getRegisterIndices(regList: RegList): RegList = regList match {
        case RegList(intRegs, contRegs, datatypeRegs) => {
          RegList(intRegs.map(r => getRegisterIndex(r, Type.Integer)),
            contRegs.map(r => getRegisterIndex(r, Type.Continuation)),
            datatypeRegs.map(r => getRegisterIndex(r, Type.Datatype)))
        }
      }
      val newInstructions = instructions.map({
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
      });
      val newTerminator = terminator match {
        case Return(args) => Return(getRegisterIndices(args))
        case Jump(target) => Jump(target)
        case Resume(cont) => Resume(getRegisterIndex(cont, Type.Continuation))
        case Match(adt_type, scrutinee, clauses) => Match(adt_type, getRegisterIndex(scrutinee, Type.Datatype),
          clauses.map({case Clause(args, target) => Clause(getRegisterIndices(args), target)}))
      };
      val newFrameDescriptor = regs.map((ty, regs) => regs.map(reg => VariableDescriptor(ty, reg))).flatten.toList;
      BasicBlock(id, newFrameDescriptor , newInstructions, newTerminator)
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
