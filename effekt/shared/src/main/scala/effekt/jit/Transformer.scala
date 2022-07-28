package effekt
package jit

import effekt.context.Context
import effekt.symbols.{BlockSymbol, ValueSymbol}
import effekt.jit.Analysis.*
import effekt.machine
import effekt.machine.LiteralInt
import effekt.jit.Analysis.indexOfOrInsert

import scala.collection.mutable.{HashMap, ListBuffer}

object Transformer {
  def transform(mainSymbol: BlockLabel, program: machine.Program): Program =
    program match {
      case machine.Program(declarations, main) =>
        implicit val ProgC: ProgramContext = new ProgramContext();

        val freeVars = List();//machine.freeVariables(main).toList;

        val entryBlock = transform(mainSymbol, freeVars, main);

        val datatypes = ProgC.datatypes.map(tpe => tpe.map(pars => pars.map(transform))).toList;

        val compiledProgram = Program(entryBlock :: ProgC.basicBlocks.toList, datatypes);
        numberBlocks(compiledProgram)
    }

  def transform(label: BlockLabel, locals: machine.Environment, body: machine.Statement)(using ProgramContext): BasicBlock = {
    val frameDescriptor = transformParameters(locals);

    implicit val BC: BlockContext = new BlockContext(frameDescriptor);

    val terminator = transform(body);
    val instructions = BC.instructions.toList;

    BasicBlock(label, frameDescriptor, instructions, terminator)
  }

  def transform(stmt: machine.Statement)(using ProgramContext, BlockContext): Terminator = {
    stmt match
      case machine.Def(machine.Label(name, environment), body, rest) => {
        emit(transform(BlockName(name), environment, body));
        transform(rest)
      }
      case machine.Jump(machine.Label(name, environment)) => {
        Jump(BlockName(name))
      }
      case machine.Substitute(bindings, rest) => {
        emitSubst(bindings.map({case (v,_) => v}), bindings.map({case (_,v) => v}));
        transform(rest)
      }
      case machine.Let(machine.Variable(name, typ), tag, environment, rest) => {
        val Type.Datatype(adtType) = transform(typ);
        emit(Construct(NamedRegister(name), adtType, tag, transformArguments(environment)))
        transform(rest)
      }
      case machine.Switch(machine.Variable(name, typ), clauses) => {
        val Type.Datatype(adtType) = transform(typ);
        Match(adtType, NamedRegister(name), clauses.map(transform))
      }
      case machine.New(machine.Variable(name, machine.Negative(List(fnTyp))), List(clause), rest) => {
        val Clause(args, target) = transform(clause);
        emit(Reset())
        val freeVars = machine.freeVariables(clause).toList;
        emit(Push(target, transformArguments(freeVars)))
        emit(Shift(NamedRegister(name), 1))
        transform(rest)
      }
      case machine.New(name, clauses, rest) => ???
      case machine.Invoke(machine.Variable(name, machine.Negative(List(contTyp))), 0, args) => {
        ensureEnvironment(args);
        Resume(NamedRegister(name))
      }
      case machine.Invoke(value, tag, environment) => ???
      case machine.PushFrame(frame, rest) => ???
      case machine.Return(environment) => {
        Return(transformArguments(environment))
      }
      case machine.Run(machine.CallForeign(name), ins, List(machine.Clause(outs, rest))) => {
        val freeVars = machine.freeVariables(machine.Clause(outs, rest)).toList;
        val killedIns = (ins.toSet -- outs -- freeVars).toList;
        ensureEnvironment(freeVars ++ outs ++ killedIns);
        emit(PrimOp(name, transformArguments(outs), transformArguments(ins)));
        transform(rest)
      }
      case machine.Run(machine.LiteralInt(n), List(), List(machine.Clause(List(out), rest))) => {
        val freeVars = machine.freeVariables(machine.Clause(List(out), rest)).toList;
        ensureEnvironment(freeVars ++ List(out));
        emit(Const(transformArgument(out).id, n));
        transform(rest)
      }
      case machine.Run(name, environment, continuation) => ???
  }

  def transform(clause: machine.Clause)(using ProgramContext, BlockContext): Clause = {
    clause match {
      case machine.Clause(parameters, body) =>
        val freeVars = machine.freeVariables(clause).toList;
        val locals = freeVars ++ parameters; // TODO: Is this correct?
        val label = new FreshBlockLabel();
        val frees = transformArguments(freeVars);
        emit(transform(label, locals, body));
        Clause(frees, label)
    }
  }

  def transform(typ: machine.Type)(using PC: ProgramContext): Type = {
    typ match
      case machine.Positive(List(List())) => Type.Unit()
      case machine.Positive(alternatives) => Type.Datatype(PC.datatypes.indexOfOrInsert(alternatives))
      case machine.Negative(contType :: Nil) => Type.Continuation()
      case machine.Negative(alternatives) => ???
      case machine.Primitive("Int") => Type.Integer()
      case machine.Primitive(name) => ???
  }

  def transformParameters(args: List[machine.Variable])(using ProgramContext): FrameDescriptor = {
    val intRegs: ListBuffer[VariableDescriptor] = ListBuffer();
    val contRegs: ListBuffer[VariableDescriptor] = ListBuffer();
    val datatypeRegs: ListBuffer[VariableDescriptor] = ListBuffer();
    for (v <- args) {
      val vd = transformParameter(v);
      vd match
        case VariableDescriptor(Type.Integer(), reg) => intRegs.addOne(vd)
        case VariableDescriptor(Type.Datatype(idx), reg) => datatypeRegs.addOne(vd)
        case VariableDescriptor(Type.Continuation(), reg) => contRegs.addOne(vd)
        case VariableDescriptor(Type.Unit(), ErasedRegister()) => {}
    }
    return FrameDescriptor(intRegs.toList, contRegs.toList, datatypeRegs.toList)
  }
  def transformArguments(params: List[machine.Variable])(using ProgramContext, BlockContext): RegList = {
    val intRegs: ListBuffer[Register] = ListBuffer();
    val contRegs: ListBuffer[Register] = ListBuffer();
    val datatypeRegs: ListBuffer[Register] = ListBuffer();
    for (v <- params) {
      transformArgument(v) match
        case VariableDescriptor(Type.Integer(), reg) => intRegs.addOne(reg)
        case VariableDescriptor(Type.Datatype(idx), reg) => datatypeRegs.addOne(reg)
        case VariableDescriptor(Type.Continuation(), reg) => contRegs.addOne(reg)
        case VariableDescriptor(Type.Unit(), ErasedRegister()) => {}
    }
    return RegList(intRegs.toList, contRegs.toList, datatypeRegs.toList)
  }

  def transformParameter(v: machine.Variable)(using ProgramContext): VariableDescriptor = {
    transform(v.tpe) match {
      case Type.Unit() => VariableDescriptor(Type.Unit(), ErasedRegister())
      case ty => VariableDescriptor(ty, NamedRegister(v.name))
    }
  }
  def transformArgument(v: machine.Variable)(using ProgC: ProgramContext, BC: BlockContext): VariableDescriptor = {
    val param = transformParameter(v);
    transform(v.tpe) match {
      case Type.Unit() => VariableDescriptor(Type.Unit(), ErasedRegister())
      case Type.Integer() => VariableDescriptor(Type.Integer(), RegisterIndex(BC.currentFrameDescriptor.intRegs.indexOf(param)))
      case Type.Continuation() => VariableDescriptor(Type.Integer(), RegisterIndex(BC.currentFrameDescriptor.contRegs.indexOf(param)))
      case Type.Datatype(adtType) => VariableDescriptor(Type.Integer(), RegisterIndex(BC.currentFrameDescriptor.datatypeRegs.indexOf(param)))
    }
  }

  class ProgramContext() {
    val basicBlocks: ListBuffer[BasicBlock] = ListBuffer();
    val datatypes: ListBuffer[List[machine.Signature]] = ListBuffer();
  }

  def emit(block: BasicBlock)(using ProgC: ProgramContext): Unit = {
    ProgC.basicBlocks.addOne(block)
  }

  class BlockContext(var currentFrameDescriptor: FrameDescriptor) {
    val instructions: ListBuffer[Instruction] = ListBuffer();
  }

  def emit(instruction: Instruction)(using BC: BlockContext): Unit = {
    BC.instructions.addOne(instruction)
  }

  def emitSubst(params: machine.Environment, args: machine.Environment)
               (using ProgC: ProgramContext, BC: BlockContext): Unit = {
    val newFrameDescriptor = transformParameters(params);
    val newFrameDescriptorWithOldNames = transformParameters(args);
    if(newFrameDescriptorWithOldNames != BC.currentFrameDescriptor){
      emit(Subst(transformArguments(args)));
    }
    BC.currentFrameDescriptor = newFrameDescriptor;
  }
  def ensureEnvironment(newEnvironment: machine.Environment)(using ProgC: ProgramContext, BC: BlockContext): Unit = {
    emitSubst(newEnvironment, newEnvironment)
  }
}
