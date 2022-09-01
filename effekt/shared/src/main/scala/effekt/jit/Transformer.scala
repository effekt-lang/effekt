package effekt
package jit

import effekt.context.Context
import effekt.symbols.{BlockSymbol, ValueSymbol}
import effekt.jit.Analysis.*
import effekt.machine
import effekt.machine.LiteralInt

import scala.collection.mutable.{HashMap, ListBuffer}

object Transformer {
  def transform(mainSymbol: BlockLabel, program: machine.Program): Program =
    program match {
      case machine.Program(declarations, main) =>
        implicit val ProgC: ProgramContext = new ProgramContext();

        val freeVars = machine.freeVariables(main).toList;

        val entryBlock = transform(mainSymbol, freeVars, main);

        val compiledProgram = Program(entryBlock :: ProgC.basicBlocks.toList);
        numberBlocks(allocateRegisters(compiledProgram))
    }

  def transform(label: BlockLabel, locals: machine.Environment, body: machine.Statement)(using ProgramContext): BasicBlock = {
    implicit val BC: BlockContext = new BlockContext();

    val frameDescriptor = transformEnvironment(locals);
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
        emit(Subst(transform(bindings.map({case (_,v) => v}))));
        transform(rest)
      }
      case machine.Let(machine.Variable(name, typ), tag, environment, rest) => {
        val adtType: Int = -1; // TODO
        emit(Construct(NamedRegister(name), adtType, tag, transform(environment)))
        transform(rest)
      }
      case machine.Switch(machine.Variable(name, typ), clauses) => {
        val adtType: Int = -1 // TODO
        Match(adtType, NamedRegister(name), clauses.map(transform))
      }
      case machine.New(machine.Variable(name, machine.Negative(List(fnTyp))), List(clause), rest) => {
        val Clause(args, target) = transform(clause);
        emit(Reset())
        val freeVars = machine.freeVariables(clause).toList;
        emit(Push(target, transform(freeVars)))
        emit(Shift(NamedRegister(name), 1))
        transform(rest)
      }
      case machine.New(name, clauses, rest) => ???
      case machine.Invoke(machine.Variable(name, machine.Negative(List(contTyp))), 0, environment) => {
        emit(Subst(transform(environment)));
        Resume(NamedRegister(name))
      }
      case machine.Invoke(value, tag, environment) => ???
      case machine.PushFrame(frame, rest) => ???
      case machine.Return(environment) => {
        Return(transform(environment))
      }
      case machine.Run(machine.CallForeign(name), environment, List(machine.Clause(params, body))) => {
        emit(PrimOp(name, transform(params), transform(environment)));
        transform(body)
      }
      case machine.Run(name, environment, continuation) => ???
  }

  def transform(clause: machine.Clause)(using ProgramContext): Clause = {
    clause match {
      case machine.Clause(parameters, body) =>
        val freeVars = machine.freeVariables(clause).toList;
        val locals = freeVars ++ parameters; // TODO: Is this correct?
        val label = new FreshBlockLabel();
        val args = transform(freeVars);
        emit(transform(label, locals, body));
        Clause(args, label)
    }
  }

  def transform(args: List[machine.Variable])(using ProgramContext): RegList = {
    val intRegs: ListBuffer[Register] = ListBuffer();
    val contRegs: ListBuffer[Register] = ListBuffer();
    val datatypeRegs: ListBuffer[Register] = ListBuffer();
    for (machine.Variable(name, typ) <- args) {
      transform(typ) match
        case Type.Integer => intRegs.addOne(NamedRegister(name))
        case Type.Datatype => datatypeRegs.addOne(NamedRegister(name))
        case Type.Continuation => contRegs.addOne(NamedRegister(name))
        case Type.Unit => {}
    }
    return RegList(intRegs.toList, contRegs.toList, datatypeRegs.toList)
  }

  def transform(typ: machine.Type)(using PC: ProgramContext): Type = {
    typ match
      case machine.Positive(List(List())) => Type.Unit
      case machine.Positive(alternatives) => Type.Datatype
      case machine.Negative(contType :: Nil) => Type.Continuation
      case machine.Negative(alternatives) => ???
      case machine.Primitive("Int") => Type.Integer
      case machine.Primitive(name) => ???
  }

  def transformEnvironment(env: machine.Environment)(using ProgramContext): List[VariableDescriptor] = {
    env.map({case machine.Variable(name, typ) => VariableDescriptor(transform(typ), NamedRegister(name))})
  }

  class ProgramContext() {
    val basicBlocks: ListBuffer[BasicBlock] = ListBuffer();
  }

  def emit(block: BasicBlock)(using ProgC: ProgramContext): Unit = {
    ProgC.basicBlocks.addOne(block)
  }

  class BlockContext() {
    val instructions: ListBuffer[Instruction] = ListBuffer();
  }

  def emit(instruction: Instruction)(using BC: BlockContext): Unit = {
    BC.instructions.addOne(instruction)
  }
}
