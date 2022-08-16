package effekt
package jit

import effekt.context.Context
import effekt.symbols.{BlockSymbol, ValueSymbol}
import effekt.jit.Analysis.*
import effekt.machine
import effekt.jit.Analysis.indexOfOrInsert

import scala.annotation.targetName
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

  def transform(label: BlockLabel, env: Environment, body: machine.Statement)(using ProgramContext): BasicBlock = {
    val frameDescriptor = FrameDescriptor(env.locals.view.mapValues(_.length).toMap);

    implicit val BC: BlockContext = new BlockContext(frameDescriptor, env);

    val terminator = transform(body);
    var instructions = BC.instructions.toList;

    val environmentTooSmall = RegisterType.values.exists(t =>
      env.locals.applyOrElse(t, t => List()).length
        < BC.frameDescriptor.locals.applyOrElse(t, t=>0));
    if(environmentTooSmall) {
      val subst = Subst(RegList(RegisterType.values.map(t =>
        (t, ((env.locals.applyOrElse(t, t=>List()).indices)
          ++ List.fill(BC.frameDescriptor.locals.applyOrElse(t, t=>0)
                       - env.locals.applyOrElse(t, t=>List()).length+1)(-1)).map(RegisterIndex).toList))
        .toMap));
      instructions = subst :: instructions;
    }

    BasicBlock(label, BC.frameDescriptor, instructions, terminator)
  }
  def transform(label: BlockLabel, locals: machine.Environment, body: machine.Statement)(using ProgramContext): BasicBlock = {
    val env = transformParameters(locals);
    transform(label, env, body)
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
        val newEnv = transformParameters(bindings.map(_._1));
        val oldEnv = transformParameters(bindings.map(_._2));
        emitSubst(newEnv, oldEnv);
        transform(rest)
      }
      case machine.Let(machine.Variable(name, typ), tag, environment, rest) => {
        val Type.Datatype(adtType) = transform(typ);
        emit(Construct(NamedRegister(name), adtType, tag, transformArguments(environment)))
        transform(rest)
      }
      case machine.Switch(v @ machine.Variable(name, typ), clauses) => {
        transform(typ) match {
        case Type.Datatype(adtType) =>
          Match(adtType, transformArgument(v).id, for (clause <- clauses) yield {
            val (closesOver, args, block) = transformInline(clause);
            emit(block);
            Clause(closesOver, block.id)
          })
        case Type.Integer() => {
          val List(elseClause, thenClause) = clauses;
          val (_ign1, thenArgs, thenBlock) = transformInline(thenClause);
          val (elseClosesOver, elseArgs, elseBlock) = transformInline(elseClause)
          emit(elseBlock);
          emit(IfZero(transformArgument(v).id, Clause(elseClosesOver, elseBlock.id)));
          emitInlined(thenBlock)
        }
        }
      }
      case machine.New(v @ machine.Variable(name, machine.Negative(List(fnTyp))), List(clause), rest) => {
        val (args, _, target) = transformClosure(clause); // TODO
        emit(NewStack(transformArgument(v).id, target, args));
        transform(rest)
      }
      case machine.New(name, clauses, rest) => ???
      case machine.Invoke(v @ machine.Variable(name, machine.Negative(List(contTyp))), 0, args) => {
        ensureEnvironment(transformParameters(args));
        Resume(transformArgument(v).id)
      }
      case machine.Invoke(value, tag, environment) => ???
      case machine.PushFrame(frame, rest) => {
        val (args, _, target) = transformClosure(frame);
        emit(Push(target, args));
        transform(rest)
      }
      case machine.Return(environment) => {
        Return(transformArguments(environment))
      }
      case machine.Run(machine.CallForeign(name), ins, List(machine.Clause(outs, rest))) => {
        extendEnvironment(transformParameters(outs));
        emit(PrimOp(name, transformArguments(outs), transformArguments(ins)));
        transform(rest)
      }
      case machine.Run(machine.LiteralInt(n), List(), List(cont)) => {
        //extendEnvironment(Environment.from(List(transformParameter(out))));
        val (_, RegList(outs), block) = transformInline(cont);
        emit(Const(outs(RegisterType.Integer).head, n));
        emitInlined(block)
      }
      case machine.Run(name, environment, continuation) => ???
      case machine.NewStack(name, frame, rest) => {
        val (closesOver, _ignored, target) = transformClosure(frame);
        emit(NewStack(transformArgument(name).id, target, closesOver));
        transform(rest)
      }
      case machine.PushStack(value, rest) => {
        emit(PushStack(transformArgument(value).id));
        transform(rest)
      }
      case machine.PopStack(name, rest) => {
        emit(Shift(transformArgument(name).id, 1));
        transform(rest)
      }
  }

  def transform(typ: machine.Type)(using PC: ProgramContext): Type = {
    typ match
      case machine.Positive(List(List())) => Type.Unit()
      case machine.Positive(List(List(),List())) => Type.Integer() // Boolean
      case machine.Positive(alternatives) => Type.Datatype(PC.datatypes.indexOfOrInsert(alternatives))
      case machine.Negative(contType :: Nil) => Type.Continuation()
      case machine.Negative(alternatives) => ???
      case machine.Primitive("Int") => Type.Integer()
      case machine.Primitive(name) => ???
  }

  def transformClosure(clause: machine.Clause)(using ProgramContext, BlockContext): (RegList, RegList, BlockLabel) = {
    val machine.Clause(parameters, body) = clause;
    val freeVars = transformParameters(machine.analysis.freeVariables(clause).toList);
    val label = new FreshBlockLabel();
    val frees = transformArguments(freeVars);
    val params = transformParameters(parameters);
    val locals = freeVars ++ params;
    val args = RegList(params.locals.view.mapValues(_.map(locals.registerIndex)).toMap);
    emit(transform(label, locals, body));
    (frees, args, label)
  }

  def transformInline(clause: machine.Clause)(using ProgC: ProgramContext, BC: BlockContext): (RegList, RegList, BasicBlock) = {
    val machine.Clause(parameters, body) = clause;
    val params = transformParameters(parameters);
    val locals = BC.environment ++ params;
    extendFrameDescriptorTo(locals);
    val args = RegList(params.locals.view.mapValues(_.map(locals.registerIndex)).toMap);
    (transformArguments(BC.environment), args, transform(new FreshBlockLabel(), locals, body))
  }

  def transformParameters(params: List[machine.Variable])(using ProgramContext): Environment =
    Environment.from(params.map(transformParameter))
  @targetName("transformMachineArguments")
  def transformArguments(args: List[machine.Variable])(using ProgramContext, BlockContext): RegList =
    RegList(args.map(transformArgument).filter(_.typ.registerType.isDefined).groupMap(_.typ.registerType.get)(_.id))
  def transformArguments(args: List[VariableDescriptor])(using ProgramContext, BlockContext): RegList =
    RegList(args.map(transformArgument).filter(_.typ.registerType.isDefined).groupMap(_.typ.registerType.get)(_.id))
  def transformArguments(args: Environment)(using ProgramContext, BlockContext): RegList =
    RegList(args.locals.view.mapValues(_.map(transformArgument(_).id)).toMap)

  def transformParameter(v: machine.Variable)(using ProgramContext): VariableDescriptor = {
    transform(v.tpe) match {
      case Type.Unit() => VariableDescriptor(Type.Unit(), ErasedRegister())
      case ty => VariableDescriptor(ty, NamedRegister(v.name))
    }
  }

  def transformArgument(v: VariableDescriptor)(using ProgC: ProgramContext, BC: BlockContext): VariableDescriptor = {
    VariableDescriptor(v.typ, BC.environment.registerIndex(v))
  }
  def transformArgument(v: machine.Variable)(using ProgC: ProgramContext, BC: BlockContext): VariableDescriptor = {
    transformArgument(transformParameter(v))
  }

  class ProgramContext() {
    val basicBlocks: ListBuffer[BasicBlock] = ListBuffer();
    val datatypes: ListBuffer[List[machine.Signature]] = ListBuffer();
  }

  def emit(block: BasicBlock)(using ProgC: ProgramContext): Unit = {
    ProgC.basicBlocks.addOne(block)
  }

  case class Environment(locals: Map[RegisterType, List[VariableDescriptor]]) {
    def registerIndex(vd: VariableDescriptor): Register = {
      vd.typ.registerType match {
        case None => ErasedRegister()
        case Some(t) => RegisterIndex(locals.applyOrElse(t,t=>List()).indexOf(vd))
      }
    }
    @targetName("extended")
    def ++(vs: Environment): Environment = {
      Environment(RegisterType.values.map(t => t -> (locals.applyOrElse(t, x => List()) ++ vs.locals.applyOrElse(t, x => List()))).toMap)
    }
  }
  object Environment {
    def from(vs: List[VariableDescriptor]): Environment = {
      Environment(vs.filter(_.typ.registerType.isDefined).groupBy(_.typ.registerType.get))
    }
  }

  class BlockContext(var frameDescriptor: FrameDescriptor,
                     var environment: Environment) {
    val instructions: ListBuffer[Instruction] = ListBuffer();
  }

  def emit(instruction: Instruction)(using BC: BlockContext): Unit = {
    BC.instructions.addOne(instruction)
  }

  def emitInlined(block: BasicBlock)(using BC: BlockContext): Terminator = {
    BC.instructions.appendAll(block.instructions);
    block.terminator
  }

  def emitSubst(newEnv: Environment, vals: Environment)
               (using ProgC: ProgramContext, BC: BlockContext): Unit = {
    val oldVals = transformArguments(vals);
    BC.environment = newEnv;
    val newVals = transformArguments(newEnv);
    if (oldVals == newVals) return // nothing to do
    emit(Subst(oldVals))
  }
  def ensureEnvironment(newEnvironment: Environment)(using ProgC: ProgramContext, BC: BlockContext): Unit = {
    emitSubst(newEnvironment, newEnvironment);
  }
  def extendEnvironment(additional: Environment)(using ProgC: ProgramContext, BC: BlockContext): Unit = {
    BC.environment = BC.environment ++ additional;
    extendFrameDescriptorTo(BC.environment);
  }
  def extendFrameDescriptorTo(env: Environment)(using ProgC: ProgramContext, BC: BlockContext): Unit = {
    BC.frameDescriptor = FrameDescriptor(
      RegisterType.values.map(t =>(t ->
        Math.max(BC.frameDescriptor.locals.applyOrElse(t, x => 0), env.locals.applyOrElse(t, x => List()).length))).toMap);
  }
}
