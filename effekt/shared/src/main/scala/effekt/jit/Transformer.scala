package effekt
package jit

import effekt.context.Context
import effekt.symbols.{BlockSymbol, ValueSymbol}
import effekt.jit.Analysis.*
import effekt.machine
import effekt.machine.analysis
import effekt.jit.Analysis.indexOfOrInsert

import scala.annotation.targetName
import scala.collection.mutable.{HashMap, ListBuffer, Queue}

object Transformer {
  def transform(mainSymbol: BlockLabel, program: machine.Program): Program =
    program match {
      case machine.Program(declarations, main) =>
        implicit val ProgC: ProgramContext = new ProgramContext();

        val freeVars = List();//machine.freeVariables(main).toList;

        val entryBlock = transform(mainSymbol, freeVars, main);

        val datatypes = ProgC.datatypes.map(tpe => tpe.map(pars => pars.map(transform))).toList;

        val compiledProgram = Program(entryBlock :: ProgC.basicBlocks.toList, datatypes, ProgC.frameSize);
        numberBlocks(compiledProgram)
    }

  def transform(label: BlockLabel, env: Environment, body: machine.Statement)(using ProgramContext): BasicBlock = {
    val frameDescriptor = env.frameDescriptor;

    implicit val BC: BlockContext = new BlockContext(frameDescriptor, env);
    extendFrameDescriptorTo(frameDescriptor);

    val terminator = transform(body);
    var instructions = BC.instructions.toList;

    BasicBlock(label, BC.frameDescriptor, instructions, terminator)
  }
  def transform(label: BlockLabel, locals: machine.Environment, body: machine.Statement)(using ProgramContext): BasicBlock = {
    val env = transformParameters(locals);
    transform(label, env, body)
  }

  def transform(stmt: machine.Statement)(using ProgC: ProgramContext, BC: BlockContext): Terminator = {
    stmt match
      case machine.Def(machine.Label(name, environment), body, rest) => {
        emit(transform(BlockName(name), environment, body));
        transform(rest)
      }
      case machine.Jump(machine.Label(name, environment)) => {
        emitSubst(transformParameters(environment), transformArguments(environment)); // TODO: Get rid of this!
        Jump(BlockName(name))
      }
      case machine.Substitute(bindings, rest) => {
        val newEnv = transformParameters(bindings.map(_._1));
        val oldEnv = transformParameters(bindings.map(_._2));
        val unchanged = Environment.from(BC.environment.locals.valuesIterator.flatten.filterNot(newEnv.contains).toList);
        emitSubst(unchanged ++ newEnv, unchanged ++ oldEnv);
        transform(rest)
      }
      case machine.Let(v, tag, environment, rest) => {
        val (_, RegList(outs), restBlock) = transformInline(machine.Clause(List(v), rest))
        val vd = transformParameter(v);
        val Type.Datatype(adtType) = vd.typ;
        emit(Construct(outs(RegisterType.Datatype).head, adtType, tag, transformArguments(environment)))
        emitInlined(restBlock)
      }
      case machine.Switch(v @ machine.Variable(name, typ), clauses) => {
        transform(typ) match {
        case Type.Datatype(adtType) =>
          Match(adtType, transformArgument(v).id, for (clause <- clauses) yield {
            val (closesOver, args, block) = transformInline(clause, reuse=false);
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
        case Type.Unit() => {
          val List(clause) = clauses;
          val (_ign1, args, block) = transformInline(clause);
          emitInlined(block)
        }
        case Type.Continuation() => {
          ???
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
      case machine.Run(machine.CallForeign(name), ins, List(cont)) => {
        val in_args = transformArguments(ins);
        val (_, outs, block) = transformInline(cont);
        emit(PrimOp(name, outs, in_args));
        emitInlined(block)
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

  def transformInline(clause: machine.Clause, reuse: Boolean = true)(using ProgC: ProgramContext, BC: BlockContext): (RegList, RegList, BasicBlock) = {
    val machine.Clause(parameters, body) = clause;
    val params = transformParameters(parameters);
    val locals = if (reuse) then {
      val frees = transformArguments(analysis.freeVariables(clause).toList);
      val reusable = transformArguments(BC.environment) -- frees;
      BC.environment.extendedReusing(params, reusable)
    } else {
      BC.environment ++ params
    }
    val args = RegList(params.locals.view.mapValues(_.map(locals.registerIndex)).toMap);
    val block = transform(new FreshBlockLabel(), locals, body);
    extendFrameDescriptorTo(block.frameDescriptor);
    (transformArguments(BC.environment), args, block)
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
    var frameSize: jit.FrameDescriptor = FrameDescriptor(Map());
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

    def extendedReusing(by: Environment, reuse: RegList): Environment = {
      val reuseQ: Map[RegisterType, Queue[Register]] = reuse.regs.view.mapValues(Queue.from(_)).toMap;
      Environment(RegisterType.values.map(t => t -> {
        val cur = ListBuffer.from(locals.applyOrElse(t, x => List()));
        for (vd <- by.locals.applyOrElse(t, x => List())) {
          if(reuseQ(t).nonEmpty) {
            val reuseReg = reuseQ(t).dequeue();
            val idx = reuseReg match {
              case RegisterIndex(index) => index
              case _ => cur.indexWhere(vd => vd.id == reuseReg)
            };
            cur(idx) = vd
          } else {
            cur.addOne(vd)
          }
        }
        cur.toList
      }).toMap)
    }

    def contains(vd: VariableDescriptor): Boolean = {
      locals.valuesIterator.flatten.contains(vd)
    }

    def frameDescriptor: FrameDescriptor =
      FrameDescriptor(RegisterType.values.map(t =>
        (t,locals.applyOrElse(t, t=>List()).length)).toMap)
  }
  object Environment {
    def from(vs: List[VariableDescriptor]): Environment = {
      Environment(vs.filter(_.typ.registerType.isDefined).groupBy(_.typ.registerType.get))
    }
  }

  private def max(a: FrameDescriptor, b: FrameDescriptor): FrameDescriptor = {
    FrameDescriptor(RegisterType.values.map(t => (t ->
      Math.max(a.locals.applyOrElse(t, x => 0), b.locals.applyOrElse(t, x => 0)))).toMap)
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
    emitSubst(newEnv, oldVals)
  }
  def emitSubst(newEnv: Environment, oldVals: RegList)
               (using ProgC: ProgramContext, BC: BlockContext): Unit = {
    BC.environment = newEnv;
    val newVals = transformArguments(newEnv);
    if (oldVals == newVals) return // nothing to do
    extendFrameDescriptorTo(newEnv);
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
    extendFrameDescriptorTo(env.frameDescriptor)
  }
  def extendFrameDescriptorTo(fd: FrameDescriptor)(using ProgC: ProgramContext, BC: BlockContext): Unit = {
    BC.frameDescriptor = max(BC.frameDescriptor, fd);
    ProgC.frameSize = max(ProgC.frameSize, fd);
  }

  extension(self: RegList) {
    @targetName("removeAll")
    def --(other: RegList): RegList = {
      RegList(RegisterType.values.map(t => t -> (self.regs.applyOrElse(t, t => List())
        .filterNot(other.regs.applyOrElse(t, t => List()).contains(_)))).toMap)
    }
  }
}
