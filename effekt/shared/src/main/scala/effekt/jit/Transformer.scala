package effekt
package jit

import effekt.context.Context
import effekt.symbols.{BlockSymbol, ValueSymbol}
import effekt.jit.BlockNumbering.*
import effekt.machine
import effekt.machine.analysis

import scala.annotation.targetName
import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer, Queue}

object Transformer {
  def transform(program: machine.Program): Program =
    program match {
      case machine.Program(declarations, main) =>
        implicit val ProgC: ProgramContext = new ProgramContext();

        val freeVars = List();//machine.freeVariables(main).toList;

        val entryBlock = transform("?entrypoint", freeVars, main);

        val datatypes = ProgC.datatypes.map(tpe => tpe.map(pars => pars.map(transform))).toList;

        val compiledProgram = Program(entryBlock :: ProgC.basicBlocks.toList, datatypes, ProgC.frameSize);
        numberBlocks(ProgC.symbols.toMap, compiledProgram)
    }

  def transform(label: String, env: Environment, body: machine.Statement)(using ProgramContext): BasicBlock = {
    val frameDescriptor = env.frameDescriptor;

    implicit val BC: BlockContext = new BlockContext(frameDescriptor, env);
    extendFrameDescriptorTo(frameDescriptor);

    val terminator = transform(body);
    var instructions = BC.instructions.toList;

    BasicBlock(label, BC.frameDescriptor, instructions, terminator)
  }
  def transform(label: String, locals: machine.Environment, body: machine.Statement)(using ProgramContext): BasicBlock = {
    val env = transformParameters(locals);
    transform(label, env, body)
  }

  def transform(stmt: machine.Statement)(using ProgC: ProgramContext, BC: BlockContext): Terminator = {
    stmt match
      case machine.Def(machine.Label(name, environment), body, rest) => {
        emitNamed(name, transform(name, environment, body));
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
      case machine.Construct(v, tag, environment, rest) => {
        val (_, RegList(outs), restBlock) = transformInline(machine.Clause(List(v), rest))
        val vd = transformParameter(v);
        vd.typ match {
          case Type.Datatype(adtType) => {
              emit(Construct(outs(RegisterType.Datatype).head, adtType, tag, transformArguments(environment)))
              emitInlined(restBlock)
          }
          case Type.Unit() => {
            val ErasedRegister() = vd.id;
            emitInlined(restBlock)
          }
          case Type.Integer() => {
            emit(Const(outs(RegisterType.Integer).head, tag))
            emitInlined(restBlock)
          }
          case _ => ???
        }
      }
      case machine.Switch(v @ machine.Variable(name, typ), clauses) => {
        transform(typ) match {
        case Type.Datatype(adtType) =>
          Match(adtType, transformArgument(v).id, for (clause <- clauses) yield {
            val (closesOver, params, block) = transformInline(clause, reuse=false);
            val label = emit(block);
            Clause(params, label)
          })
        case Type.Integer() => {
          val List(elseClause, thenClause) = clauses;
          val (_ign1, thenArgs, thenBlock) = transformInline(thenClause);
          val (elseClosesOver, elseArgs, elseBlock) = transformInline(elseClause)
          val elseLabel = emit(elseBlock);
          emit(IfZero(transformArgument(v).id, Clause(elseArgs, elseLabel)));
          emitInlined(thenBlock)
        }
        case Type.Unit() => {
          val List(clause) = clauses;
          val (_ign1, args, block) = transformInline(clause);
          emitInlined(block)
        }
        case Type.Continuation() => {
          sys error "Fatal error: Trying to match on continuation"
        }
        }
      }
      case machine.New(v @ machine.Variable(name, machine.Negative(List(fnTyp))), List(clause), rest) => {
        val (args, _, target) = transformClosure(clause);
        val (_, RegList(outs), restBlock) = transformInline(machine.Clause(List(v), rest));
        val out = outs(RegisterType.Continuation).head
        emit(NewStack(out, target, args));
        emitInlined(restBlock)
      }
      case machine.New(name, clauses, rest) => ??? // TODO Implement codata
      case machine.Invoke(v @ machine.Variable(name, machine.Negative(List(contTyp))), 0, args) => {
        ensureEnvironment(transformParameters(args));
        Resume(transformArgument(v).id)
      }
      case machine.Invoke(value, tag, environment) => ??? // TODO Implement codata
      case machine.PushFrame(frame, rest) => {
        val (args, _, target) = transformClosure(frame);
        emit(Push(target, args));
        transform(rest)
      }
      case machine.Return(environment) => {
        Return(transformArguments(environment))
      }
      case machine.ForeignCall(out, name, ins, rest) => {
        val in_args = transformArguments(ins);
        val (_, outs, block) = transformInline(machine.Clause(List(out), rest));
        emit(PrimOp(name, outs, in_args));
        emitInlined(block)
      }
      case machine.LiteralInt(out, n, rest) => {
        //extendEnvironment(Environment.from(List(transformParameter(out))));
        val (_, RegList(outs), block) = transformInline(machine.Clause(List(out), rest));
        emit(Const(outs(RegisterType.Integer).head, n));
        emitInlined(block)
      }
      case machine.NewStack(name, frame, rest) => {
        val (closesOver, _, target) = transformClosure(frame);
        val (_, RegList(outs), restBlock) = transformInline(machine.Clause(List(name), rest));
        val out = outs(RegisterType.Continuation).head
        emit(NewStack(out, target, closesOver));
        emitInlined(restBlock)
      }
      case machine.PushStack(value, rest) => {
        emit(PushStack(transformArgument(value).id));
        transform(rest)
      }
      case machine.PopStack(name, rest) => {
        val (_, RegList(outs), block) = transformInline(machine.Clause(List(name), rest));
        val out = outs(RegisterType.Continuation).head;
        emit(Shift(out, 1));
        emitInlined(block)
      }
  }

  def transform(typ: machine.Type)(using PC: ProgramContext): Type = {
    typ match
      case machine.Positive(List(List())) => Type.Unit()
      case machine.Positive(List(List(),List())) => Type.Integer() // Boolean
      case machine.Positive(alternatives) => Type.Datatype(PC.datatypes.indexOfOrInsert(alternatives))
      case machine.Negative(contType :: Nil) => Type.Continuation()
      case machine.Negative(alternatives) => ??? // TODO Implement codata
      case machine.Type.Int() => Type.Integer()
      case machine.Type.Stack() => Type.Continuation()
  }

  def transformClosure(machineClause: machine.Clause)(using ProgramContext, BlockContext): (RegList, RegList, BlockLabel) = {
    val machine.Clause(machineParams, machineBody) = machineClause;
    val freeParams = transformParameters(machine.analysis.freeVariables(machineClause).toList);
    val freeArgs = transformArguments(freeParams);
    val jitParams = transformParameters(machineParams);
    val locals = freeParams ++ jitParams;
    val args = RegList(jitParams.locals.view.mapValues(_.map(locals.registerIndex)).toMap);
    val label = emit(transform("?generated", locals, machineBody));
    (freeArgs, args, label)
  }

  def transformInline(machineClause: machine.Clause, reuse: Boolean = true)(using ProgC: ProgramContext, BC: BlockContext): (RegList, RegList, BasicBlock) = {
    val machine.Clause(machineParams, machineBody) = machineClause;
    val jitParams = transformParameters(machineParams);
    val locals = if (reuse) then {
      val frees = transformArguments(analysis.freeVariables(machineClause).toList);
      val reusable = transformArguments(BC.environment) -- frees;
      BC.environment.extendedReusing(jitParams, reusable)
    } else {
      BC.environment ++ jitParams
    }
    val args = RegList(jitParams.locals.view.mapValues(_.map(locals.registerIndex)).toMap);
    val block = transform("?generated", locals, machineBody);
    extendFrameDescriptorTo(block.frameDescriptor);
    (transformArguments(BC.environment), args, block)
  }

  def transformParameters(params: List[machine.Variable])(using ProgramContext): Environment =
    Environment.from(params.map(transformParameter))
  @targetName("transformMachineArguments")
  def transformArguments(args: List[machine.Variable])(using ProgramContext, BlockContext): RegList =
    RegList(args.map(transformArgument).filterNot(_.typ.registerType.isErased).groupMap(_.typ.registerType)(_.id))
  def transformArguments(args: List[VariableDescriptor])(using ProgramContext, BlockContext): RegList =
    RegList(args.map(transformArgument).filterNot(_.typ.registerType.isErased).groupMap(_.typ.registerType)(_.id))
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
    val datatypes: ListBuffer[List[machine.Signature]] = mutable.ListBuffer();
    var frameSize: jit.FrameDescriptor = FrameDescriptor(Map());
    var symbols: mutable.HashMap[String, BlockIndex] = mutable.HashMap();
  }

  def emit(block: BasicBlock)(using ProgC: ProgramContext): BlockIndex = {
    ProgC.basicBlocks.addOne(block)
    // This is the correct index since the entry block is missing in [[ProgC.basicBlocks]]
    BlockIndex(ProgC.basicBlocks.size)
  }

  def emitNamed(name: String, block: BasicBlock)(using ProgC: ProgramContext): Unit = {
    ProgC.basicBlocks.addOne(block)
    // This is the correct index since the entry block is missing in [[ProgC.basicBlocks]]
    ProgC.symbols.addOne(name, BlockIndex(ProgC.basicBlocks.size))
  }

  case class Environment(locals: Map[RegisterType, List[VariableDescriptor]]) {
    def registerIndex(vd: VariableDescriptor): Register = {
      vd.typ.registerType match {
        case RegisterType.Erased => ErasedRegister()
        case t => RegisterIndex(locals.applyOrElse(t,t=>List()).indexOf(vd))
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
      Environment(vs.filterNot(_.typ.registerType.isErased).groupBy(_.typ.registerType))
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

    for (ty <- RegisterType.values) {
      // initialize graph structure
      val olds = oldVals.regs.applyOrElse(ty, ty => List()).map { case RegisterIndex(i) => i; case _ => -1 }
      val news = newVals.regs.applyOrElse(ty, ty => List()).map { case RegisterIndex(i) => i; case _ => -1 }
      //val todo = HashMap.from((news zip olds));

      // A graph where
      //   nodes represent registers
      //   edges represent data flow
      //     source: register to move from
      //     sink: register to move to
      //
      // Example graph
      //
      //   ┌►1─┐      ┌────3◄────┐
      //   │   │      │          │
      //   │   │      ▼          │
      //   └─2◄┘      4─────────►5─────►6─────►7
      //
      // In the representation of graphs,
      //   keys (Int) are sources
      //   values (mutable.HashSet[Int]) are all targets
      val todo: mutable.HashMap[Int, mutable.HashSet[Int]] = mutable.HashMap()

      // generate graph structure
      for ((source, target) <- olds zip news) {
        val targets = todo.getOrElse(source, mutable.HashSet())
        todo(source) = targets.addOne(target);
      }

      // 1. cut hairs (with COPYs)
      var changed = true;
      while (changed) {
        changed = false;
        for (source <- todo.keys; target <- todo(source); if !todo.contains(target)) {
          //                                                                               generate COPY
          //   ┌►1─┐      ┌────3◄────┐                          ┌►1─┐      ┌────3◄────┐      and remove
          //   │   │      │          │                          │   │      │          │         |
          //   │   │      ▼          │                 ~~~>     │   │      ▼          │         v
          //   └─2◄┘      4─────────►5─────►6─────►7            └─2◄┘      4─────────►5─────►6──/──►7
          //                                ^      ^                                         ^      ^
          //                             source   target                                  source   target

          emit(Copy(ty, RegisterIndex(source), RegisterIndex(target)));
          todo(source).remove(target)
          changed = true

          // no targets left for this source, drop it.
          if (todo(source).isEmpty) todo.remove(source)
        }
      }

      // 2. rotate cycles (with SWAPs)
      while (todo.nonEmpty) {
        //
        //  ┌►1─┐      ┌────3◄────┐            ┌►1─┐      ┌────3◄────┐
        //  │   │      │          │            │   │      │    ▲     │
        //  │   │      ▼          │    ~~~>    │   │      ▼    │     │
        //  └─2◄┘      4─────────►5            └─2◄┘      4────┴─/──►5
        //             ^          ^                       ^    ^     ^
        //          source      target                 source  |  target
        //                                                     |
        //                                                generate SWAP
        //                                            and change edge target

        // get a "random" source from the graph
        val (source, targets) = todo.head

        assert(targets.size == 1)
        val target = targets.head;

        if (target != source) {
          emit(Swap(ty, RegisterIndex(target), RegisterIndex(source)));
        }
        todo(source) = todo(target);
        todo.remove(target);
      }

      // 3. drop unused registers (with DROPs)
      for (i <- 0 until BC.frameDescriptor.locals(ty)) {
        if (!news.contains(i)) {
          emit(Drop(ty, RegisterIndex(i)))
        }
      }
    }
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

  extension[T] (b: mutable.ListBuffer[T]) {
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
  }

  extension (self: RegList) {
    @targetName("removeAll")
    def --(other: RegList): RegList = {
      RegList(RegisterType.values.map(t => t -> (self.regs.applyOrElse(t, t => List())
        .filterNot(other.regs.applyOrElse(t, t => List()).contains(_)))).toMap)
    }
  }
}
