package effekt
package generator
package llvm

import effekt.machine
import effekt.util.intercalate
import effekt.machine.analysis.*

import scala.collection.mutable

object Transformer {

  val llvmFeatureFlags: List[String] = List("llvm")

  def transform(program: machine.Program): List[Definition] = program match {
    case machine.Program(declarations, statement) =>
      given MC: ModuleContext = ModuleContext();
      given FC: FunctionContext = FunctionContext();
      given BC: BlockContext = BlockContext();

      // TODO proper initialization of runtime
      emit(Call("env", envType, malloc, List(ConstantInt(1024 * 1024))));
      emit(Call("sp", spType, malloc, List(ConstantInt(1024 * 1024))));
      emit(Store(ConstantGlobal(PointerType(), "base"), LocalReference(spType, "sp")));
      pushReturnAddress("topLevel", "topLevelSharer", "topLevelEraser");

      val terminator = transform(statement);

      val definitions = MC.definitions; MC.definitions = null;
      val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
      val instructions = BC.instructions; BC.instructions = null;

      val entryBlock = BasicBlock("entry", instructions, terminator)
      val entryFunction = Function(VoidType(), "effektMain", List(), entryBlock :: basicBlocks)
      declarations.map(transform) ++ definitions :+ entryFunction
  }

  // context getters
  private def MC(using MC: ModuleContext): ModuleContext = MC
  private def FC(using FC: FunctionContext): FunctionContext = FC
  private def BC(using BC: BlockContext): BlockContext = BC

  def transform(declaration: machine.Declaration): Definition =
    declaration match {
      case machine.Extern(functionName, parameters, returnType, machine.ExternBody(_, body)) =>
        VerbatimFunction(transform(returnType), functionName, parameters.map {
          case machine.Variable(name, tpe) => Parameter(transform(tpe), name)
        }, transform(body))
      case machine.Include(ff, content) if ff.matches(llvmFeatureFlags) =>
        Verbatim(content)
      case machine.Include(ff, content) =>
        Verbatim("") // ignore, not meant for us
    }

  def transform(t: Template[machine.Variable]): String = intercalate(t.strings, t.args.map {
    case machine.Variable(name, tpe) => PrettyPrinter.localName(name)
  }).mkString

  def transform(statement: machine.Statement)(using ModuleContext, FunctionContext, BlockContext): Terminator =
    statement match {

      case machine.Def(machine.Label(name, environment), body, rest) =>
        defineFunction(name, List(Parameter(envType, "env"), Parameter(spType, "sp"))) {
          loadEnvironment(initialEnvironmentPointer, environment)
          eraseValues(environment, freeVariables(body))
          transform(body)
        }

        transform(rest)

      case machine.Jump(label) =>
        shareValues(label.environment, Set())
        storeEnvironment(initialEnvironmentPointer, label.environment)

        emit(TailCall(transform(label), List(LocalReference(envType, "env"), getStackPointer())))
        RetVoid()

      case machine.Substitute(bindings, rest) =>
        withBindings(bindings) { () =>
          transform(rest)
        }

      case machine.Construct(variable, tag, values, rest) =>
        val obj = produceObject(values, freeVariables(rest))
        val tmpName = freshName("tmp")
        emit(InsertValue(tmpName, ConstantAggregateZero(positiveType), ConstantInt(tag), 0))
        emit(InsertValue(variable.name, LocalReference(positiveType, tmpName), obj, 1))

        eraseValues(List(variable), freeVariables(rest))
        transform(rest)

      case machine.Switch(value, clauses, default) =>
        shareValues(List(value), clauses.flatMap(freeVariables).toSet)

        val tagName = freshName("tag")
        val objName = freshName("obj")
        emit(ExtractValue(tagName, transform(value), 0))
        emit(ExtractValue(objName, transform(value), 1))

        val freeInClauses = clauses.flatMap(freeVariables)

        val stackPointer = getStackPointer();
        def labelClause(clause: machine.Clause): String = {
          implicit val BC = BlockContext()
          BC.stackPointer = stackPointer

          consumeObject(LocalReference(objType, objName), clause.parameters, freeVariables(clause.body));
          eraseValues(freeInClauses, freeVariables(clause));

          val terminator = transform(clause.body);

          val instructions = BC.instructions;
          BC.instructions = null;

          val label = freshName("l");
          emit(BasicBlock(label, instructions, terminator))
          label
        }

        val defaultLabel = default match {
          case Some(clause) => labelClause(clause)
          case None =>
            val label = freshName("l");
            emit(BasicBlock(label, List(), RetVoid()))
            label
        }

        val labels = clauses.map {
          case (tag, clause) => (tag, labelClause(clause))
        }

        assert(labels.nonEmpty, "Should not be possible. In the future also support matching on void")

        Switch(LocalReference(IntegerType64(), tagName), defaultLabel, labels)

      case machine.New(variable, clauses, rest) =>
        val closureEnvironment = freeVariables(clauses).toList;

        val clauseNames = clauses.map { clause =>
          val clauseName = freshName(variable.name);
          defineFunction(clauseName, List(Parameter(objType, "obj"), Parameter(envType, "env"), Parameter(spType, "sp"))) {
            consumeObject(LocalReference(objType, "obj"), closureEnvironment, freeVariables(clause));
            loadEnvironment(initialEnvironmentPointer, clause.parameters);
            eraseValues(clause.parameters, freeVariables(clause.body));
            transform(clause.body);
          }
          ConstantGlobal(methodType, clauseName)
        }

        val arrayName = freshName(variable.name)
        emit(GlobalConstant(arrayName, ConstantArray(methodType, clauseNames)))

        val obj = produceObject(closureEnvironment, freeVariables(rest));
        val tmpName = freshName("tmp");
        emit(InsertValue(tmpName, ConstantAggregateZero(negativeType), ConstantGlobal(PointerType(), arrayName), 0));
        emit(InsertValue(variable.name, LocalReference(negativeType, tmpName), obj, 1));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.Invoke(value, tag, values) =>
        shareValues(value :: values, Set());
        storeEnvironment(initialEnvironmentPointer, values);

        val arrayName = freshName("arrayp");
        val objName = freshName("obj");
        val pointerName = freshName("fpp");
        val functionName = freshName("fp");

        emit(ExtractValue(arrayName, transform(value), 0));
        emit(ExtractValue(objName, transform(value), 1));
        emit(GetElementPtr(pointerName, methodType, LocalReference(PointerType(), arrayName), List(tag)))
        emit(Load(functionName, methodType, LocalReference(PointerType(), pointerName)))
        emit(TailCall(LocalReference(methodType, functionName), List(LocalReference(objType, objName), initialEnvironmentPointer, getStackPointer())));
        RetVoid()

      case machine.Allocate(ref @ machine.Variable(name, machine.Type.Reference(tpe)), init, evidence, rest) =>
        val idx = regionIndex(ref.tpe)

        val tmp = freshName("tmp")
        val tmpRef = LocalReference(StructureType(List(PointerType(), refType)), tmp)
        emit(Call(tmp, tmpRef.tpe, alloc, List(ConstantInt(idx), transform(evidence))));

        val ptr = freshName("ptr");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(ExtractValue(ptr, tmpRef, 0))

        emit(ExtractValue(name, tmpRef, 1))


        emit(Store(ptrRef, transform(init)))

        shareValues(List(init), freeVariables(rest))
        transform(rest);

      case machine.Allocate(_, _, _, _) =>
        ???

      case machine.Load(name, ref, ev, rest) =>
        val idx = regionIndex(ref.tpe)

        val ptr = freshName("ptr");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(Call(ptr, PointerType(), getPtr, List(transform(ref), ConstantInt(idx), transform(ev))))

        val oldVal = machine.Variable(freshName(ref.name + "_old"), name.tpe)
        emit(Load(oldVal.name, transform(oldVal.tpe), ptrRef))
        shareValue(oldVal)

        emit(Load(name.name, transform(name.tpe), ptrRef))
        eraseValues(List(name), freeVariables(rest))
        transform(rest)

      case machine.Store(ref, value, ev, rest) =>
        val idx = regionIndex(ref.tpe)

        val ptr = freshName("ptr");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(Call(ptr, PointerType(), getPtr, List(transform(ref), ConstantInt(idx), transform(ev))))

        val oldVal = machine.Variable(freshName(ref.name + "_old"), value.tpe)
        emit(Load(oldVal.name, transform(oldVal.tpe), ptrRef))
        eraseValue(oldVal)

        emit(Store(ptrRef, transform(value)))
        shareValues(List(value), freeVariables(rest))
        transform(rest)

      case machine.PushFrame(frame, rest) =>
        val frameEnvironment = freeVariables(frame).toList;

        val returnAddressName = freshName("k");
        defineFunction(returnAddressName, List(Parameter(envType, "env"), Parameter(spType, "sp"))) {

          popEnvironment(frameEnvironment);
          // eraseValues(frameEnvironment, frameEnvironment) (unnecessary)
          loadEnvironment(initialEnvironmentPointer, frame.parameters);
          eraseValues(frame.parameters, freeVariables(frame.body))

          transform(frame.body);
        }

        // TODO cache based on environment
        val sharerName = freshName("sharer");
        defineFunction(sharerName, List(Parameter(spType, "sp"))) {
          popEnvironment(frameEnvironment);
          shareValues(frameEnvironment, Set.from(frameEnvironment));
          emit(TailCall(shareFrames, List(getStackPointer())));
          RetVoid()
        }

        // TODO cache based on environment (careful, this is different from other erasers)
        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(spType, "sp"))) {
          popEnvironment(frameEnvironment);
          eraseValues(frameEnvironment, Set());
          emit(TailCall(eraseFrames, List(getStackPointer())));
          RetVoid()
        }

        shareValues(frameEnvironment, freeVariables(rest));
        pushEnvironment(frameEnvironment);
        pushReturnAddress(returnAddressName, sharerName, eraserName);

        transform(rest)

      case machine.Return(values) =>
        shareValues(values, Set())
        storeEnvironment(initialEnvironmentPointer, values);

        val returnAddress = popReturnAddress();
        emit(TailCall(LocalReference(returnAddressType, returnAddress), List(initialEnvironmentPointer, getStackPointer())));
        RetVoid()

      case machine.NewStack(variable, frame, rest) =>
        emit(Call(variable.name, transform(variable.tpe), newStack, List()));

        val frameEnvironment = freeVariables(frame).toList;

        val returnAddressName = freshName("k");
        defineFunction(returnAddressName, List(Parameter(envType, "env"), Parameter(spType, "sp"))) {

          popEnvironment(frameEnvironment);
          // eraseValues(frameEnvironment, frameEnvironment) (unnecessary)
          loadEnvironment(initialEnvironmentPointer, frame.parameters);
          eraseValues(frame.parameters, freeVariables(frame.body));

          val newStackPointer = LocalReference(spType, freshName("sp"));
          emit(Call(newStackPointer.name, spType, underflowStack, List(getStackPointer())));
          setStackPointer(newStackPointer);

          transform(frame.body);
        }

        // TODO cache based on environment (this is different from other sharers)
        val sharerName = freshName("sharer");
        defineFunction(sharerName, List(Parameter(spType, "sp"))) {
          popEnvironment(frameEnvironment);
          shareValues(frameEnvironment, Set.from(frameEnvironment));
          RetVoid()
        }

        // TODO cache based on environment (careful, this is different from other erasers)
        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(spType, "sp"))) {
          popEnvironment(frameEnvironment);
          eraseValues(frameEnvironment, Set());
          emit(Call("_", VoidType(), free, List(getStackPointer())));
          RetVoid()
        }

        shareValues(frameEnvironment, freeVariables(rest));
        val stackPointerPointer = LocalReference(PointerType(), freshName("stkspp"));
        val oldStackPointer = LocalReference(spType, freshName("stksp"));
        emit(GetElementPtr(stackPointerPointer.name, NamedType("StkVal"), LocalReference(PointerType(), variable.name), List(0, 1, 0)));
        emit(Load(oldStackPointer.name, NamedType("Sp"), stackPointerPointer));
        val temporaryStackPointer = pushEnvironmentOnto(oldStackPointer, frameEnvironment);
        val newStackPointer = pushReturnAddressOnto(temporaryStackPointer, returnAddressName, sharerName, eraserName);
        emit(Store(stackPointerPointer, newStackPointer));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.PushStack(value, rest) =>
        shareValues(List(value), freeVariables(rest));
        val newStackName = freshName("stk");
        emit(Call(newStackName, stkType, uniqueStack, List(transform(value))));
        val newStackPointerName = freshName("sp");
        emit(Call(newStackPointerName, spType, pushStack, List(LocalReference(stkType, newStackName), getStackPointer())));
        setStackPointer(LocalReference(spType, newStackPointerName));
        transform(rest)

      case machine.PopStacks(variable, n, rest) =>
        // TODO Handle n (n+1 = number of stacks to pop)
        val newStackPointerName = freshName("sp");
        val tmpName = freshName("tmp");
        val tmpReference = LocalReference(StructureType(List(stkType, spType)), tmpName);
        emit(Call(tmpName, StructureType(List(stkType, spType)), popStacks, List(getStackPointer(), transform(n))));
        emit(ExtractValue(variable.name, tmpReference, 0));
        emit(ExtractValue(newStackPointerName, tmpReference, 1));
        setStackPointer(LocalReference(spType, newStackPointerName));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.ComposeEvidence(machine.Variable(name, _), ev1, ev2, rest) =>
        emit(Add(name, transform(ev1), transform(ev2)))
        transform(rest)

      case machine.LiteralInt(machine.Variable(name, _), n, rest) =>
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.LiteralDouble(machine.Variable(name, _), x, rest) =>
        emit(FAdd(name, ConstantDouble(x), ConstantDouble(0)));
        transform(rest)

      case machine.LiteralUTF8String(v@machine.Variable(bind, _), utf8, rest) =>
        emit(GlobalConstant(s"$bind.lit", ConstantArray(IntegerType8(), utf8.map { b => ConstantInteger8(b) }.toList)))

        val res = positiveType
        val args = List(ConstantInt(utf8.size), ConstantGlobal(PointerType(), s"$bind.lit"))
        val argsT = List(IntegerType64(), PointerType())
        emit(Call(bind, res, ConstantGlobal(FunctionType(res, argsT), "c_buffer_construct"), args))

        eraseValues(List(v), freeVariables(rest));
        transform(rest)

      case machine.LiteralEvidence(machine.Variable(name, _), n, rest) =>
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.ForeignCall(machine.Variable(resultName, resultType), foreign, values, rest) =>
        // TODO careful with calling convention?!?
        val functionType = PointerType();
        shareValues(values, freeVariables(rest));
        emit(Call(resultName, transform(resultType), ConstantGlobal(functionType, foreign), values.map(transform)));
        transform(rest)

      case machine.Statement.Hole =>
        emit(Call("_", VoidType(), ConstantGlobal(FunctionType(VoidType(), Nil), "hole"), List.empty))
        RetVoid()
    }

  def transform(label: machine.Label): ConstantGlobal =
    label match {
      case machine.Label(name, _) => ConstantGlobal(PointerType(), name)
    }

  def transform(value: machine.Variable)(using FunctionContext): Operand =
    substitute(value) match {
      case machine.Variable(name, tpe) => LocalReference(transform(tpe), name)
    }

  def positiveType = NamedType("Pos");
  // TODO multiple methods (should be pointer to vtable)
  def negativeType = NamedType("Neg");
  def methodType = PointerType();
  def returnAddressType = NamedType("RetAdr");
  def sharerType = NamedType("Sharer");
  def eraserType = NamedType("Eraser");
  def frameHeaderType = NamedType("FrameHeader");
  def envType = NamedType("Env");
  def objType = NamedType("Obj");
  def spType = NamedType("Sp");
  def stkType = NamedType("Stk");
  def refType = NamedType("Ref");

  def transform(tpe: machine.Type): Type = tpe match {
    case machine.Positive(_)         => positiveType
    case machine.Negative(_)         => negativeType
    case machine.Type.Int()          => NamedType("Int")
    case machine.Type.Double()       => NamedType("Double")
    case machine.Type.String()       => positiveType
    case machine.Type.Stack()        => stkType
    case machine.Type.Reference(tpe) => refType
  }

  def environmentSize(environment: machine.Environment): Int =
    environment.map { case machine.Variable(_, typ) => typeSize(typ) }.sum

  def typeSize(tpe: machine.Type): Int =
    tpe match {
      case machine.Positive(_)       => 16
      case machine.Negative(_)       => 16
      case machine.Type.Int()        => 8 // TODO Make fat?
      case machine.Type.Double()     => 8 // TODO Make fat?
      case machine.Type.String()     => 16
      case machine.Type.Stack()      => 8 // TODO Make fat?
      case machine.Type.Reference(_) => 8
    }

  def regionIndex(tpe: machine.Type): Int =
    tpe match {
          case machine.Type.Reference(machine.Type.Int()) => 0
          case machine.Type.Reference(machine.Type.Double()) => 0
          case machine.Type.Reference(machine.Type.Positive(_)) => 1
          case machine.Type.Reference(machine.Type.Negative(_)) => 1
          case machine.Type.Reference(machine.Type.String()) => 2
          case _ => ???
    }

  def defineFunction(name: String, parameters: List[Parameter])(prog: (FunctionContext, BlockContext) ?=> Terminator): ModuleContext ?=> Unit = {
    implicit val FC = FunctionContext();
    implicit val BC = BlockContext();

    val terminator = prog;

    val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
    val instructions = BC.instructions; BC.instructions = null;

    val entryBlock = BasicBlock("entry", instructions, terminator);
    val function = Function(VoidType(), name, parameters, entryBlock :: basicBlocks);

    emit(function)
  }

  def initialEnvironmentPointer = LocalReference(envType, "env")

  def loadEnvironment(environmentPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      loadEnvironmentAt(environmentPointer, environment);
    }
  }

  def storeEnvironment(environmentPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      storeEnvironmentAt(environmentPointer, environment);
    }
  }

  def getEraser(environment: machine.Environment)(using C: ModuleContext): Operand = {
    val types = environment.map{ _.tpe };
    val freshEnvironment = environment.map{
      case machine.Variable(name, tpe) => machine.Variable(freshName(name), tpe)
    };
    val eraser = ConstantGlobal(eraserType, freshName("eraser"));

    C.erasers.getOrElseUpdate(types, {
      defineFunction(eraser.name, List(Parameter(envType, "env"))) {
        // TODO avoid unnecessary loads
        loadEnvironmentAt(LocalReference(envType, "env"), freshEnvironment);
        eraseValues(freshEnvironment, Set());
        RetVoid()
      };
      eraser
    });
  }

  def produceObject(environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Operand = {
    if (environment.isEmpty) {
      ConstantNull(objType)
    } else {
      val obj = LocalReference(objType, freshName("obj"));
      val env = LocalReference(envType, freshName("env"));
      val size = ConstantInt(environmentSize(environment));
      val eraser = getEraser(environment)

      emit(Call(obj.name, objType, newObject, List(eraser, size)));
      emit(Call(env.name, envType, objectEnvironment, List(obj)));
      shareValues(environment, freeInBody);
      storeEnvironment(env, environment);
      obj
    }
  }

  def consumeObject(obj: Operand, environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      val env = LocalReference(envType, freshName("env"));
      emit(Call(env.name, envType, objectEnvironment, List(obj)));
      loadEnvironment(env, environment);
      shareValues(environment, freeInBody);
      emit(Call("_", VoidType(), eraseObject, List(obj)));
    }
  }

  def pushEnvironment(environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    setStackPointer(pushEnvironmentOnto(getStackPointer(), environment))
  }

  def pushEnvironmentOnto(oldStackPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Operand = {
    if (environment.isEmpty) {
      oldStackPointer
    } else {
      storeEnvironmentAt(oldStackPointer, environment);

      val newStackPointer = LocalReference(spType, freshName("sp"));
      emit(GetElementPtr(newStackPointer.name, environmentType(environment), oldStackPointer, List(1)));

      newStackPointer
    }
  }

  def popEnvironment(environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    setStackPointer(popEnvironmentFrom(getStackPointer(), environment))
  }

  def popEnvironmentFrom(oldStackPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Operand = {
    if (environment.isEmpty) {
      oldStackPointer
    } else {

      val newStackPointer = LocalReference(spType, freshName("sp"));
      emit(GetElementPtr(newStackPointer.name, environmentType(environment), oldStackPointer, List(-1)));

      loadEnvironmentAt(newStackPointer, environment);

      newStackPointer
    }
  }

  def environmentType(environment: machine.Environment): Type =
    StructureType(environment.map {
      case machine.Variable(_, tpe) => transform(tpe)
    })

  def storeEnvironmentAt(pointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    val envType = environmentType(environment)
    environment.zipWithIndex.foreach {
      case (machine.Variable(name, tpe), i) =>
        val field = LocalReference(PointerType(), freshName(name + "p"));
        emit(GetElementPtr(field.name, envType, pointer, List(0, i)));
        emit(Store(field, transform(machine.Variable(name, tpe))))
    }
  }

  def loadEnvironmentAt(pointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    val envType = environmentType(environment)
    environment.zipWithIndex.foreach {
      case (machine.Variable(name, tpe), i) =>
        val field = LocalReference(PointerType(), freshName(name + "p"));
        emit(GetElementPtr(field.name, envType, pointer, List(0, i)));
        emit(Load(name, transform(tpe), field))
    }
  }

  def shareValues(values: machine.Environment, freeInBody: Set[machine.Variable])(using FunctionContext, BlockContext): Unit = {
    def loop(values: machine.Environment): Unit = {
      values match {
        case Nil => ()
        case value :: values =>
        if values.map(substitute).contains(substitute(value)) then {
          shareValue(value);
          loop(values)
        } else if freeInBody.map(substitute).contains(substitute(value)) then {
          shareValue(value);
          loop(values)
        } else {
          loop(values)
        }
      }
    };
    loop(values)
  }

  def eraseValues(environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Unit = {
    environment.map { value =>
      if !freeInBody.map(substitute).contains(substitute(value)) then {
        eraseValue(value)
      } else {
        ()
      }
    }
  }

  def shareValue(value: machine.Variable)(using FunctionContext, BlockContext): Unit = {
    value.tpe match {
      case machine.Positive(_)       => emit(Call("_", VoidType(), sharePositive, List(transform(value))))
      case machine.Negative(_)       => emit(Call("_", VoidType(), shareNegative, List(transform(value))))
      case machine.Type.Stack()      => emit(Call("_", VoidType(), shareStack, List(transform(value))))
      case machine.Type.Int()        => ()
      case machine.Type.Double()     => ()
      case machine.Type.String()     => emit(Call("_", VoidType(), shareString, List(transform(value))))
      case machine.Type.Reference(_) => ()
    }
  }

  def eraseValue(value: machine.Variable)(using FunctionContext, BlockContext): Unit = {
    value.tpe match {
      case machine.Positive(_)       => emit(Call("_", VoidType(), erasePositive, List(transform(value))))
      case machine.Negative(_)       => emit(Call("_", VoidType(), eraseNegative, List(transform(value))))
      case machine.Type.Stack()      => emit(Call("_", VoidType(), eraseStack, List(transform(value))))
      case machine.Type.Int()        => ()
      case machine.Type.Double()     => ()
      case machine.Type.String()     => emit(Call("_", VoidType(), eraseString, List(transform(value))))
      case machine.Type.Reference(_) => ()
    }
  }

  def pushReturnAddress(returnAddressName: String, scannerName: String, eraserName: String)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    setStackPointer(pushReturnAddressOnto(getStackPointer(), returnAddressName, scannerName, eraserName));
  }

  def pushReturnAddressOnto(oldStackPointer: Operand, returnAddressName: String, sharerName: String, eraserName: String)(using ModuleContext, FunctionContext, BlockContext): Operand = {

    val pointerType = PointerType();

    val returnAddressPointer = LocalReference(PointerType(), freshName("retadrp"));
    emit(GetElementPtr(returnAddressPointer.name, frameHeaderType, oldStackPointer, List(0, 0)));
    val sharerPointer = LocalReference(PointerType(), freshName("sharerp"));
    emit(GetElementPtr(sharerPointer.name, frameHeaderType, oldStackPointer, List(0, 1)));
    val eraserPointer = LocalReference(PointerType(), freshName("eraserp"));
    emit(GetElementPtr(eraserPointer.name, frameHeaderType, oldStackPointer, List(0, 2)));

    emit(Store(returnAddressPointer, ConstantGlobal(returnAddressType, returnAddressName)));
    emit(Store(sharerPointer, ConstantGlobal(sharerType, sharerName)));
    emit(Store(eraserPointer, ConstantGlobal(eraserType, eraserName)));

    val newStackPointer = LocalReference(spType, freshName("sp"));
    emit(GetElementPtr(newStackPointer.name, frameHeaderType, oldStackPointer, List(1)));

    newStackPointer
  }

  def popReturnAddress()(using ModuleContext, FunctionContext, BlockContext): String = {
    val returnAddress = freshName("f");
    setStackPointer(popReturnAddressFrom(getStackPointer(), returnAddress));
    returnAddress
  }

  def popReturnAddressFrom(oldStackPointer: Operand, returnAddressName: String)(using ModuleContext, FunctionContext, BlockContext): Operand = {

    val newStackPointer = LocalReference(spType, freshName("sp"));
    emit(GetElementPtr(newStackPointer.name, frameHeaderType, oldStackPointer, List(-1)));

    val returnAddressPointer = LocalReference(PointerType(), freshName("retadrp"));
    emit(GetElementPtr(returnAddressPointer.name, frameHeaderType, newStackPointer, List(0, 0)));

    emit(Load(returnAddressName, returnAddressType, returnAddressPointer));

    newStackPointer
  }

  def malloc = ConstantGlobal(PointerType(), "malloc");
  def free = ConstantGlobal(PointerType(), "free");

  def newObject = ConstantGlobal(PointerType(), "newObject");
  def objectEnvironment = ConstantGlobal(PointerType(), "objectEnvironment");

  def shareObject = ConstantGlobal(PointerType(), "shareObject");
  def sharePositive = ConstantGlobal(PointerType(), "sharePositive");
  def shareNegative = ConstantGlobal(PointerType(), "shareNegative");
  def shareStack = ConstantGlobal(PointerType(), "shareStack");
  def shareFrames = ConstantGlobal(PointerType(), "shareFrames");
  def shareString = ConstantGlobal(PointerType(), "c_buffer_refcount_increment");

  def eraseObject = ConstantGlobal(PointerType(), "eraseObject");
  def erasePositive = ConstantGlobal(PointerType(), "erasePositive");
  def eraseNegative = ConstantGlobal(PointerType(), "eraseNegative");
  def eraseStack = ConstantGlobal(PointerType(), "eraseStack");
  def eraseFrames = ConstantGlobal(PointerType(), "eraseFrames");
  def eraseString = ConstantGlobal(PointerType(), "c_buffer_refcount_decrement");

  def alloc = ConstantGlobal(PointerType(), "alloc")
  def getPtr = ConstantGlobal(PointerType(), "getPtr")

  def newStack = ConstantGlobal(PointerType(), "newStack");
  def pushStack = ConstantGlobal(PointerType(), "pushStack");
  def popStacks = ConstantGlobal(PointerType(), "popStacks");
  def underflowStack = ConstantGlobal(PointerType(), "underflowStack");
  def uniqueStack = ConstantGlobal(PointerType(), "uniqueStack");


  /**
   * Extra info in context
   */
  class ModuleContext() {
    var counter = 0;
    var definitions: List[Definition] = List();
    val erasers = mutable.HashMap[List[machine.Type], Operand]();
  }

  def emit(definition: Definition)(using C: ModuleContext) =
    C.definitions = C.definitions :+ definition

  def freshName(name: String)(using C: ModuleContext): String = {
    C.counter = C.counter + 1;
    name + "_" + C.counter
  }

  class FunctionContext() {
    var substitution: Map[machine.Variable, machine.Variable] = Map();
    var basicBlocks: List[BasicBlock] = List();
  }

  def emit(basicBlock: BasicBlock)(using C: FunctionContext) =
    C.basicBlocks = C.basicBlocks :+ basicBlock

  def withBindings[R](bindings: List[(machine.Variable, machine.Variable)])(prog: () => R)(using C: FunctionContext): R = {
    val substitution = C.substitution;
    C.substitution = substitution ++ bindings.map { case (variable -> value) => (variable -> substitution.getOrElse(value, value) ) }.toMap;
    val result = prog();
    C.substitution = substitution
    result
  }

  def substitute(value: machine.Variable)(using C: FunctionContext): machine.Variable =
    C.substitution.toMap.getOrElse(value, value)

  class BlockContext() {
    var stackPointer: Operand = LocalReference(spType, "sp");
    var instructions: List[Instruction] = List();
  }

  def emit(instruction: Instruction)(using C: BlockContext) =
    C.instructions = C.instructions :+ instruction

  def getStackPointer()(using C: BlockContext) =
    C.stackPointer

  def setStackPointer(stackPointer: Operand)(using C: BlockContext) =
    C.stackPointer = stackPointer;
}
