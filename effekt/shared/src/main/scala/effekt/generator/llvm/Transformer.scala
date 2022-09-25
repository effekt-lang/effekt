package effekt.llvm

import effekt.machine
import effekt.machine.analysis.*

object Transformer {

  def transform(program: machine.Program): List[Definition] = program match {
    case machine.Program(declarations, statement) =>
      given MC: ModuleContext = ModuleContext();
      given FC: FunctionContext = FunctionContext();
      given BC: BlockContext = BlockContext();

      // TODO proper initialization of runtime
      emit(Call("env", envType, malloc, List(ConstantInt(1024))));
      emit(Call("sp", spType, malloc, List(ConstantInt(1024))));
      emit(Store(ConstantGlobal(PointerType(NamedType("Sp")), "base"), LocalReference(spType, "sp")));
      pushReturnAddress("topLevel", "topLevelSharer", "topLevelEraser");

      val terminator = transform(statement);

      val definitions = MC.definitions; MC.definitions = null;
      val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
      val instructions = BC.instructions; BC.instructions = null;

      val entryBlock = BasicBlock("entry", instructions, terminator)
      val entryFunction = Function(VoidType(), "effektMain", List(), entryBlock :: basicBlocks)
      declarations.map(transform) ++ definitions ++ staticTextDefinitions :+ entryFunction
  }

  // context getters
  private def MC(using MC: ModuleContext): ModuleContext = MC
  private def FC(using FC: FunctionContext): FunctionContext = FC
  private def BC(using BC: BlockContext): BlockContext = BC

  private def staticTextDefinitions(using MC: ModuleContext): List[Definition] =
      MC.staticText.map { (global, bytes) =>
        val escaped = bytes.map(b => f"\$b%02x").mkString;
        Verbatim(s"@$global = private constant [${bytes.length} x i8] c\"$escaped\"")
      }.toList

  def transform(declaration: machine.Declaration): Definition =
    declaration match {
      case machine.Extern(functionName, parameters, returnType, body) =>
        VerbatimFunction(transform(returnType), functionName, parameters.map {
          case machine.Variable(name, tpe) => Parameter(transform(tpe), name)
        }, body)
      case machine.Include(content) =>
        Verbatim(content)
    }

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

      case machine.Switch(value, clauses) =>
        shareValues(List(value), freeVariables(clauses))

        val tagName = freshName("tag")
        val objName = freshName("obj")
        emit(ExtractValue(tagName, transform(value), 0))
        emit(ExtractValue(objName, transform(value), 1))

        val freeInClauses = freeVariables(clauses).toList

        val stackPointer = getStackPointer();
        val labels = clauses.map {
          case clause =>
            implicit val BC = BlockContext();
            BC.stackPointer = stackPointer;

            consumeObject(LocalReference(objType, objName), clause.parameters, freeVariables(clause.body));
            eraseValues(freeInClauses, freeVariables(clause));

            val terminator = transform(clause.body);

            val instructions = BC.instructions; BC.instructions = null;

            val label = freshName("l");
            emit(BasicBlock(label, instructions, terminator));
            label
        }
        labels.zipWithIndex match {
          case Nil =>
            // TODO more informative way to end program. Clean up too?
            RetVoid()
          case (label, _) :: labels =>
            Switch(LocalReference(IntegerType64(), tagName), label, labels.map { case (l, i) => (i, l) })
        }

      case machine.New(variable, List(clause), rest) =>
        // TODO multiple methods (see case below)

        val closureEnvironment = freeVariables(clause).toList;

        val clauseName = freshName(variable.name);

        defineFunction(clauseName, List(Parameter(objType, "obj"), Parameter(envType, "env"), Parameter(spType, "sp"))) {
          consumeObject(LocalReference(objType, "obj"), closureEnvironment, freeVariables(clause));
          loadEnvironment(initialEnvironmentPointer, clause.parameters);
          eraseValues(clause.parameters, freeVariables(clause.body));
          transform(clause.body);
        }

        val obj = produceObject(closureEnvironment, freeVariables(rest));
        val tmpName = freshName("tmp");
        emit(InsertValue(tmpName, ConstantAggregateZero(negativeType), ConstantGlobal(methodType, clauseName), 0));
        emit(InsertValue(variable.name, LocalReference(negativeType, tmpName), obj, 1));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      // TODO multiple methods (for one method see case above)
      case machine.New(variable, clauses, rest) =>
        ???

      case machine.Invoke(value, 0, values) =>
        shareValues(value :: values, Set());
        storeEnvironment(initialEnvironmentPointer, values);

        val functionName = freshName("fp");
        val objName = freshName("obj");

        emit(ExtractValue(functionName, transform(value), 0));
        emit(ExtractValue(objName, transform(value), 1));
        emit(TailCall(LocalReference(methodType, functionName), List(LocalReference(objType, objName), initialEnvironmentPointer, getStackPointer())));
        RetVoid()

      // TODO What is `tag`'s meaning?
      case machine.Invoke(value, tag, values) =>
        ???

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
        val stackPointerPointer = LocalReference(PointerType(spType), freshName("stkspp"));
        val oldStackPointer = LocalReference(spType, freshName("stksp"));
        emit(GetElementPtr(stackPointerPointer.name, LocalReference(PointerType(NamedType("StkVal")), variable.name), List(0, 1)));
        emit(Load(oldStackPointer.name, stackPointerPointer));
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

      case machine.PopStack(variable, rest) =>
        val newStackPointerName = freshName("sp");
        val tmpName = freshName("tmp");
        val tmpReference = LocalReference(StructureType(List(stkType, spType)), tmpName);
        emit(Call(tmpName, StructureType(List(stkType, spType)), popStack, List(getStackPointer())));
        emit(ExtractValue(variable.name, tmpReference, 0));
        emit(ExtractValue(newStackPointerName, tmpReference, 1));
        setStackPointer(LocalReference(spType, newStackPointerName));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.LiteralInt(machine.Variable(name, _), n, rest) =>
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.LiteralDouble(machine.Variable(name, _), x, rest) =>
        emit(FAdd(name, ConstantDouble(x), ConstantDouble(0)));
        transform(rest)

      case machine.LiteralUTF8String(v@machine.Variable(bind, _), utf8, rest) =>
        MC.staticText += s"$bind.lit" -> utf8
        emit(RawLLVM(s"""
%$bind.lit.decayed = bitcast [${utf8.length} x i8]* @$bind.lit to i8*
%$bind = call %Pos @c_buffer_construct(i64 ${utf8.size}, i8* %$bind.lit.decayed)
"""))
        val transformed = transform(rest)
        eraseValue(v) // TODO Is this the correct point in time to decrease the ref count by one?
        transformed

      case machine.ForeignCall(machine.Variable(resultName, resultType), foreign, values, rest) =>
        // TODO careful with calling convention?!?
        val functionType = PointerType(FunctionType(transform(resultType), values.map { case machine.Variable(_, tpe) => transform(tpe) }));
        emit(Call(resultName, transform(resultType), ConstantGlobal(functionType, foreign), values.map(transform)));
        transform(rest)
    }

  def transform(label: machine.Label): ConstantGlobal =
    label match {
      case machine.Label(name, _) => ConstantGlobal(PointerType(FunctionType(VoidType(), List(envType, spType))), name)
    }

  def transform(value: machine.Variable)(using FunctionContext): Operand =
    substitute(value) match {
      case machine.Variable(name, tpe) => LocalReference(transform(tpe), name)
    }

  def positiveType = NamedType("Pos");
  // TODO multiple methods (should be pointer to vtable)
  def negativeType = NamedType("Neg");
  def methodType = PointerType(FunctionType(VoidType(), List(objType, envType, spType)));
  def returnAddressType = NamedType("RetAdr");
  def sharerType = NamedType("Sharer");
  def eraserType = NamedType("Eraser");
  def frameHeaderType = NamedType("FrameHeader");
  def envType = NamedType("Env");
  def objType = NamedType("Obj");
  def spType = NamedType("Sp");
  def stkType = NamedType("Stk");

  def transform(tpe: machine.Type): Type = tpe match {
    case machine.Positive(_)   => positiveType
    case machine.Negative(_)   => negativeType
    case machine.Type.Int()    => NamedType("Int")
    case machine.Type.Double() => NamedType("Double")
    case machine.Type.String() => positiveType
    case machine.Type.Stack()  => stkType
  }

  def environmentSize(environment: machine.Environment): Int =
    environment.map { case machine.Variable(_, typ) => typeSize(typ) }.sum

  def typeSize(tpe: machine.Type): Int =
    tpe match {
      case machine.Positive(_)      => 16
      case machine.Negative(_)      => 16
      case machine.Type.Int()       => 8 // TODO Make fat?
      case machine.Type.Double()    => 8 // TODO Make fat?
      case machine.Type.String()    => 16
      case machine.Type.Stack()     => 8 // TODO Make fat?
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
      val typedEnvironmentPointer = castEnvironmentPointer(environmentPointer, environment);
      loadEnvironmentAt(typedEnvironmentPointer, environment);
    }
  }

  def storeEnvironment(environmentPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      val typedEnvironmentPointer = castEnvironmentPointer(environmentPointer, environment);
      storeEnvironmentAt(typedEnvironmentPointer, environment);
    }
  }

  def produceObject(environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Operand = {
    if (environment.isEmpty) {
      ConstantNull(objType)
    } else {
      val obj = LocalReference(objType, freshName("obj"));
      val env = LocalReference(envType, freshName("env"));
      val size = ConstantInt(environmentSize(environment));
      val eraser = ConstantGlobal(eraserType, freshName("eraser"));

      //TODO cache eraser based on environment
      defineFunction(eraser.name, List(Parameter(envType, "env"))) {
        val typedEnvironmentPointer = castEnvironmentPointer(LocalReference(envType, "env"), environment);
        // TODO avoid unnecessary loads
        loadEnvironmentAt(typedEnvironmentPointer, environment);
        eraseValues(environment, Set());
        RetVoid()
      }

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
      val oldTypedPointer = castEnvironmentPointer(oldStackPointer, environment);

      storeEnvironmentAt(oldTypedPointer, environment);

      val newTypedPointer = LocalReference(oldTypedPointer.tpe, freshName("sp.t"));
      emit(GetElementPtr(newTypedPointer.name, oldTypedPointer, List(1)));

      val newStackPointer = LocalReference(spType, freshName("sp"));
      emit(BitCast(newStackPointer.name, newTypedPointer, spType));

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
      val oldTypedPointer = castEnvironmentPointer(oldStackPointer, environment);

      val newTypedPointer = LocalReference(oldTypedPointer.tpe, freshName("sp.t"));
      emit(GetElementPtr(newTypedPointer.name, oldTypedPointer, List(-1)));

      loadEnvironmentAt(newTypedPointer, environment);

      val newStackPointer = LocalReference(spType, freshName("sp"));
      emit(BitCast(newStackPointer.name, newTypedPointer, spType));

      newStackPointer
    }
  }

  def castEnvironmentPointer(oldEnvironmentPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): LocalReference = {
    val environmentPointerType = PointerType(StructureType(environment.map {
      case machine.Variable(_, typ) => transform(typ)
    }));
    val newEnvironmentPointer = LocalReference(environmentPointerType, freshName("env.t."));
    emit(BitCast(newEnvironmentPointer.name, oldEnvironmentPointer, environmentPointerType));
    newEnvironmentPointer
  }

  def storeEnvironmentAt(pointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    environment.zipWithIndex.foreach {
      case (machine.Variable(name, tpe), i) =>
        val field = LocalReference(PointerType(transform(tpe)), freshName(name + "p"));
        emit(GetElementPtr(field.name, pointer, List(0, i)));
        emit(Store(field, transform(machine.Variable(name, tpe))))
    }
  }

  def loadEnvironmentAt(pointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    environment.zipWithIndex.foreach {
      case (machine.Variable(name, tpe), i) =>
        val field = LocalReference(PointerType(transform(tpe)), freshName(name + "p"));
        emit(GetElementPtr(field.name, pointer, List(0, i)));
        emit(Load(name, field))
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
      case machine.Positive(_)   => emit(Call("_", VoidType(), sharePositive, List(transform(value))))
      case machine.Negative(_)   => emit(Call("_", VoidType(), shareNegative, List(transform(value))))
      case machine.Type.Stack()  => emit(Call("_", VoidType(), shareStack, List(transform(value))))
      case machine.Type.Int()    => ()
      case machine.Type.Double() => ()
      case machine.Type.String() => emit(Call("_", VoidType(), shareString, List(transform(value))))
    }
  }

  def eraseValue(value: machine.Variable)(using FunctionContext, BlockContext): Unit = {
    value.tpe match {
      case machine.Positive(_)   => emit(Call("_", VoidType(), erasePositive, List(transform(value))))
      case machine.Negative(_)   => emit(Call("_", VoidType(), eraseNegative, List(transform(value))))
      case machine.Type.Stack()  => emit(Call("_", VoidType(), eraseStack, List(transform(value))))
      case machine.Type.Int()    => ()
      case machine.Type.Double() => ()
      case machine.Type.String() => emit(Call("_", VoidType(), eraseString, List(transform(value))))
    }
  }

  def pushReturnAddress(returnAddressName: String, scannerName: String, eraserName: String)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    setStackPointer(pushReturnAddressOnto(getStackPointer(), returnAddressName, scannerName, eraserName));
  }

  def pushReturnAddressOnto(oldStackPointer: Operand, returnAddressName: String, sharerName: String, eraserName: String)(using ModuleContext, FunctionContext, BlockContext): Operand = {

    val pointerType = PointerType(frameHeaderType);

    val oldTypedPointer = LocalReference(pointerType, freshName("sp.t"));
    emit(BitCast(oldTypedPointer.name, oldStackPointer, pointerType));

    val returnAddressPointer = LocalReference(PointerType(returnAddressType), freshName("retadrp"));
    emit(GetElementPtr(returnAddressPointer.name, oldTypedPointer, List(0, 0)));
    val sharerPointer = LocalReference(PointerType(sharerType), freshName("sharerp"));
    emit(GetElementPtr(sharerPointer.name, oldTypedPointer, List(0, 1)));
    val eraserPointer = LocalReference(PointerType(eraserType), freshName("eraserp"));
    emit(GetElementPtr(eraserPointer.name, oldTypedPointer, List(0, 2)));

    emit(Store(returnAddressPointer, ConstantGlobal(returnAddressType, returnAddressName)));
    emit(Store(sharerPointer, ConstantGlobal(sharerType, sharerName)));
    emit(Store(eraserPointer, ConstantGlobal(eraserType, eraserName)));

    val newTypedPointer = LocalReference(pointerType, freshName("sp.t"));
    emit(GetElementPtr(newTypedPointer.name, oldTypedPointer, List(1)));

    val newStackPointer = LocalReference(spType, freshName("sp"));
    emit(BitCast(newStackPointer.name, newTypedPointer, spType));

    newStackPointer
  }

  def popReturnAddress()(using ModuleContext, FunctionContext, BlockContext): String = {
    val returnAddress = freshName("f");
    setStackPointer(popReturnAddressFrom(getStackPointer(), returnAddress));
    returnAddress
  }

  def popReturnAddressFrom(oldStackPointer: Operand, returnAddressName: String)(using ModuleContext, FunctionContext, BlockContext): Operand = {

    val pointerType = PointerType(frameHeaderType);

    val oldTypedPointer = LocalReference(pointerType, freshName("sp.t"));
    emit(BitCast(oldTypedPointer.name, oldStackPointer, pointerType));

    val newTypedPointer = LocalReference(pointerType, freshName("sp.t"));
    emit(GetElementPtr(newTypedPointer.name, oldTypedPointer, List(-1)));

    val returnAddressPointer = LocalReference(PointerType(returnAddressType), freshName("retadrp"));
    emit(GetElementPtr(returnAddressPointer.name, newTypedPointer, List(0, 0)));

    emit(Load(returnAddressName, returnAddressPointer));

    val newStackPointer = LocalReference(spType, freshName("sp"));
    emit(BitCast(newStackPointer.name, newTypedPointer, spType));

    newStackPointer
  }

  def malloc = ConstantGlobal(PointerType(FunctionType(PointerType(IntegerType8()), List(IntegerType64()))), "malloc");
  def free = ConstantGlobal(PointerType(FunctionType(VoidType(), List(PointerType(IntegerType8())))), "free");

  def newObject = ConstantGlobal(PointerType(FunctionType(objType,List(IntegerType64()))), "newObject");
  def objectEnvironment = ConstantGlobal(PointerType(FunctionType(envType,List(objType))), "objectEnvironment");

  def shareObject = ConstantGlobal(PointerType(FunctionType(VoidType(),List(objType))), "shareObject");
  def sharePositive = ConstantGlobal(PointerType(FunctionType(VoidType(),List(positiveType))), "sharePositive");
  def shareNegative = ConstantGlobal(PointerType(FunctionType(VoidType(),List(negativeType))), "shareNegative");
  def shareStack = ConstantGlobal(PointerType(FunctionType(VoidType(),List(stkType))), "shareStack");
  def shareFrames = ConstantGlobal(PointerType(FunctionType(VoidType(),List(spType))), "shareFrames");
  def shareString = ConstantGlobal(PointerType(FunctionType(VoidType(),List(positiveType))), "c_buffer_refcount_increment");

  def eraseObject = ConstantGlobal(PointerType(FunctionType(VoidType(),List(objType))), "eraseObject");
  def erasePositive = ConstantGlobal(PointerType(FunctionType(VoidType(),List(positiveType))), "erasePositive");
  def eraseNegative = ConstantGlobal(PointerType(FunctionType(VoidType(),List(negativeType))), "eraseNegative");
  def eraseStack = ConstantGlobal(PointerType(FunctionType(VoidType(),List(stkType))), "eraseStack");
  def eraseFrames = ConstantGlobal(PointerType(FunctionType(VoidType(),List(spType))), "eraseFrames");
  def eraseString = ConstantGlobal(PointerType(FunctionType(VoidType(),List(positiveType))), "c_buffer_refcount_decrement");

  def newStack = ConstantGlobal(PointerType(FunctionType(stkType,List())), "newStack");
  def pushStack = ConstantGlobal(PointerType(FunctionType(spType,List(stkType, spType))), "pushStack");
  def popStack = ConstantGlobal(PointerType(FunctionType(StructureType(List(stkType,spType)),List(spType))), "popStack");
  def underflowStack = ConstantGlobal(PointerType(FunctionType(VoidType(),List(spType))), "underflowStack");
  def uniqueStack = ConstantGlobal(PointerType(FunctionType(stkType,List(stkType))), "uniqueStack");


  /**
   * Extra info in context
   */
  class ModuleContext() {
    var counter = 0;
    var definitions: List[Definition] = List();
    var staticText: Map[String, Array[Byte]] = Map();
  }

  def emit(definition: Definition)(using C: ModuleContext) =
    C.definitions = C.definitions :+ definition

  def freshName(name: String)(using C: ModuleContext): String = {
    C.counter = C.counter + 1;
    name + "." + C.counter
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
    var stackPointer: Operand = LocalReference(NamedType("Sp"), "sp");
    var instructions: List[Instruction] = List();
  }

  def emit(instruction: Instruction)(using C: BlockContext) =
    C.instructions = C.instructions :+ instruction

  def getStackPointer()(using C: BlockContext) =
    C.stackPointer

  def setStackPointer(stackPointer: Operand)(using C: BlockContext) =
    C.stackPointer = stackPointer;

}
