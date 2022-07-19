package effekt
package llvm

import effekt.machine

object Transformer {

  def transform(program: machine.Program): List[Definition] =
    program match {
      case machine.Program(declarations, statement) =>
        implicit val MC = ModuleContext();
        implicit val FC = FunctionContext();
        implicit val BC = BlockContext();

        // TODO proper initialization of runtime
        emit(Call("env", NamedType("Env"), constantMalloc, List(ConstantInt(1024))));
        emit(Call("sp", NamedType("Sp"), constantMalloc, List(ConstantInt(1024))));
        pushReturnAddress("topLevel");

        val terminator = transform(List(), statement);

        val definitions = MC.definitions; MC.definitions = null;
        val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
        val instructions = BC.instructions; BC.instructions = null;

        val entryBlock = BasicBlock("entry", instructions, terminator);
        val entryFunction = Function(VoidType(), "effektMain", List(), entryBlock :: basicBlocks);
        declarations.map(transform) ++ definitions :+ entryFunction
    }

  def transform(declaration: machine.Declaration): Definition =
    declaration match {
      case machine.DefineForeign(returnType, functionName, parameters, body) =>
        VerbatimFunction(transform(returnType), functionName, parameters.map(transformParameter), body)
      case machine.Include(content) =>
        Verbatim(content)
    }

  def transform(environment: machine.Environment, statement: machine.Statement)(using ModuleContext, FunctionContext, BlockContext): Terminator =
    statement match {
      case machine.Def(machine.Label(name, localEnvironment), body, rest) =>

        val () = {
          implicit val FC = FunctionContext();
          implicit val BC = BlockContext();

          loadEnvironment(initialEnvironmentReference, localEnvironment);
          val terminator = transform(localEnvironment, body);

          val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
          val instructions = BC.instructions; BC.instructions = null;

          val parameters = List(Parameter(NamedType("Env"), "env"), Parameter(NamedType("Sp"), "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), name, parameters, entryBlock :: basicBlocks);

          emit(function)
        };

        transform(environment, rest)

      case machine.Jump(label) =>

        storeEnvironment(initialEnvironmentReference, environment);
        emit(TailCall(transform(label), List(LocalReference(NamedType("Env"), "env"), LocalReference(NamedType("Sp"), getStackPointer()))));
        RetVoid()

      case machine.Let(variable @ machine.Variable(name, _), tag, values, rest) =>
        // TODO do nothing if values is empty
        val objName = freshName("obj");
        emit(Call(objName, NamedType("Env"), constantMalloc, List(ConstantInt(environmentSize(values)))));
        storeEnvironment(LocalReference(NamedType("Env"), objName), values);

        val tmpName = freshName("tmp");
        emit(InsertValue(tmpName, ConstantAggregateZero(positiveType), ConstantInt(tag), 0));
        emit(InsertValue(name, LocalReference(positiveType, tmpName), LocalReference(NamedType("Env"), objName), 1));

        transform(variable :: environment, rest)

      case machine.Switch(value, clauses) =>
        val tagName = freshName("tag");
        val objName = freshName("obj");
        emit(ExtractValue(tagName, transform(value), 0));
        emit(ExtractValue(objName, transform(value), 1));
        val labels = clauses.map {
          case machine.Clause(parameters, body) =>
            implicit val BC = BlockContext();

            loadEnvironment(LocalReference(NamedType("Env"), objName), parameters);
            emit(Call("_", VoidType(), constantFree, List(LocalReference(NamedType("Env"), objName))));

            val terminator = transform(environment, body);

            val instructions = BC.instructions; BC.instructions = null;

            val label = freshName("l");
            emit(BasicBlock(label, instructions, terminator));
            label
        };
        labels match {
          case Nil =>
            // TODO more informative way to end program. Clean up too?
            RetVoid()
          case label :: labels =>
            Switch(LocalReference(I64(), tagName), label, labels.zipWithIndex.map { case (l, i) => (i, l) })
        }

      case machine.Substitute(bindings, rest) =>

        // TODO this is a hack
        val environment = bindings.map { case (_, value) => value };
        transform(environment, rest)

      case machine.PushFrame(frame, rest) =>

        val frameEnvironment: machine.Environment = machine.freeVariables(frame).toList;

        val frameName = freshName("k");

        val () = {
          implicit val FC = FunctionContext();
          implicit val BC = BlockContext();

          loadEnvironment(initialEnvironmentReference, frame.parameters);
          popEnvironment(frameEnvironment);
          val terminator = transform(frame.parameters ++ frameEnvironment, frame.body);

          val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
          val instructions = BC.instructions; BC.instructions = null;

          val parameters = List(Parameter(NamedType("Env"), "env"), Parameter(NamedType("Sp"), "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), frameName, parameters, entryBlock :: basicBlocks);

          emit(function)
        };

        pushEnvironment(frameEnvironment);
        pushReturnAddress(frameName);

        transform(environment, rest)

      case machine.Return(environment) =>

        storeEnvironment(initialEnvironmentReference, environment);

        val returnAddress = popReturnAddress();
        emit(TailCall(LocalReference(returnAddressType, returnAddress), List(initialEnvironmentReference, LocalReference(NamedType("Sp"), getStackPointer()))));
        RetVoid()

      case machine.Run(machine.LiteralInt(n), List(), List(machine.Clause(List(variable @ machine.Variable(name, machine.Primitive("Int"))), rest))) =>

        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(variable :: environment, rest)

      case machine.Run(machine.CallForeign(name), values, List()) =>
        // TODO careful with calling convention?!?
        val functionType = PointerType(FunctionType(VoidType(), values.map { case machine.Variable(_, typ) => transform(typ) }));
        emit(Call("_", VoidType(), ConstantGlobal(functionType, name), values.map(transform)));
        RetVoid()

      case machine.Run(machine.CallForeign(name), values, List(machine.Clause(List(variable @ machine.Variable(resultName, resultType)), rest))) =>
        // TODO careful with calling convention?!?
        val functionType = PointerType(FunctionType(transform(resultType), values.map { case machine.Variable(_, typ) => transform(typ) }));
        emit(Call(resultName, transform(resultType), ConstantGlobal(functionType, name), values.map(transform)));
        transform(variable :: environment, rest)
    }

  def transform(label: machine.Label): ConstantGlobal =
    label match {
      case machine.Label(name, typ) => ConstantGlobal(PointerType(FunctionType(VoidType(), List(NamedType("Env"), NamedType("Sp")))), name)
    }

  def transform(value: machine.Variable): Operand =
    value match {
      case machine.Variable(name, typ) => LocalReference(transform(typ), name)
    }

  def transformParameter(variable: machine.Variable): Parameter =
    variable match {
      case machine.Variable(name, typ) => Parameter(transform(typ), name)
    }

  def positiveType: Type = StructureType(List(I64(), NamedType("Env")));

  def transform(typ: machine.Type): Type = typ match {
    case machine.Primitive("Int") => NamedType("Int")
    case machine.Positive(_)      => positiveType
  }

  def environmentSize(environment: machine.Environment): Int =
    environment.map { case machine.Variable(_, typ) => typeSize(typ) }.sum

  def typeSize(typ: machine.Type): Int =
    typ match {
      case machine.Positive(_)      => 16
      case machine.Negative(_)      => 16
      case machine.Primitive("Int") => 8
    }

  def initialEnvironmentReference = LocalReference(NamedType("Env"), "env")

  def loadEnvironment(environmentReference: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    // TODO do nothing if empty

    val environmentType = StructureType(environment.map { case machine.Variable(_, typ) => transform(typ) });
    val pointerType = PointerType(environmentType);
    val environmentPointerName = freshName("env.t.");
    emit(BitCast(environmentPointerName, environmentReference, pointerType));

    environment.zipWithIndex.foreach {
      case (machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(pointerType, environmentPointerName), List(0, i)));
        emit(Load(name, LocalReference(PointerType(fieldType), fieldPointerName)))
    }
  }

  def storeEnvironment(environmentReference: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    // TODO do nothing if empty

    val environmentType = StructureType(environment.map { case machine.Variable(_, typ) => transform(typ) });
    val pointerType = PointerType(environmentType);
    val environmentPointerName = freshName("env.t.");
    emit(BitCast(environmentPointerName, environmentReference, pointerType));

    environment.zipWithIndex.foreach {
      case (value @ machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(pointerType, environmentPointerName), List(0, i)));
        emit(Store(LocalReference(PointerType(fieldType), fieldPointerName), transform(value)))
    }
  }

  def pushEnvironment(environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    // TODO do nothing if empty

    val environmentType = StructureType(environment.map { case machine.Variable(_, typ) => transform(typ) });

    val oldStackPointer = getStackPointer();
    val pointerType = PointerType(environmentType);
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(NamedType("Sp"), oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), NamedType("Sp")));
    setStackPointer(newStackPointer);

    environment.zipWithIndex.foreach {
      case (value @ machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(pointerType, oldTypedPointer), List(0, i)));
        emit(Store(LocalReference(PointerType(fieldType), fieldPointerName), transform(value)))
    }

  }

  def popEnvironment(environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    // TODO do nothing if empty

    val environmentType = StructureType(environment.map { case machine.Variable(_, typ) => transform(typ) });

    val oldStackPointer = getStackPointer();
    val pointerType = PointerType(environmentType);
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(NamedType("Sp"), oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(-1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), NamedType("Sp")));
    setStackPointer(newStackPointer);

    environment.zipWithIndex.foreach {
      case (value @ machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(pointerType, newTypedPointer), List(0, i)));
        emit(Load(name, LocalReference(PointerType(fieldType), fieldPointerName)))
    }

  }

  def returnAddressType = PointerType(FunctionType(VoidType(), List(NamedType("Env"), NamedType("Sp"))))

  def pushReturnAddress(frameName: String)(using ModuleContext, FunctionContext, BlockContext): Unit = {

    val oldStackPointer = getStackPointer();
    val pointerType = PointerType(returnAddressType);
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(NamedType("Sp"), oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), NamedType("Sp")));
    setStackPointer(newStackPointer);

    emit(Store(LocalReference(pointerType, oldTypedPointer), ConstantGlobal(returnAddressType, frameName)));
  }

  def popReturnAddress()(using ModuleContext, FunctionContext, BlockContext): String = {

    val oldStackPointer = getStackPointer();
    val pointerType = PointerType(returnAddressType);
    val poppedAddress = freshName("f");
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(NamedType("Sp"), oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(-1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), NamedType("Sp")));
    setStackPointer(newStackPointer);

    emit(Load(poppedAddress, LocalReference(pointerType, newTypedPointer)));
    poppedAddress
  }

  def constantMalloc = ConstantGlobal(PointerType(FunctionType(PointerType(I8()), List(I64()))), "malloc");
  def constantFree = ConstantGlobal(PointerType(FunctionType(VoidType(), List(PointerType(I8())))), "free");

}

  /**
   * Extra info in context
   */
  case class ModuleContext() {
    var counter = 0;
    var definitions: List[Definition] = List();
  }

  def emit(definition: Definition)(using C: ModuleContext) = {
    C.definitions = C.definitions :+ definition
  }

  def freshName(name: String)(using C: ModuleContext): String = {
    C.counter = C.counter + 1;
    name + "." + C.counter
  }

  case class FunctionContext() {
    var basicBlocks: List[BasicBlock] = List();
  }

  def emit(basicBlock: BasicBlock)(using C: FunctionContext) = {
    C.basicBlocks = C.basicBlocks :+ basicBlock
  }

  case class BlockContext() {
    var instructions: List[Instruction] = List();
    var stackPointer: String = "sp";
  }

  def emit(instruction: Instruction)(using C: BlockContext) = {
    C.instructions = C.instructions :+ instruction
  }

  def getStackPointer()(using C: BlockContext) = {
    C.stackPointer
  }

  def setStackPointer(stackPointer: String)(using C: BlockContext) = {
      C.stackPointer = stackPointer;
    }

