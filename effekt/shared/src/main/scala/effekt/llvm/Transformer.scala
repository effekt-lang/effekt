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
        emit(Call("env", envType, constantMalloc, List(ConstantInt(1024))));
        emit(Call("sp", spType, constantMalloc, List(ConstantInt(1024))));
        pushReturnAddress("topLevel");

        val terminator = transform(statement);

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

  def transform(statement: machine.Statement)(using ModuleContext, FunctionContext, BlockContext): Terminator =
    statement match {
      case machine.Def(machine.Label(name, environment), body, rest) =>

        val () = {
          implicit val FC = FunctionContext();
          implicit val BC = BlockContext();

          loadEnvironment(initialEnvironmentReference, environment);
          val terminator = transform(body);

          val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
          val instructions = BC.instructions; BC.instructions = null;

          val parameters = List(Parameter(envType, "env"), Parameter(spType, "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), name, parameters, entryBlock :: basicBlocks);

          emit(function)
        };

        transform(rest)

      case machine.Jump(label) =>

        storeEnvironment(initialEnvironmentReference, label.environment);

        emit(TailCall(transform(label), List(LocalReference(envType, "env"), LocalReference(spType, getStackPointer()))));
        RetVoid()

      case machine.Substitute(bindings, rest) =>

        withBindings(bindings) { () =>
          transform(rest)
        }

      case machine.Let(variable, tag, values, rest) =>
        // TODO do nothing if values is empty
        val objName = freshName("obj");
        emit(Call(objName, envType, constantMalloc, List(ConstantInt(environmentSize(values)))));
        storeEnvironment(LocalReference(envType, objName), values);

        val tmpName = freshName("tmp");
        emit(InsertValue(tmpName, ConstantAggregateZero(positiveType), ConstantInt(tag), 0));
        emit(InsertValue(variable.name, LocalReference(positiveType, tmpName), LocalReference(envType, objName), 1));

        transform(rest)

      case machine.Switch(value, clauses) =>

        val tagName = freshName("tag");
        val objName = freshName("obj");
        emit(ExtractValue(tagName, transform(value), 0));
        emit(ExtractValue(objName, transform(value), 1));
        val labels = clauses.map {
          case machine.Clause(parameters, body) =>
            implicit val BC = BlockContext();

            loadEnvironment(LocalReference(envType, objName), parameters);
            emit(Call("_", VoidType(), constantFree, List(LocalReference(envType, objName))));

            val terminator = transform(body);

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
            Switch(LocalReference(IntegerType64(), tagName), label, labels.zipWithIndex.map { case (l, i) => (i, l) })
        }

      case machine.New(variable, List(clause), rest) =>
        // TODO multiple methods

        val closureEnvironment = machine.freeVariables(clause).toList;

        val clauseName = freshName(variable.name);

        val () = {
          implicit val FC = FunctionContext();
          implicit val BC = BlockContext();

          val objName = "obj";
          loadEnvironment(LocalReference(envType, objName), closureEnvironment);
          emit(Call("_", VoidType(), constantFree, List(LocalReference(envType, objName))));
          loadEnvironment(initialEnvironmentReference, clause.parameters);

          val terminator = transform(clause.body);

          val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
          val instructions = BC.instructions; BC.instructions = null;

          val parameters = List(Parameter(envType, objName), Parameter(envType, "env"), Parameter(spType, "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), clauseName, parameters, entryBlock :: basicBlocks);

          emit(function)
        };

        // TODO do nothing if closure environment is empty
        val objName = freshName("obj");
        emit(Call(objName, envType, constantMalloc, List(ConstantInt(environmentSize(closureEnvironment)))));
        storeEnvironment(LocalReference(envType, objName), closureEnvironment);

        val tmpName = freshName("tmp");
        val clauseType = PointerType(FunctionType(VoidType(), List(envType, envType, spType)));
        emit(InsertValue(tmpName, ConstantAggregateZero(negativeType), ConstantGlobal(clauseType, clauseName), 0));
        emit(InsertValue(variable.name, LocalReference(negativeType, tmpName), LocalReference(envType, objName), 1));

        transform(rest)

      case machine.Invoke(value, 0, values) =>

        storeEnvironment(initialEnvironmentReference, values);

        val functionName = freshName("fp");
        val objName = freshName("obj");
        val clauseType = PointerType(FunctionType(VoidType(), List(envType, envType, spType)));

        emit(ExtractValue(functionName, transform(value), 0));
        emit(ExtractValue(objName, transform(value), 1));
        emit(TailCall(LocalReference(clauseType, functionName), List(LocalReference(envType, objName), initialEnvironmentReference, LocalReference(spType, getStackPointer()))));
        RetVoid()


      case machine.PushFrame(frame, rest) =>

        val frameEnvironment = machine.freeVariables(frame).toList;

        val frameName = freshName("k");

        val () = {
          implicit val FC = FunctionContext();
          implicit val BC = BlockContext();

          popEnvironment(frameEnvironment);
          loadEnvironment(initialEnvironmentReference, frame.parameters);
          val terminator = transform(frame.body);

          val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
          val instructions = BC.instructions; BC.instructions = null;

          val parameters = List(Parameter(envType, "env"), Parameter(spType, "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), frameName, parameters, entryBlock :: basicBlocks);

          emit(function)
        };

        pushEnvironment(frameEnvironment);
        pushReturnAddress(frameName);

        transform(rest)

      case machine.Return(values) =>

        storeEnvironment(initialEnvironmentReference, values);

        val returnAddress = popReturnAddress();
        emit(TailCall(LocalReference(returnAddressType, returnAddress), List(initialEnvironmentReference, LocalReference(spType, getStackPointer()))));
        RetVoid()

      case machine.Run(machine.LiteralInt(n), List(), List(machine.Clause(List(machine.Variable(name, machine.Primitive("Int"))), rest))) =>
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.Run(machine.CallForeign(name), values, List()) =>
        // TODO careful with calling convention?!?
        val functionType = PointerType(FunctionType(VoidType(), values.map { case machine.Variable(_, tpe) => transform(tpe) }));
        emit(Call("_", VoidType(), ConstantGlobal(functionType, name), values.map(transform)));
        RetVoid()

      case machine.Run(machine.CallForeign(name), values, List(machine.Clause(List(machine.Variable(resultName, resultType)), rest))) =>
        // TODO careful with calling convention?!?
        val functionType = PointerType(FunctionType(transform(resultType), values.map { case machine.Variable(_, tpe) => transform(tpe) }));
        emit(Call(resultName, transform(resultType), ConstantGlobal(functionType, name), values.map(transform)));
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

  def transformParameter(variable: machine.Variable): Parameter =
    variable match {
      case machine.Variable(name, tpe) => Parameter(transform(tpe), name)
    }

  def positiveType: Type = StructureType(List(IntegerType64(), envType));
  // TODO multiple methods (should be pointer to vtable)
  def negativeType: Type = StructureType(List(methodType, envType));
  def methodType: Type = PointerType(FunctionType(VoidType(), List(envType, envType, spType)));
  def envType: Type = NamedType("Env");
  def spType: Type = NamedType("Sp");

  def transform(tpe: machine.Type): Type = tpe match {
    case machine.Positive(_)      => positiveType
    case machine.Negative(_)      => negativeType
    case machine.Primitive("Int") => NamedType("Int")
  }

  def environmentSize(environment: machine.Environment): Int =
    environment.map { case machine.Variable(_, typ) => typeSize(typ) }.sum

  def typeSize(tpe: machine.Type): Int =
    tpe match {
      case machine.Positive(_)      => 16
      case machine.Negative(_)      => 16
      case machine.Primitive("Int") => 8
    }

  def initialEnvironmentReference = LocalReference(envType, "env")

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
    emit(BitCast(oldTypedPointer, LocalReference(spType, oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), spType));
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
    emit(BitCast(oldTypedPointer, LocalReference(spType, oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(-1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), spType));
    setStackPointer(newStackPointer);

    environment.zipWithIndex.foreach {
      case (value @ machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(pointerType, newTypedPointer), List(0, i)));
        emit(Load(name, LocalReference(PointerType(fieldType), fieldPointerName)))
    }

  }

  def returnAddressType = PointerType(FunctionType(VoidType(), List(envType, spType)))

  def pushReturnAddress(frameName: String)(using ModuleContext, FunctionContext, BlockContext): Unit = {

    val oldStackPointer = getStackPointer();
    val pointerType = PointerType(returnAddressType);
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(spType, oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), spType));
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
    emit(BitCast(oldTypedPointer, LocalReference(spType, oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(-1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), spType));
    setStackPointer(newStackPointer);

    emit(Load(poppedAddress, LocalReference(pointerType, newTypedPointer)));
    poppedAddress
  }

  def constantMalloc = ConstantGlobal(PointerType(FunctionType(PointerType(IntegerType8()), List(IntegerType64()))), "malloc");
  def constantFree = ConstantGlobal(PointerType(FunctionType(VoidType(), List(PointerType(IntegerType8())))), "free");

}

  /**
   * Extra info in context
   */
  class ModuleContext() {
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

  class FunctionContext() {
    var substitution: Map[machine.Variable, machine.Variable] = Map();
    var basicBlocks: List[BasicBlock] = List();
  }

  def emit(basicBlock: BasicBlock)(using C: FunctionContext) = {
    C.basicBlocks = C.basicBlocks :+ basicBlock
  }

  def withBindings[R](bindings: List[(machine.Variable, machine.Variable)])(prog: () => R)(using C: FunctionContext): R = {
    val substitution = C.substitution;
    C.substitution = substitution ++ bindings.map { case (variable -> value) => (variable -> substitution.getOrElse(value, value) ) }.toMap;
    val result = prog();
    C.substitution = substitution
    result
  }

  def substitute(value: machine.Variable)(using C: FunctionContext): machine.Variable = {
    C.substitution.toMap.getOrElse(value, value)
  }

  class BlockContext() {
    var stackPointer: String = "sp";
    var instructions: List[Instruction] = List();
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

