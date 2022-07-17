package effekt
package llvm

import effekt.machine

/* `LLVMTransformer` transforms Effekt Core into intermediate LLVM */
object LLVMTransformer {

  def transform(program: machine.Program): List[Definition] =
    program match {
      case machine.Program(declarations, statement) =>
        implicit val C = LLVMTransformerContext(LLVMTransformerGlobalContext());

        val terminator = transform(List(), statement);
        val definitions = C.definitions; C.definitions = null;
        val basicBlocks = C.basicBlocks; C.basicBlocks = null;
        val instructions = C.instructions; C.instructions = null;

        val entryInstructions = List(
          Call("env", NamedType("Env"), constantMalloc, List(ConstantInt(1024))),
          Call("sp", NamedType("Sp"), constantMalloc, List(ConstantInt(1024)))
        )

        val entryBlock = BasicBlock("entry", entryInstructions ++ instructions, terminator);
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

  def transform(environment: machine.Environment, statement: machine.Statement)(implicit C: LLVMTransformerContext): Terminator =
    statement match {
      case machine.Def(machine.Label(name, localEnvironment), body, rest) =>

        val () = {
          implicit val C2 = LLVMTransformerContext(C.context);

          loadEnvironment(localEnvironment);
          val terminator = transform(localEnvironment, body);

          val basicBlocks = C2.basicBlocks; C2.basicBlocks = null;
          val instructions = C2.instructions; C2.instructions = null;

          val parameters = List(Parameter(NamedType("Env"), "env"), Parameter(NamedType("Sp"), "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), name, parameters, entryBlock :: basicBlocks);

          emit(function)
        };

        transform(environment, rest)

      case machine.Jump(label) =>

        storeEnvironment(environment);
        emit(TailCall(transform(label), List(LocalReference(NamedType("Env"), "env"), LocalReference(NamedType("Sp"), C.stackPointer))));
        RetVoid()

      case machine.Let(variable @ machine.Variable(name, _), tag, List(), rest) =>
        // TODO non-empty environments
        emit(InsertValue(name, ConstantAggregateZero(StructureType(List(I64(), NamedType("Env")))), ConstantInt(tag), 0))
        transform(variable :: environment, rest)

      case machine.Switch(value, clauses) =>
        val tag = freshName("m");
        emit(ExtractValue(tag, transform(value), 0));
        val labels = clauses.map {
          case machine.Clause(List(), body) =>
            implicit val C2 = LLVMTransformerContext(C.context);

            // TODO read out fields into parameters

            // TODO add parameters to environment
            val terminator = transform(environment, body);

            val basicBlocks = C2.basicBlocks; C2.basicBlocks = null;
            val instructions = C2.instructions; C2.instructions = null;

            // TODO more nested contexts
            basicBlocks.foreach(b => emit(b)(C))

            val label = freshName("l");
            emit(BasicBlock(label, instructions, terminator))(C);
            label
        };
        labels match {
          case Nil =>
            // TODO more informative way to end program. Clean up too?
            RetVoid()
          case label :: labels =>
            Switch(LocalReference(I64(), tag), label, labels.zipWithIndex.map { case (l, i) => (i, l) })
        }

      case machine.Substitute(bindings, rest) =>

        val environment = bindings.map { case (_, value) => value };
        transform(environment, rest)

      case machine.PushFrame(frame, rest) =>

        val frameEnvironment: machine.Environment = machine.freeVariables(frame).toList;

        val frameName = freshName("k");

        val () = {
          implicit val C2 = LLVMTransformerContext(C.context);

          loadEnvironment(frame.parameters);
          popEnvironment(frameEnvironment);
          val terminator = transform(frame.parameters ++ frameEnvironment, frame.body);

          val basicBlocks = C2.basicBlocks; C2.basicBlocks = null;
          val instructions = C2.instructions; C2.instructions = null;

          val parameters = List(Parameter(NamedType("Env"), "env"), Parameter(NamedType("Sp"), "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), frameName, parameters, entryBlock :: basicBlocks);

          emit(function)
        };

        pushEnvironment(frameEnvironment);
        pushReturnAddress(frameName);

        transform(environment, rest)

      case machine.Return(environment) =>

        storeEnvironment(environment);

        val returnAddress = popReturnAddress();
        emit(TailCall(LocalReference(returnAddressType, returnAddress), List(environmentReference, LocalReference(NamedType("Sp"), C.stackPointer))));
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

  def transform(typ: machine.Type): Type = typ match {
    case machine.Primitive("Int") => NamedType("Int")
    case machine.Positive(_)      => StructureType(List(I64(), PointerType(I8())))
  }

  def environmentReference = LocalReference(NamedType("Env"), "env")

  def loadEnvironment(environment: machine.Environment)(implicit C: LLVMTransformerContext): Unit = {
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

  def storeEnvironment(environment: machine.Environment)(implicit C: LLVMTransformerContext): Unit = {
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

  def pushEnvironment(environment: machine.Environment)(implicit C: LLVMTransformerContext): Unit = {
    // TODO do nothing if empty

    val environmentType = StructureType(environment.map { case machine.Variable(_, typ) => transform(typ) });

    val pointerType = PointerType(environmentType);
    val oldStackPointer = C.stackPointer;
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(NamedType("Sp"), oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), NamedType("Sp")));
    C.stackPointer = newStackPointer;

    environment.zipWithIndex.foreach {
      case (value @ machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(PointerType(fieldType), oldTypedPointer), List(0, i)));
        emit(Store(LocalReference(PointerType(fieldType), fieldPointerName), transform(value)))
    }

  }

  def popEnvironment(environment: machine.Environment)(implicit C: LLVMTransformerContext): Unit = {
    // TODO do nothing if empty

    val environmentType = StructureType(environment.map { case machine.Variable(_, typ) => transform(typ) });

    val pointerType = PointerType(environmentType);
    val oldStackPointer = C.stackPointer;
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(NamedType("Sp"), oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(-1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), NamedType("Sp")));
    C.stackPointer = newStackPointer;

    environment.zipWithIndex.foreach {
      case (value @ machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(PointerType(fieldType), newTypedPointer), List(0, i)));
        emit(Load(name, LocalReference(PointerType(fieldType), fieldPointerName)))
    }

  }

  def returnAddressType = PointerType(FunctionType(VoidType(), List(NamedType("Env"), NamedType("Sp"))))

  def pushReturnAddress(frameName: String)(implicit C: LLVMTransformerContext): Unit = {
    val pointerType = PointerType(returnAddressType);
    val oldStackPointer = C.stackPointer;
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(NamedType("Sp"), oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), NamedType("Sp")));
    C.stackPointer = newStackPointer;
    emit(Store(LocalReference(pointerType, oldTypedPointer), ConstantGlobal(returnAddressType, frameName)));
  }

  def popReturnAddress()(implicit C: LLVMTransformerContext): String = {
    val pointerType = PointerType(returnAddressType);
    val poppedAddress = freshName("f");
    val oldStackPointer = C.stackPointer;
    val newStackPointer = freshName("sp");
    val oldTypedPointer = freshName("sp.t");
    val newTypedPointer = freshName("sp.t");
    emit(BitCast(oldTypedPointer, LocalReference(NamedType("Sp"), oldStackPointer), pointerType));
    emit(GetElementPtr(newTypedPointer, LocalReference(pointerType, oldTypedPointer), List(-1)));
    emit(BitCast(newStackPointer, LocalReference(pointerType, newTypedPointer), NamedType("Sp")));
    C.stackPointer = newStackPointer;
    emit(Load(poppedAddress, LocalReference(pointerType, newTypedPointer)));
    poppedAddress
  }

  def constantMalloc = ConstantGlobal(PointerType(FunctionType(PointerType(I8()), List(I64()))), "malloc");

  /**
   * Extra info in context
   */
  case class LLVMTransformerGlobalContext() {
    var counter = 0;
    var definitions: List[Definition] = List();
  }

  case class LLVMTransformerContext(context: LLVMTransformerGlobalContext) {
    var basicBlocks: List[BasicBlock] = List();
    var instructions: List[Instruction] = List();
    var stackPointer: String = "sp";
  }

  def emit(instruction: Instruction)(implicit C: LLVMTransformerContext) = {
    C.instructions = C.instructions :+ instruction
  }

  def emit(basicBlock: BasicBlock)(C: LLVMTransformerContext) = {
    C.basicBlocks = C.basicBlocks :+ basicBlock
  }

  def emit(definition: Definition)(implicit C: LLVMTransformerContext) = {
    C.definitions = C.definitions :+ definition
  }

  def freshName(name: String)(implicit C: LLVMTransformerContext): String = {
    C.counter = C.counter + 1;
    name + "." + C.counter
  }

  private implicit def asContext(C: LLVMTransformerContext): LLVMTransformerGlobalContext = C.context
  private implicit def getContext(implicit C: LLVMTransformerContext): LLVMTransformerGlobalContext = C.context
}
