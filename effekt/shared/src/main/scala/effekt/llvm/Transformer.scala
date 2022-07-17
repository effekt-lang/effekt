package effekt
package llvm

import effekt.machine

/* `LLVMTransformer` transforms Effekt Core into intermediate LLVM */
object LLVMTransformer {

  def transform(program: machine.Program): List[Definition] =
    program match {
      case machine.Program(declarations, statement) =>
        implicit val C = LLVMTransformerContext();

        val terminator = transform(List(), statement);
        val definitions = C.definitions; C.definitions = null;
        val basicBlocks = C.basicBlocks; C.basicBlocks = null;
        val instructions = C.instructions; C.instructions = null;

        val entryInstructions = List(
          Call("env", NamedType("Env"), constantMalloc, List(ConstantNumber(1024))),
          Call("sp", NamedType("Sp"), constantMalloc, List(ConstantNumber(1024)))
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

        val definitions = {
          implicit val C = LLVMTransformerContext();

          loadEnvironment(localEnvironment);
          val terminator = transform(localEnvironment, body);

          val definitions = C.definitions; C.definitions = null;
          val basicBlocks = C.basicBlocks; C.basicBlocks = null;
          val instructions = C.instructions; C.instructions = null;

          val parameters = List(Parameter(NamedType("Env"), "env"), Parameter(NamedType("Sp"), "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), name, parameters, entryBlock :: basicBlocks);

          function :: definitions
        };
        definitions.foreach(emit);

        transform(environment, rest)

      case machine.Jump(label) =>

        storeEnvironment(environment);
        emit(TailCall(transform(label), List(LocalReference(NamedType("Env"), "env"), LocalReference(NamedType("Sp"), C.stackPointer))));
        RetVoid()

      case machine.Substitute(bindings, rest) =>

        val environment = bindings.map { case (_, value) => value };
        transform(environment, rest)

      case machine.PushFrame(frame, rest) =>

        val frameEnvironment: machine.Environment = machine.freeVariables(frame).toList;

        val frameName = freshName("k");
        // TODO prefix this name with function name

        val definitions = {
          implicit val C = LLVMTransformerContext();

          loadEnvironment(frame.parameters);
          popEnvironment(frameEnvironment);
          val terminator = transform(frame.parameters ++ frameEnvironment, frame.body);

          val definitions = C.definitions; C.definitions = null;
          val basicBlocks = C.basicBlocks; C.basicBlocks = null;
          val instructions = C.instructions; C.instructions = null;

          val parameters = List(Parameter(NamedType("Env"), "env"), Parameter(NamedType("Sp"), "sp"));
          val entryBlock = BasicBlock("entry", instructions, terminator);
          val function = Function(VoidType(), frameName, parameters, entryBlock :: basicBlocks);

          function :: definitions
        };
        definitions.foreach(emit);

        pushEnvironment(frameEnvironment);
        pushReturnAddress(frameName);

        transform(environment, rest)

      case machine.Return(environment) =>

        storeEnvironment(environment);

        val returnAddress = popReturnAddress();
        emit(TailCall(LocalReference(returnAddressType, returnAddress), List(environmentReference, LocalReference(NamedType("Sp"), C.stackPointer))));
        RetVoid()

      case machine.Run(machine.LiteralInt(n), List(), List(machine.Clause(List(parameter @ machine.Variable(name, machine.Primitive("Int"))), rest))) =>

        emit(Add(name, ConstantNumber(n), ConstantNumber(0)));
        transform(parameter :: environment, rest)

      case machine.Run(machine.CallForeign(name), values, List()) =>
        // TODO careful with calling convention?!?
        val functionType = PointerType(FunctionType(VoidType(), values.map { case machine.Variable(_, typ) => transform(typ) }));
        emit(Call("_", VoidType(), ConstantGlobal(functionType, name), values.map(transform)));
        RetVoid()

      case machine.Run(machine.CallForeign(name), values, List(machine.Clause(List(parameter @ machine.Variable(resultName, resultType)), rest))) =>
        // TODO careful with calling convention?!?
        val functionType = PointerType(FunctionType(transform(resultType), values.map { case machine.Variable(_, typ) => transform(typ) }));
        emit(Call(resultName, transform(resultType), ConstantGlobal(functionType, name), values.map(transform)));
        transform(parameter :: environment, rest)
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

  case class LLVMTransformerContext() {
    var counter = 0;

    var definitions: List[Definition] = List();
    var basicBlocks: List[BasicBlock] = List();
    var instructions: List[Instruction] = List();

    var stackPointer: String = "sp";
  }

  def emit(instruction: Instruction)(implicit C: LLVMTransformerContext) = {
    C.instructions = C.instructions :+ instruction
  }

  def emit(definition: Definition)(implicit C: LLVMTransformerContext) = {
    C.definitions = C.definitions :+ definition
  }

  def freshName(name: String)(implicit C: LLVMTransformerContext): String = {
    C.counter = C.counter + 1;
    name + "." + C.counter
  }

}
