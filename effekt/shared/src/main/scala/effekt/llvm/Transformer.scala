package effekt
package llvm

import scala.collection.mutable
import effekt.machine.FreshValueSymbol
import effekt.symbols.{ BlockSymbol, Module, Name, Symbol, ValueSymbol, builtins }
import effekt.util.{ Task, control }
import effekt.util.control._

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

        val entryBlock = BasicBlock("entry", instructions, terminator);
        val entryFunction = Function(VoidType(), "effektMain", List(), entryBlock :: basicBlocks);
        declarations.map(transform) ++ definitions :+ entryFunction
    }

  def transform(declaration: machine.Declaration): Definition =
    declaration match {
      case machine.Foreign(returnType, functionName, parameters, body) =>
        VerbatimFunction(transform(returnType), functionName, parameters.map(transformParameter), body)
      case machine.Include(content) =>
        Verbatim(content)
    }

  def transform(environment: machine.Environment, statement: machine.Statement)(implicit C: LLVMTransformerContext): Terminator =
    statement match {
      case machine.Def(machine.Label(name, environment), body, rest) =>

        val definitions = {
          implicit val C = LLVMTransformerContext();

          loadEnvironment(environment);
          val terminator = transform(environment, body);

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

        val environment = bindings.map { case (_, x) => x };
        transform(environment, rest)

      case machine.Run(machine.Return(), environment, List()) =>

        storeEnvironment(environment);

        val returnAddress = popReturnAddress();
        emit(TailCall(LocalReference(returnAddressType, returnAddress), List(environmentReference, LocalReference(NamedType("Sp"), C.stackPointer))));
        RetVoid()
      case machine.Run(machine.LiteralInt(n), List(), List(machine.Clause(List(variable @ machine.Variable(name, machine.Primitive("Int"))), rest))) =>
        emit(Add(name, ConstantNumber(n), ConstantNumber(0)));
        transform(variable :: environment, rest)
      case machine.Run(machine.Panic(), List(), List()) =>
        // emit(TailCall(ConstantGlobal(???, "panic"), List()));
        RetVoid()
    }

  def transform(label: machine.Label): ConstantGlobal =
    label match {
      case machine.Label(name, typ) => ConstantGlobal(PointerType(FunctionType(VoidType(), List(NamedType("Env"), NamedType("Sp")))), name)
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
    val environmentPointerName = freshName("env.t.");
    emit(BitCast(environmentPointerName, environmentReference, PointerType(environmentType)));

    environment.zipWithIndex.foreach {
      case (machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(PointerType(environmentType), environmentPointerName), List(0, i)));
        emit(Load(name, LocalReference(PointerType(fieldType), fieldPointerName)))
    }
  }

  def storeEnvironment(environment: machine.Environment)(implicit C: LLVMTransformerContext): Unit = {

    val environmentType = StructureType(environment.map { case machine.Variable(_, typ) => transform(typ) });
    val environmentPointerName = freshName("env.t.");
    emit(BitCast(environmentPointerName, environmentReference, PointerType(environmentType)));

    environment.zipWithIndex.foreach {
      case (machine.Variable(name, typ), i) =>
        val fieldType = transform(typ);
        val fieldPointerName = freshName(name + "p");
        emit(GetElementPtr(fieldPointerName, LocalReference(PointerType(environmentType), environmentPointerName), List(0, i)));
        emit(Store(LocalReference(PointerType(fieldType), fieldPointerName), LocalReference(fieldType, name)))
    }
  }

  def returnAddressType = PointerType(FunctionType(VoidType(), List(NamedType("Env"), NamedType("Sp"))))

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

  /**
   * Extra info in context
   */

  // TODO do we need this context? why, we shouldn't
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
