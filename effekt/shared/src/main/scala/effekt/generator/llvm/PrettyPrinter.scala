package effekt.llvm

import effekt.context.Context

object PrettyPrinter {

  type LLVMString = String

  def show(definitions: List[Definition])(using Context): LLVMString =
    definitions.map(show).mkString("\n\n")

  def show(definition: Definition)(using C: Context): LLVMString = definition match {
    case Function(returnType, name, parameters, basicBlocks) =>
      s"""
define fastcc ${show(returnType)} ${globalName(name)}(${commaSeparated(parameters.map(show))}) {
    ${indentedLines(basicBlocks.map(show).mkString("\n\n"))}
}
"""
    case VerbatimFunction(returnType, name, parameters, body) =>
      // TODO what about calling convention?
      s"""
define ${show(returnType)} ${globalName(name)}(${commaSeparated(parameters.map(show))}) {
    $body
}
"""
    case Verbatim(content) => content

    case GlobalVariableArray(name, IntegerType8(), ConstantArray(IntegerType8(), members)) =>
      val bytes = members.map { ini => ini match {
        case ConstantInteger8(b) => b
        case _ => ???
      }}
      val escaped = bytes.map(b => "\\" + f"$b%02x").mkString;
      s"@$name = private constant [${bytes.length} x i8] c\"$escaped\""

    case GlobalVariableArray(name, typ, initializer) =>
        C.abort(s"cannot compile non-i8 constant array: $name = [ x ${typ}] ${initializer}")
  }

  def show(basicBlock: BasicBlock)(using Context): LLVMString = basicBlock match {
    case BasicBlock(name, instructions, terminator) =>
      s"""
${name}:
${indentedLines(instructions.map(show).mkString("\n"))}
    ${show(terminator)}
"""
  }

  def show(instruction: Instruction)(using C: Context): LLVMString = instruction match {

    case Call(_, VoidType(), ConstantGlobal(_, name), arguments) =>
      s"call void ${globalName(name)}(${commaSeparated(arguments.map(show))})"
    case Call(result, tpe, ConstantGlobal(_, name), arguments) =>
      s"${localName(result)} = call ${show(tpe)} ${globalName(name)}(${commaSeparated(arguments.map(show))})"
    case Call(_, _, nonglobal, _) => C.abort(s"cannot call non-global operand: $nonglobal")

    case TailCall(LocalReference(_, name), arguments) =>
      s"tail call fastcc void ${localName(name)}(${commaSeparated(arguments.map(show))})"
    case TailCall(ConstantGlobal(_, name), arguments) =>
      s"tail call fastcc void ${globalName(name)}(${commaSeparated(arguments.map(show))})"
    case TailCall(nonglobal, _) => C.abort(s"can only tail call references, not: $nonglobal")
    // TODO [jfrech, 2022-07-26] Why does tail call even have a return type if we do not use it?

    case Load(result, LocalReference(PointerType(tpe), name)) =>
      s"${localName(result)} = load ${show(tpe)}, ${show(LocalReference(PointerType(tpe), name))}"
    case Load(_, operand) => C.abort(s"WIP: loading anything but local references not yet implemented: $operand")

    // TODO [jfrech, 2022-07-26] Why does `Load` explicitly check for a local reference and `Store` does not?
    case Store(address, value) =>
      s"store ${show(value)}, ${show(address)}"

    case GetElementPtr(result, LocalReference(PointerType(tpe), name), List(i0)) =>
      s"${localName(result)} = getelementptr ${show(tpe)}, ${show(LocalReference(PointerType(tpe), name))}, i64 $i0"
    case GetElementPtr(result, LocalReference(PointerType(tpe), name), List(i0, i1)) =>
      s"${localName(result)} = getelementptr ${show(tpe)}, ${show(LocalReference(PointerType(tpe), name))}, i64 $i0, i32 $i1"
    case GetElementPtr(_, operand, _) => C.abort(s"can only form a pointer to a local reference, not: $operand")

    case BitCast(result, operand, tpe) =>
      s"${localName(result)} = bitcast ${show(operand)} to ${show(tpe)}"

    case Add(result, operand0, ConstantInt(n)) =>
      s"${localName(result)} = add ${show(operand0)}, $n"
    case Add(_, _, operand1) =>
      C.abort(s"WIP: currently only right-constant additions are supported, not: $operand1")

    case FAdd(result, operand0, ConstantDouble(x)) =>
      s"${localName(result)} = fadd ${show(operand0)}, $x"
    case FAdd(_, _, operand1) =>
      C.abort(s"WIP: currently only right-constant floating-point additions are supported, not: $operand1")

    case InsertValue(result, aggregate, element, index) =>
      s"${localName(result)} = insertvalue ${show(aggregate)}, ${show(element)}, $index"

    case ExtractValue(result, aggregate, index) =>
      s"${localName(result)} = extractvalue ${show(aggregate)}, $index"

    case Comment(msg) =>
      val sanitized = msg.map((c: Char) => if (' ' <= c && c != '\\' && c <= '~') c else '?').mkString
      s"; $sanitized"
  }

  def show(terminator: Terminator): LLVMString = terminator match {
    case RetVoid() =>
      s"ret void"
    case Switch(operand, defaultDest, dests) =>
      def destAsFragment(dest: (Int, String)) = s"i64 ${dest._1}, label ${localName(dest._2)}";
      s"switch ${show(operand)}, label ${localName(defaultDest)} [${spaceSeparated(dests.map(destAsFragment))}]"
    case CondBr(condition, trueDest, falseDest) =>
      s"br ${show(condition)}, label ${localName(trueDest)}, label ${localName(falseDest)}"
  }

  def show(operand: Operand): LLVMString = operand match {
    case LocalReference(tpe, name)          => s"${show(tpe)} ${localName(name)}"
    case ConstantGlobal(tpe, name)          => s"${show(tpe)} ${globalName(name)}"
    case ConstantInt(n)                     => s"i64 $n"
    case ConstantDouble(n)                  => s"double $n"
    case ConstantAggregateZero(tpe)         => s"${show(tpe)} zeroinitializer"
    case ConstantNull(tpe)                  => s"${show(tpe)} null"
    case ConstantArray(memberType, members) => s"[${members.length} x ${show(memberType)}]"
    case ConstantInteger8(b)                => s"i8 $b"
  }

  def show(tpe: Type): LLVMString = tpe match {
    case VoidType() => "void"
    case IntegerType1() => "i1"
    case IntegerType8() => "i8"
    case IntegerType64() => "i64"
    case PointerType(referentType) => s"${show(referentType)}*"
    case ArrayType(size, of) => s"[$size x ${show(of)}]"
    case StructureType(elementTypes) => s"{${commaSeparated(elementTypes.map(show))}}"
    case FunctionType(returnType, argumentTypes) => s"${show(returnType)} (${commaSeparated(argumentTypes.map(show))})"
    case NamedType(name) => localName(name)
  }

  def show(parameter: Parameter): LLVMString = parameter match {
    case Parameter(tpe, name) => s"${show(tpe)} ${localName(name)}"
  }

  def localName(name: String): LLVMString = "%" + name
  def globalName(name: String): LLVMString = "@" + name

  // indent all but the first line with four spaces
  def indentedLines(text: String): String = text.split("\n").map("    " + _).mkString("\n").drop(4)

  def commaSeparated(args: List[String]): String = args.mkString(", ")

  def spaceSeparated(args: List[String]): String = args.mkString(" ")
}
