package effekt.llvm

// Sane names are important, since unescaping string interpolation is used
// to construct `LLVMFragment`s. As such, great care should be taken as to
// not let user-controlled name data leak into LLVM source.
val SANE_NAME_REGEX = """^[a-zA-Z_$][a-zA-Z0-9_$]*$""".r

// after de-kiama-ing, everything will have been dissolved into strings
type LLVMFragment = String

// indent all but the first line with four spaces
def indentedLines(text: String): String = text.split("\n").map("    " + _).mkString("\n").drop(4)

def commaSeparated(args: List[String]): String = args.mkString(", ")
def spaceSeparated(args: List[String]): String = args.mkString(" ")

object LLVMFragmentPrinter {

  def asFragment(definition: Definition): LLVMFragment = definition match {
    case Function(returnType, name, parameters, basicBlocks) =>
      s"""
define fastcc ${asFragment(returnType)} ${globalName(name)}(${commaSeparated(parameters.map(asFragment))}) {
    ${indentedLines(basicBlocks.map(asFragment).mkString("\n\n"))}
}
"""
    case VerbatimFunction(_, _, _, _) => ???
    case Verbatim(content) => content
  }

  def asFragment(basicBlock: BasicBlock): LLVMFragment = basicBlock match {
    case BasicBlock(name, instructions, terminator) =>
      s"""
${name}:
${instructions.map(asFragment).mkString("\n")}
${asFragment(terminator)}
"""
  }

  def asFragment(instruction: Instruction): LLVMFragment = instruction match {
    case Call(_, VoidType(), ConstantGlobal(_, name), arguments) =>
       s"call void ${globalName(name)}(${commaSeparated(arguments.map(asFragment))})"
    case Call(result, typ, ConstantGlobal(_, name), arguments) =>
       s"${localName(result)} = call ${asFragment(typ)} ${globalName(name)}(${commaSeparated(arguments.map(asFragment))})"
    case TailCall(LocalReference(_, name), arguments) =>
      s"tail call fastcc void ${localName(name)}(${commaSeparated(arguments.map(asFragment))})"
    case TailCall(ConstantGlobal(_, name), arguments) =>
      s"tail call fastcc void ${globalName(name)}(${commaSeparated(arguments.map(asFragment))})"
    case BitCast(result, operand, typ) =>
      s"${localName(result)} = bitcast ${asFragment(operand)} to ${asFragment(typ)}"
    case GetElementPtr(result, LocalReference(PointerType(typ), name), List(i0)) =>
      s"${localName(result)} = getelementptr ${asFragment(typ)}, ${asFragment(LocalReference(PointerType(typ), name))}, i64 $i0"
    case GetElementPtr(result, LocalReference(PointerType(typ), name), List(i0, i1)) =>
      s"${localName(result)} = getelementptr ${asFragment(typ)}, ${asFragment(LocalReference(PointerType(typ), name))}, i64 $i0, i32 $i1"
    case Load(result, LocalReference(PointerType(typ), name)) =>
      s"${localName(result)} = load ${asFragment(typ)}, ${asFragment(LocalReference(PointerType(typ), name))}"
    case Store(address, value) =>
      s"store ${asFragment(value)}, ${asFragment(address)}"
    case Add(result, operand0, ConstantNumber(n)) =>
      s"${localName(result)} = add ${asFragment(operand0)}, $n"

  }
  def asFragment(terminator: Terminator): LLVMFragment = terminator match {
    case RetVoid() =>
      s"""
ret void
"""
  }

  def asFragment(operand: Operand): LLVMFragment = operand match {
    case LocalReference(typ, name) => s"${asFragment(typ)} ${localName(name)}"
    case ConstantGlobal(typ, name) => s"${asFragment(typ)} ${globalName(name)}"
    case ConstantNumber(n) => s"i64 $n"
  }

  def asFragment(typ: Type): LLVMFragment = typ match {
    case VoidType() => "void"
    case I64() => "i64"
    case I8() => "i8"
    case NamedType(name) => localName(name)
    case PointerType(referentType) => s"${asFragment(referentType)}*"
    case StructureType(elementTypes) => s"{${commaSeparated(elementTypes.map(asFragment))}}"
    case FunctionType(returnType, argumentTypes) => s"${asFragment(returnType)} (${commaSeparated(argumentTypes.map(asFragment))})"
  }

  def asFragment(parameter: Parameter): LLVMFragment = parameter match {
    case Parameter(typ, name) => s"${asFragment(typ)} ${localName(name)}"
  }

  def localName(name: String): LLVMFragment =
    "%" + name

  def globalName(name: String): LLVMFragment =
    "@" + name
}

