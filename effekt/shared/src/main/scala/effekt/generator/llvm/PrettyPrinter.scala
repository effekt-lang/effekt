package effekt
package generator
package llvm

import effekt.context.Context

object PrettyPrinter {

  type LLVMString = String

  def show(definitions: List[Definition])(using Context): LLVMString =
    definitions.map(show).mkString("\n\n")

  def show(definition: Definition)(using C: Context): LLVMString = definition match {
    case Function(callingConvention, returnType, name, parameters, basicBlocks) =>
      val privateOrNot = if name.contains("effektMain") then "" else "private "
      s"""
define ${privateOrNot} ${show(callingConvention)} ${showReturnType(returnType)} ${globalName(name)}(${commaSeparated(parameters.map(show))}) #0 {
    ${indentedLines(basicBlocks.map(show).mkString)}
}
"""
    case VerbatimFunction(returnType, name, parameters, body) =>
      // TODO what about calling convention?
      // TODO annotated as readonly, though this might not always be true...
      s"""
define private ${showReturnType(returnType)} ${globalName(name)}(${commaSeparated(parameters.map(show))}) #1 {
    $body
}
"""
    case Verbatim(content) => content

    case GlobalConstant(name, ConstantArray(IntegerType8(), members)) =>
      val bytes = members.map { ini => ini match {
        case ConstantInteger8(b) => b
        case _ => ???
      }}
      val escaped = bytes.map(b => "\\" + f"$b%02x").mkString;
      s"@$name = private constant [${bytes.length} x i8] c\"$escaped\""

    case GlobalConstant(name, initializer) =>
      s"@$name = private constant ${show(initializer)}"
  }

  def showReturnType(returnType: Type): LLVMString = returnType match {
      case PointerType() => s"noalias nonnull ${show(returnType)}"
      case _ => show(returnType)
    }

  def show(callingConvention: CallingConvention): LLVMString = callingConvention match {
    case Ccc() => "ccc"
    case Tailcc() => "tailcc"
  }

  def show(basicBlock: BasicBlock)(using Context): LLVMString = basicBlock match {
    case BasicBlock(name, instructions, terminator) =>
      s"""
${name}:
${indentedLines(instructions.map(show).mkString("\n"))}
    ${show(terminator)}
"""
  }

  def tbaa(tpe: String): String = tpe match {
    case "Stack" => ", !tbaa !1"
    case "StackPointer" => ", !tbaa !2"
    case "ReferenceCount" => ", !tbaa !3"
    case "Eraser" => ", !tbaa !4"
    case "Sharer" => ", !tbaa !5"

    case "Char" => ", !tbaa !10"
    case "Float" => ", !tbaa !11"
    case "Double" => ", !tbaa !12"
    case "Int" => ", !tbaa !13"
    case "String" => ", !tbaa !14"
    case "Pos" => ", !tbaa !15"
    case "Neg" => ", !tbaa !16"

    case _ => ""
  }

  def show(instruction: Instruction)(using C: Context): LLVMString = instruction match {

    case Call(_, Ccc(), VoidType(), ConstantGlobal(_, name), arguments) =>
      s"call ccc void ${globalName(name)}(${commaSeparated(arguments.map(showArgument))})"
    case Call(result, Ccc(), tpe, ConstantGlobal(_, name), arguments) =>
      s"${localName(result)} = call ccc ${show(tpe)} ${globalName(name)}(${commaSeparated(arguments.map(showArgument))})"
    case Call(_, Ccc(), _, nonglobal, _) =>
      C.abort(s"cannot call non-global operand: $nonglobal")
    case Call(_, Tailcc(), VoidType(), ConstantGlobal(_, name), arguments) =>
      s"musttail call tailcc void ${globalName(name)}(${commaSeparated(arguments.map(showArgument))})"
    case Call(_, Tailcc(), VoidType(), LocalReference(_, name), arguments) =>
      s"musttail call tailcc void ${localName(name)}(${commaSeparated(arguments.map(showArgument))})"
    case Call(_, Tailcc(), tpe, _, _) =>
      C.abort(s"tail call to non-void function returning: $tpe")
    case Load(result, NamedType(tpe), LocalReference(PointerType(), name)) =>
      s"${localName(result)} = load ${localName(tpe)}, ${show(LocalReference(PointerType(), name))}${tbaa(tpe)}"

    case Load(result, tpe, LocalReference(PointerType(), name)) =>
      s"${localName(result)} = load ${show(tpe)}, ${show(LocalReference(PointerType(), name))}"

    case Load(_, _, operand) => C.abort(s"WIP: loading anything but local references not yet implemented: $operand")

    // TODO [jfrech, 2022-07-26] Why does `Load` explicitly check for a local reference and `Store` does not?
    case Store(address @ Operand.LocalReference(NamedType(tpe), name), value) =>
      s"store ${show(value)}, ${show(address)}${tbaa(tpe)}"

    case Store(address, value) =>
      s"store ${show(value)}, ${show(address)}"

    case GetElementPtr(result, tpe, ptr @ LocalReference(_, name), i :: is) =>
      s"${localName(result)} = getelementptr ${show(tpe)}, ${show(ptr)}, i64 $i" + is.map(", i32 " + _).mkString
    case GetElementPtr(_, _, operand, _) => C.abort(s"can only form a pointer to a local reference, not: $operand")

    case BitCast(result, operand, tpe) =>
      s"${localName(result)} = bitcast ${show(operand)} to ${show(tpe)}"

    case Add(result, operand0, ConstantInt(n)) =>
      s"${localName(result)} = add ${show(operand0)}, $n"
    case Add(result, LocalReference(t1, n1), LocalReference(t2, n2)) =>
      assert(t1 == t2)
      s"${localName(result)} = add ${show(t1)} ${localName(n1)}, ${localName(n2)}"
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

    case Comment(msg) if C.config.debug() =>
      val sanitized = msg.map((c: Char) => if (' ' <= c && c != '\\' && c <= '~') c else '?').mkString
      s"\n; $sanitized"

    case Comment(msg) => ""
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
    case ConstantArray(memberType, members) => s"[${members.length} x ${show(memberType)}] [${commaSeparated(members.map(show))}]"
    case ConstantInteger8(b)                => s"i8 $b"
  }

  def showArgument(operand: Operand): LLVMString = operand match {
    case LocalReference(tpe @ Type.PointerType(), name) => s"${show(tpe)} nonnull ${localName(name)}"
    case LocalReference(tpe, name)          => s"${show(tpe)} ${localName(name)}"
    case ConstantGlobal(tpe, name)          => s"${show(tpe)} ${globalName(name)}"
    case ConstantInt(n)                     => s"i64 $n"
    case ConstantDouble(n)                  => s"double $n"
    case ConstantAggregateZero(tpe)         => s"${show(tpe)} zeroinitializer"
    case ConstantNull(tpe)                  => s"${show(tpe)} null"
    case ConstantArray(memberType, members) => s"[${members.length} x ${show(memberType)}] [${commaSeparated(members.map(show))}]"
    case ConstantInteger8(b)                => s"i8 $b"
  }

  def show(tpe: Type): LLVMString = tpe match {
    case VoidType() => "void"
    case IntegerType1() => "i1"
    case IntegerType8() => "i8"
    case IntegerType64() => "i64"
    case DoubleType() => "double"
    case PointerType() => "ptr"
    case ArrayType(size, of) => s"[$size x ${show(of)}]"
    case StructureType(elementTypes) => s"{${commaSeparated(elementTypes.map(show))}}"
    case FunctionType(returnType, argumentTypes) => s"${show(returnType)} (${commaSeparated(argumentTypes.map(show))})"
    case NamedType(name) => localName(name)
  }

  def show(parameter: Parameter): LLVMString = parameter match {
    case Parameter(tpe, "stack") => s"${show(tpe)} noalias nonnull ${localName("stack")}"
    case Parameter(tpe, "environment") => s"${show(tpe)} noalias nonnull ${localName("environment")}"
    case Parameter(tpe, "obj") => s"${show(tpe)} noalias nonnull ${localName("obj")}"
    case Parameter(tpe, "stackPointer") => s"${show(tpe)} noalias nonnull ${localName("stackPointer")}"
    case Parameter(tpe @ Type.PointerType(), name) => s"${show(tpe)} nonnull ${localName(name)}"
    case Parameter(tpe, name) => s"${show(tpe)} ${localName(name)}"
  }

  def localName(name: String): LLVMString = "%" + name
  def globalName(name: String): LLVMString = "@" + name

  // indent all lines with four spaces
  def indentedLines(text: String): String = text.split("\n").map("    " + _).mkString("\n")

  def commaSeparated(args: List[String]): String = args.mkString(", ")

  def spaceSeparated(args: List[String]): String = args.mkString(" ")
}
