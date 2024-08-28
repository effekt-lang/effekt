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

    // !100 = !{!"frame_100", !13, i64 0, !13, i64 8}
    case TypeDescriptor(id: Int, typeName: String, structure: List[(TBAA, Int)]) =>
      val fields = structure match {
        case Nil => List("!0, i64 0") // TODO does this make sense at all?!
        case structure => structure.map { case (tbaa, offset) => s"${show(tbaa)}, i64 ${offset}" }
      }
      s"""!${id} = !{!"${typeName}", ${fields.mkString(", ")} }"""

    // !25 = !{!99, !13, i64 8}
    case AccessTag(id: Int, base: Int, access: TBAA, offset: Int) =>
      s"""!${id} = !{ !${base}, ${show(access)}, i64 ${offset} }"""
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

    case Load(result, tpe, LocalReference(PointerType(), name), Some(tbaa)) =>
      s"${localName(result)} = load ${show(tpe)}, ${show(LocalReference(PointerType(), name))}, !tbaa ${show(tbaa)}"

    case Load(result, tpe, LocalReference(PointerType(), name), None) =>
      s"${localName(result)} = load ${show(tpe)}, ${show(LocalReference(PointerType(), name))}"

    case Load(_, _, operand, _) => C.abort(s"WIP: loading anything but local references not yet implemented: $operand")

    // TODO [jfrech, 2022-07-26] Why does `Load` explicitly check for a local reference and `Store` does not?
    case Store(address, value, Some(tbaa)) =>
      s"store ${show(value)}, ${show(address)}, !tbaa ${show(tbaa)}"

    case Store(address, value, None) =>
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

  def show(tbaa: TBAA): String = tbaa match {
    case TBAA.Stack() => "!1"
    case TBAA.StackPointer() => "!2"
    case TBAA.ReferenceCount() => "!3"
    case TBAA.Memory() => "!8"
    case TBAA.Region() => "!8"
    case TBAA.FrameAccess(n, idx) => s"!${n + 1 + idx}"

    case TBAA.Byte() => "!10"
    case TBAA.Double() => "!12"
    case TBAA.Int() => "!13"

    case TBAA.String() => "!14"
    case TBAA.Pos() => "!15"
    case TBAA.Neg() => "!16"
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
