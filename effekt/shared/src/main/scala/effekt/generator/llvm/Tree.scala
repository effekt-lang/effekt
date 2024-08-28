package effekt
package generator
package llvm


/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST.html#t:Definition
 */
enum Definition {
  case Function(callingConvention: CallingConvention, returnType: Type, name: String, parameters: List[Parameter], basicBlocks: List[BasicBlock])
  case VerbatimFunction(returnType: Type, name: String, parameters: List[Parameter], body: String)
  case Verbatim(content: String)
  case GlobalConstant(name: String, initializer: Operand) // initializer should be constant

  //  !100 = !{!"frame_100", !13, i64 0, !13, i64 8}
  case TypeDescriptor(id: Int, typeName: String, structure: List[(TBAA, Int)])

  //  ; int, into stack at offset 1
  //  !25 = !{!99, !13, i64 8}
  case AccessTag(id: Int, base: Int, access: TBAA, offset: Int)
}
export Definition.*

enum CallingConvention {
  case Ccc()
  case Tailcc()
}
export CallingConvention.*

case class BasicBlock(name: String, instructions: List[Instruction], terminator: Terminator)

/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST-Instruction.html#t:Instruction
 */
enum Instruction {
  case Call(result: String, callingConvention: CallingConvention, resultType: Type, function: Operand, arguments: List[Operand])
  case Load(result: String, tpe: Type, address: Operand, tbaa: Option[TBAA])
  case Store(address: Operand, value: Operand, tbaa: Option[TBAA])
  case GetElementPtr(result: String, tpe: Type, address: Operand, indices: List[Int])
  case BitCast(result: String, operand: Operand, typ: Type)
  case Add(result: String, operand0: Operand, operand1: Operand)
  case FAdd(result: String, operand0: Operand, operand1: Operand)
  case InsertValue(result: String, aggregate: Operand, element: Operand, index: Int)
  case ExtractValue(result: String, aggregate: Operand, index: Int)
  case Comment(msg: String)
}
export Instruction.*

enum Terminator {
  case RetVoid()
  case Switch(operand: Operand, defaultDest: String, dests: List[(Int, String)])
  case CondBr(condition: Operand, trueDest: String, falseDest: String)
}
export Terminator.*

case class Parameter(typ: Type, name: String)

// Operands cannot be an enum since we use the more specific types massively.
// Scala 3 will perform widening way too often.
sealed trait Operand
object Operand {
  case class LocalReference(tpe: Type, name: String) extends Operand
  case class ConstantInt(n: Long) extends Operand
  case class ConstantDouble(x: Double) extends Operand
  case class ConstantAggregateZero(typ: Type) extends Operand
  case class ConstantGlobal(tpe: Type, name: String) extends Operand
  case class ConstantNull(tpe: Type) extends Operand
  case class ConstantArray(memberType: Type, members: List[Operand]) extends Operand // members should be homogeneous
  case class ConstantInteger8(b: Byte) extends Operand
}
export Operand.*

enum TBAA {
  case Byte()
  case Int()
  case Double()
  case String()
  case Pos()
  case Neg()
  case Stack()
  case StackPointer()
  case ReferenceCount()
  case Memory()
  case Region()
  case FrameAccess(id: scala.Int, offset: scala.Int)

  def size: scala.Int = this match {
    case TBAA.Byte() => 1
    case TBAA.Int() => 8
    case TBAA.Double() => 8
    case TBAA.String() => 8
    case TBAA.Pos() => 16
    case TBAA.Neg() => 16
    case TBAA.Stack() => 8
    case TBAA.StackPointer() => 8
    case TBAA.ReferenceCount() => 8
    case _ => ???
  }
}



/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST.html#t:Type
 */
enum Type {
  case VoidType()
  case IntegerType1()
  case IntegerType8() // required for `void*` (which only exists as `i8*` in LLVM) and `char*`
  case IntegerType64()
  case DoubleType()
  case PointerType()
  case ArrayType(size: Int, of: Type)
  case StructureType(elementTypes: List[Type])
  case FunctionType(resultType: Type, argumentTypes: List[Type])
  case NamedType(name: String)
}
export Type.*
