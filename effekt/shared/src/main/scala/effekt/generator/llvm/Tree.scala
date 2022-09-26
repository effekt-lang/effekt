package effekt
package llvm

/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST.html#t:Definition
 */
enum Definition {
  case Function(returnType: Type, name: String, parameters: List[Parameter], basicBlocks: List[BasicBlock])
  case VerbatimFunction(returnType: Type, name: String, parameters: List[Parameter], body: String)
  case Verbatim(content: String)
}
export Definition.*

case class BasicBlock(name: String, instructions: List[Instruction], terminator: Terminator)

/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST-Instruction.html#t:Instruction
 */
enum Instruction {
  case Call(result: String, resultType: Type, function: Operand, arguments: List[Operand])
  case TailCall(function: Operand, arguments: List[Operand])
  case Load(result: String, address: Operand)
  case Store(address: Operand, value: Operand)
  case GetElementPtr(result: String, address: Operand, indices: List[Int])
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

// Operands cannot be an enum since we use the more specific types massively. Scala 3 will perform widening way
// too often.
sealed trait Operand
object Operand {
  case class LocalReference(tpe: Type, name: String) extends Operand
  case class ConstantInt(n: Int) extends Operand
  case class ConstantDouble(x: Double) extends Operand
  case class ConstantAggregateZero(typ: Type) extends Operand
  case class ConstantGlobal(tpe: Type, name: String) extends Operand
  case class ConstantNull(tpe: Type) extends Operand
}
export Operand.*

/**
 *  see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST.html#t:Type
 */
enum Type {
  case VoidType()
  case IntegerType1()
  case IntegerType8() // required for `void*` (which only exists as `i8*` in LLVM) and `char*`
  case IntegerType64()
  case PointerType(pointerReferent: Type)
  case ArrayType(size: Int, of: Type)
  case StructureType(elementTypes: List[Type])
  case FunctionType(resultType: Type, argumentTypes: List[Type])
  case NamedType(name: String)
}
export Type.*
