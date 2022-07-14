package effekt
package llvm

import effekt.context.Context
import effekt.symbols.{ Symbol, ValueSymbol, BlockSymbol }

sealed trait Tree extends Product {
  def inheritPosition(from: source.Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

/**
 * Toplevel definitions
 */

sealed trait Top extends Tree
case class DefCnt(id: BlockSymbol, params: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
case class DefFrm(id: BlockSymbol, params: List[machine.Param], env: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
case class DefClo(id: BlockSymbol, params: List[machine.Param], env: List[machine.Param], entry: BlockSymbol, body: List[BasicBlock]) extends Top
case class DefFun(typ: machine.Type, id: BlockSymbol, params: List[machine.Param], body: String) extends Top
case class DefScn(id: BlockSymbol, env: List[machine.Param]) extends Top
case class Include(content: String) extends Top
case class BasicBlock(id: BlockSymbol, instructions: List[Instruction], terminator: Terminator) extends Tree

// TODO Should we be more type-strict?
type Operand = String
type Name = String

// see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST-Instruction.html#t:Instruction
sealed trait Instruction extends Tree
case class Add(x: Operand, y: Operand)
case class Phi(from: List[(Operand, Name)], to: Type) extends Instruction // once was: case class Phi(param: machine.Param, args: List[(BlockSymbol, machine.Value)]) extends Instruction
case class InsertValue(flatAggregate: Operand, index: int64, part: Operand) extends Instruction // once was: case class InsertValues(id: ValueSymbol, typ: machine.Record, args: List[machine.Value]) extends Instruction
case class ExtractValue(flatAggregate: Operand, index: int64, part: Operand) extends Instruction // once was: case class ExtractValue(id: ValueSymbol, target: machine.Value, field: Int) extends Instruction

// An LLVM terminator.
sealed trait Terminator extends Tree
case class Ret(values: Option[Operand]) extends Terminator
case class Br(dest: Name)  extends Terminator // TODO needed?

// TODO `jump` does not exist in LLVM. (In LLVM, one can only jump within declarations, not globally to anywhere within the program. When implementing a C compiler, this is of little hinderance, since one wants to use the provided C stack anyways. When one integral feature of one's language's implementation -- here Effekt's LLVM backend -- is correctly and sanely managing stacks, this truly becomes a hinderance.) Therefore, `jump` ought to appear in one of the many machine representations and be implemented using `tail call ; ret void` in the machine->llvm translation.
case class Jump(id: BlockSymbol, args: List[machine.Value]) extends Terminator // LLVM `tail call`
case class JumpLocal(id: BlockSymbol, args: List[machine.Value]) extends Terminator // LLVM unconditional `br`
case class If(cond: machine.Value, thenBlock: BlockSymbol, thenArgs: List[machine.Value], elseBlock: BlockSymbol, elseArgs: List[machine.Value]) extends Terminator
case class Switch(arg: machine.Value, default: BlockSymbol, labels: List[(Int, BlockSymbol)]) extends Terminator
case class Panic() extends Terminator

// TODO rename `s/TrueLLVMType/Type/` (it is correctly namespaced since we are in `llvm/Tree.scala`) after name confusion is no longer likely
/*
All these types form compile-time statically known data. These Scala values are
lifted to LLVM type values during compilation.
*/
// see: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST.html#t:Type
type TrueLLVMType = Type
sealed trait TrueLLVMType
case class Void() extends TrueLLVMType
// all integers are 64 bits wide
case class Int64() extends TrueLLVMType
// ignoring LLVM's address space semantic
case class Pointer(to: TrueLLVMType) extends TrueLLVMType
// TODO struct and union
// a C-style function (**not** an Effekt function), never variadic
case class CFunction(args: List[TrueLLVMType], ret: TrueLLVMType) extends TrueLLVMType

/*
A frame is one function's environment together with its static behaviour.
The static `@scanner` function pointer knows about the frame's type and layout
and can therefore both copy and destroy the frame (given its dynamically known
absolute reference position on the heap).
frame:
    @scanner
    primitive_1
    primitive_2
    ...
    primitive_n
    boxed_1
    boxed_2
    ...
    boxed_n
*/
case class EffektFrame(scanner: Pointer, primitives: List[Int64], boxed: List[Pointer]) extends TrueLLVMType

/*
[jfrech, 2022-07-12] TODO apparently, this notion is subtly incorrect, yet its clarification requires exact specification of what we mean by an *environment* (local register representation of a frame, C-style environment, or Effekt-stack baked state representation)
[jfrech, 2022-07-14] Maybe we should use a terminology along the lines of "C environment", "Effekt environment" and "live environment".

From a function's point of view, its stacks form an array-singly-linked-list-
hybrid data structure (a "metastack" of sorts) which defines the entire
environment known to it.
When no effect arbitration is necessary, a subfunction simply gets its frame
pushed onto the heap relative to the stack's `top`.
If an effect split happens, a new stack is created with evidence-sufficiently
deep parent copying.
A stack is copiable and destructible to varying depths (the necessary depth is
called evidence) only utilizing the various frames' `@scanner` implementations.
*/
case class EffektStack(base: Pointer, top: Pointer, cap: Pointer, parent: Pointer) extends TrueLLVMType
