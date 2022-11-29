package effekt
package core

import symbols.{ Symbol, TypeSymbol }
import symbols.Name

class Id(n: String) extends symbols.Symbol {
  val name = Name.local(n)
}

type Capture = Id
type Captures = Set[Capture]

sealed trait Type

enum ValueType extends Type {
  case Var(name: Id)
  case Data(symbol: TypeSymbol, targs: List[ValueType])
  case Record(symbol: TypeSymbol, targs: List[ValueType])
  case Boxed(tpe: BlockType, capt: Captures)
  case Extern(symbol: TypeSymbol, targs: List[ValueType])
}

enum BlockType extends Type {

  // [A, B, C] (X, Y, Z)   {  f  :   S }    =>    T
  //  ^^^^^^^   ^^^^^^^     ^^^^^^^^^^^          ^^^
  //  tparams   vparams   cparams zip bparams   result
  case Function(tparams: List[Id], cparams: List[Id], vparams: List[ValueType], bparams: List[BlockType], result: ValueType)
  case Interface(symbol: TypeSymbol, targs: List[ValueType])
  case Extern(symbol: TypeSymbol, targs: List[ValueType])
}


