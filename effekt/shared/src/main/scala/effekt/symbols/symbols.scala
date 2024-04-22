package effekt
package symbols


import effekt.context.Context
import kiama.util.Source

sealed trait TermSymbol extends Symbol
sealed trait ValueSymbol extends TermSymbol
sealed trait BlockSymbol extends TermSymbol

case class Module(
  source: Source
) extends Symbol {
  val name: Name = ???
}

sealed trait RefBinder extends BlockSymbol

sealed trait Param extends TermSymbol
case class ValueParam(name: Name, tpe: Option[ValueType]) extends Param, ValueSymbol


sealed trait TrackedParam extends Param, BlockSymbol {
  // Every tracked block gives rise to a capture parameter (except resumptions, they are transparent)
  lazy val capture: Capture = ???
}
object TrackedParam {
  case class BlockParam(name: Name, tpe: BlockType) extends TrackedParam
  case class ResumeParam(module: Module) extends TrackedParam { val name = ??? }
  case class ExternResource(name: Name, tpe: BlockType) extends TrackedParam

}


trait Callable extends BlockSymbol

case class UserFunction(
  name: Name
) extends Callable


trait Binder extends TermSymbol {
  def tpe: Option[Type]

}

sealed trait TypeSymbol extends Symbol
sealed trait ValueTypeSymbol extends TypeSymbol
sealed trait BlockTypeSymbol extends TypeSymbol

enum TypeVar(val name: Name) extends ValueTypeSymbol {
  case TypeParam(n: Name) extends TypeVar(n)
  case UnificationVar(underlying: TypeVar.TypeParam) extends TypeVar(underlying.name)
}
export TypeVar.*

enum TypeConstructor extends TypeSymbol {
  def tparams: List[TypeParam]

  case DataType(name: Name, tparams: List[TypeParam])
  case Record(name: Name, tparams: List[TypeParam])
  case ExternType(name: Name, tparams: List[TypeParam])
}
export TypeConstructor.*



enum BlockTypeConstructor extends BlockTypeSymbol {
  def tparams: List[TypeParam]

  case Interface(name: Name, tparams: List[TypeParam])
  case ExternInterface(name: Name, tparams: List[TypeParam])
}
export BlockTypeConstructor.*



sealed trait CaptVar extends TypeSymbol
trait Capture extends TypeSymbol

case class CaptUnificationVar(role: CaptUnificationVar.Role) extends Captures, CaptVar {
  val name = ???

}
object CaptUnificationVar {
  sealed trait Role

}

sealed trait Captures
