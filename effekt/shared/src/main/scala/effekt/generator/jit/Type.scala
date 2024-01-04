package effekt.generator
package jit

sealed trait Type
object Top extends Type
object Ptr extends Type
object Num extends Type
object Bottom extends Type
enum Purity {
  case Pure, Effectful
}
case class Function(params: List[Type], ret: Type, purity: Purity) extends Type
case class Method(tag: Id, params: List[Type], ret: Type) {
  def purity = Purity.Effectful
}
case class Codata(ifce_tag: Id, methods: List[Method]) extends Type

case class Constructor(tag: Id, fields: List[Type])
case class Data(type_tag: Id, constructors: List[Constructor]) extends Type
case class Stack(resume_ret: Type, resume_args: List[Type]) extends Type

case class Ref(to: Type) extends Type

sealed trait Base extends Type {
  type ScalaType
}
object Base {
  object Int extends Base { override type ScalaType = scala.Int }
  object Double extends Base { override type ScalaType = scala.Double }
  object String extends Base { override type ScalaType = java.lang.String }
  object Label extends Base { override type ScalaType = Null }
  object Unit extends Base { override type ScalaType = Unit }
}

val Region = Ptr // TODO actual Region type


