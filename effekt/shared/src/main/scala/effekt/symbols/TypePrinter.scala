//package effekt
//package symbols
//
//import effekt.symbols.builtins.*
//import effekt.typer.ConcreteEffects
//import kiama.output.ParenPrettyPrinter
//
//import scala.language.implicitConversions
//
//object TypePrinter extends ParenPrettyPrinter {
//
//  import kiama.output.PrettyPrinterTypes.Document
//
//  val debug = false
//
//  def show(id: source.IdDef): String = id.name
//  def show(id: source.IdRef): String = (id.path :+ id.name).mkString("::")
//
//  def show(n: Name): String = n match {
//    case name: NoName.type  => name.name
//    case name: LocalName    => name.name
//    case name: QualifiedName => name.qualifiedName
//  }
//  def show(t: Type): String = pretty(toDoc(t), 80).layout
//  def show(t: Capture): String = pretty(toDoc(t), 80).layout
//  def show(t: Captures): String = pretty(toDoc(t), 80).layout
//  def show(t: Effects): String = pretty(toDoc(t), 80).layout
//  def show(t: List[ValueType | TypeVar]): String = pretty(maybeTypeParams(t), 80).layout
//
//  val show: PartialFunction[Any, String] =  ???
//
//  def toDoc(m: Type): Doc = m match {
//    case tpe: ValueType => toDoc(tpe)
//    case tpe: BlockType => toDoc(tpe)
//  }
//
//  def toDoc(tpe: ValueType): Doc = ???
//
//  def toDoc(tpe: TypeVar): Doc = tpe match {
//    case typeVar: UnificationVar => typeVar.toString
//    case typeVar: TypeVar => typeVar.name
//  }
//
//  def toDoc(tpe: BlockType): Doc = ???
//
//  def toDoc(t: BlockTypeConstructor): Doc = ???
//
//  def toDoc(t: TypeConstructor): Doc = ???
//
//  def toDoc(eff: Effects): Doc = ???
//
//  def toDoc(c: Captures): Doc = ???
//
//  def toDoc(c: Capture): Doc = c.name
//
//  implicit def toDoc(name: Name): Doc = name.name
//
//  def typeParams(tparams: List[ValueType | TypeVar]): Doc = ???
//
//  def maybeTypeParams(tparams: List[ValueType | TypeVar]): Doc = ???
//
//}
//
//implicit class ErrorMessageInterpolator(private val sc: StringContext) extends AnyVal {
//  def pp(args: Any*): String = sc.s(args.map(TypePrinter.show.orElse(_.toString)): _*)
//}
//
