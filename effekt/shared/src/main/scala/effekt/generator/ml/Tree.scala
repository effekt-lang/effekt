package effekt
package generator
package ml

// TODO choose appropriate representation and apply conversions
class MLName(n: String) {
  private def fixName(nn: String): String = {
    val tmp = nn.replace("$", "Dollar")
    if (tmp.startsWith("_")) "f" + tmp else tmp
  }

  val name: String = fixName(n)

  override def equals(obj: Any): Boolean = obj match {
    case n: MLName =>
      this.name == n.name
    case _ => false
  }

  override def hashCode(): Int = this.name.hashCode
}

enum Type {
  case Tapp(tpe: Type, arg: List[Type])
  case Builtin(n: MLName)
  case Var(n: MLName)
  case Tuple(l: List[Type])
  case Unit
  case Integer
  case Real
  case String
  case Bool
  case Data(name: MLName)
  case Alias(name: MLName)
}

enum Param {
  case Named(name: MLName)
  case Patterned(pattern: Pattern)
}

enum Binding {
  case AnonBind(body: Expr)
  case ValBind(name: MLName, body: Expr)
  case FunBind(name: MLName, params: List[Param], body: Expr)
  case RawBind(raw: String)
  case DataBind(name: MLName, tparams: List[Type.Var], constructors: List[(MLName, Option[Type])])
  case TypeBind(name: MLName, tparams: List[Type.Var], tpe: Type)
}
export Binding.*

case class Toplevel(bindings: List[Binding], body: Expr)

//case class Handler(constructorName: MLName, operations: List[Operation])
//case class Operation(name: MLName, params: List[MLName], k: MLName, body: Expr)

/**
 * This file defines the syntax of ML as it is the image of our translation.
 */
enum Expr {

  // e.g. (<EXPR>(<EXPR>, ..., <EXPR>))
  case Call(callee: Expr, args: List[Expr])

  // e.g. (display "foo")
  case RawExpr(raw: String)

  // e.g. 42 (represented as Scala string "42") and inserted verbatim
  case RawValue(raw: String)

  // Sequential Scoping
  case Let(bindings: List[Binding], body: Expr)

  case Tuple(terms: List[Expr])

  case Sequence(exps: List[Expr], rest: Expr)

  //  // e.g. (let* ([x 42] [y x]) (+ x y))
  //  case Let_*(bindings: List[Binding], body: Block)

  // e.g. (fn (x y) => body)
  case Lambda(params: List[Param], body: Expr)

  // e.g. (if COND then THEN else ELSE)
  case If(cond: Expr, thn: Expr, els: Expr)

  // e.g x
  case Variable(name: MLName)

  case MakeDatatype(tag: MLName, arg: Option[Expr])

  case Match(scrutinee: Expr, clauses: List[MatchClause], default: Option[Expr])
}

case class MatchClause(pattern: Pattern, body: Expr)
enum Pattern {
  case Wild()
  case Named(name: MLName)
  case Datatype(tag: MLName, terms: List[Pattern])
}

export Expr.*

object Consts {

  val unitVal: Expr = RawValue("()")
  val trueVal: Expr = RawValue("true")
  val falseVal: Expr = RawValue("false")
  val id: Expr = ml.Lambda(ml.Param.Named(MLName("x")))(Variable(MLName("x")))

  // from effekt.sml
  val bind: Expr = Variable(MLName("bind"))
  val pure: Expr = Variable(MLName("pure"))
  val lift: Expr = Variable(MLName("lift"))
  val reset: Expr = Variable(MLName("reset"))
  val run: Expr = Variable(MLName("run"))
  val nested: Expr = Variable(MLName("nested"))
  val here: Expr = Variable(MLName("here"))

}

// smart constructors
def Call(name: MLName)(args: Expr*): Expr = Expr.Call(Variable(name), args.toList)

def Call(expr: Expr)(args: Expr*): Expr = Expr.Call(expr, args.toList)

def Lambda(params: Param*)(body: Expr): Lambda = Expr.Lambda(params.toList, body)

def MLString(mlString: String): Expr = RawValue(s"\"$mlString\"")
