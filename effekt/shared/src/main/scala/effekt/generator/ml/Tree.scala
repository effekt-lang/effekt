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
  case Integer
  case Real
  case String
  case Bool
  case Data(name: MLName)
  case Alias(name: MLName)
  case Record(fields: List[(MLName, Type)])
}

enum Binding {
  case AnonBind(body: Expr)
  case ValBind(name: MLName, body: Expr)
  case FunBind(name: MLName, params: List[(MLName, Option[Type])], body: Expr)
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
  case Lambda(params: List[(MLName, Option[Type])], body: Expr)

  // e.g. (if COND then THEN else ELSE)
  case If(cond: Expr, thn: Expr, els: Expr)

  // e.g x
  case Variable(name: MLName)

  case FieldLookup(record: Expr, field: MLName)

  case MakeRecord(fields: List[(MLName, Expr)])

  case MakeDatatype(tag: MLName, arg: Option[Expr])

  case Match(scrutinee: Expr, clauses: List[MatchClause], default: Option[Expr])
}

case class MatchClause(pattern: Pattern, body: Expr)
enum Pattern {
  // ml supports direct binders (`{x, y}`) but we don't need them.
  case Record(assignments: List[(MLName, MLName)])
  case Datatype(tag: MLName, terms: List[MLName])
}

export Expr.*

object Consts {

  val unitVal: Expr = RawValue("()")
  val trueVal: Expr = RawValue("true")
  val falseVal: Expr = RawValue("false")
  val id: Expr = ml.Lambda((MLName("x"), None))(Variable(MLName("x")))

  // from effekt.sml
  val bind: Expr = Variable(MLName("bind"))
  val pure: Expr = Variable(MLName("pure"))
  val lift: Expr = Variable(MLName("lift"))
  val reset: Expr = Variable(MLName("reset"))
  val run: Expr = Variable(MLName("run"))
  val nested: Expr = Variable(MLName("nested"))
  val here: Expr = Variable(MLName("here"))

}

//enum Def {
//  // e.g. (define x 42)
//  case Constant(name: MLName, value: Expr)
//
//  // e.g. (define (f x y) ...)
//  case Function(name: MLName, params: List[MLName], body: Block)
//
//  case RawDef(raw: String)
//
//  case Record(typeName: MLName, constructorName: MLName, predicateName: MLName, uid: MLName, fields: List[MLName])
//}
//export Def.*

// smart constructors
def Call(name: MLName)(args: Expr*): Expr = Expr.Call(Variable(name), args.toList)

def Call(expr: Expr)(args: Expr*): Expr = Expr.Call(expr, args.toList)

def Lambda(params: (MLName, Option[Type])*)(body: Expr): Lambda = Expr.Lambda(params.toList, body)

def MLString(mlString: String): Expr = RawValue(s"\"$mlString\"")
