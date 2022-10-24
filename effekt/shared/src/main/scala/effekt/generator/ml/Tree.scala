package effekt
package generator
package ml

// TODO choose appropriate representation and apply conversions
case class MLName(name: String)

sealed trait Binding
case class ValBind(name: MLName, body: Expr) extends Binding
case class FunBind(name: MLName, params: List[MLName], body: Expr) extends Binding
case class RawBind(raw: String) extends Binding

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

//  // e.g. (let* ([x 42] [y x]) (+ x y))
//  case Let_*(bindings: List[Binding], body: Block)

  // e.g. (fn (x y) => body)
  case Lambda(params: List[MLName], body: Expr)

  // e.g. (if COND then THEN else ELSE)
  case If(cond: Expr, thn: Expr, els: Expr)

  // e.g x
  case Variable(name: MLName)

//  // match and handle are both macros, stable across Chez variants, so we add them to the language.
//  case Match(scrutinee: Expr, clauses: List[(Expr, Expr)])

//  case Handle(handlers: List[Handler], body: Expr)
}
export Expr.*

object Constants {
  
  val unitVal: Expr = RawValue("()")
  val trueVal: Expr = RawValue("true")
  val falseVal: Expr = RawValue("false")
  
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

def Lambda(params: MLName*)(body: Expr): Lambda = Expr.Lambda(params.toList, body)

//def Function(name: MLName)(params: MLName*)(body: Expr): Function = Expr.Function(name, params.toList, body)

def MLString(mlString: String): Expr = RawValue(s"\"$mlString\"")

def Builtin(name: String)(args: Expr*): Expr = Call(Variable(MLName(name)), args.toList)
