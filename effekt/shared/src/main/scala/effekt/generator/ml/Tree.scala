package effekt
package generator
package ml

// TODO choose appropriate representation and apply conversions
case class MLName(name: String)

case class Binding(name: MLName, expr: Expr)

case class Block(definitions: List[Def], expressions: List[Expr], result: Expr)

case class Handler(constructorName: MLName, operations: List[Operation])
case class Operation(name: MLName, params: List[MLName], k: MLName, body: Expr)

/**
 * This file defines the syntax of ML as it is the image of our translation.
 */
enum Expr {

  // e.g. (<EXPR> <EXPR>)
  case Call(callee: Expr, arguments: List[Expr])

  // e.g. (display "foo")
  case RawExpr(raw: String)

  // e.g. 42 (represented as Scala string "42") and inserted verbatim
  case RawValue(raw: String)

  // e.g. (let ([x 42]) (+ x x))
  case Let(bindings: List[Binding], body: Block)

  // e.g. (let* ([x 42] [y x]) (+ x y))
  case Let_*(bindings: List[Binding], body: Block)

  // e.g. (lambda (x y) body)
  case Lambda(params: List[MLName], body: Block)

  // e.g. (if COND THEN ELSE)
  case If(cond: Expr, thn: Expr, els: Expr)

  // e.g x
  case Variable(name: MLName)

  // match and handle are both macros, stable across Chez variants, so we add them to the language.
  case Match(scrutinee: Expr, clauses: List[(Expr, Expr)])

  case Handle(handlers: List[Handler], body: Expr)
}
export Expr.*

enum Def {
  // e.g. (define x 42)
  case Constant(name: MLName, value: Expr)

  // e.g. (define (f x y) ...)
  case Function(name: MLName, params: List[MLName], body: Block)

  case RawDef(raw: String)

  case Record(typeName: MLName, constructorName: MLName, predicateName: MLName, uid: MLName, fields: List[MLName])
}
export Def.*

// smart constructors
def Call(name: MLName, args: Expr*): Expr = Call(Variable(name), args.toList)

def Lambda(params: List[MLName], body: Expr): Lambda = Lambda(params, Block(Nil, Nil, body))

def Function(name: MLName, params: List[MLName], body: Expr): Function = Function(name, params, Block(Nil, Nil, body))

def MLString(chezString: String): Expr = RawExpr(s"\"${chezString}\"")

def Builtin(name: String, args: Expr*): Expr = Call(Variable(MLName(name)), args.toList)

def curry(lam: ml.Lambda): ml.Lambda = lam.params.foldRight[ml.Lambda](ml.Lambda(Nil, lam.body)) {
  case (p, body) => ml.Lambda(List(p), body)
}
