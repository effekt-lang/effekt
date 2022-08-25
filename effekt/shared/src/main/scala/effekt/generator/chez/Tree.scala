package effekt
package generator
package chez

// TODO choose appropriate representation and apply conversions
case class ChezName(name: String)

case class Binding(name: ChezName, expr: Expr)

case class Block(definitions: List[Def], expressions: List[Expr], result: Expr)

case class Handler(constructorName: ChezName, operations: List[Operation])
case class Operation(name: ChezName, params: List[ChezName], k: ChezName, body: Expr)

/**
 * This file defines the syntax of Chez Scheme as it is the image of our translation.
 */
enum Expr {

  // e.g. (<EXPR> <EXPR>)
  case Call(callee: Expr, arguments: List[Expr])

  // e.g. 42 (represented as Scala string "42") and inserted verbatim
  case RawExpr(raw: String)

  // e.g. (let ([x 42]) (+ x x))
  case Let(bindings: List[Binding], body: Block)

  // e.g. (let* ([x 42] [y x]) (+ x y))
  case Let_*(bindings: List[Binding], body: Block)

  // e.g. (lambda (x y) body)
  case Lambda(params: List[ChezName], body: Block)

  // e.g. (if COND THEN ELSE)
  case If(cond: Expr, thn: Expr, els: Expr)

  // e.g x
  case Variable(name: ChezName)

  // match and handle are both macros, stable across Chez variants, so we add them to the language.
  case Match(scrutinee: Expr, clauses: List[(Expr, Expr)])

  case Handle(handlers: List[Handler], body: Expr)
}
export Expr.*

enum Def {
  // e.g. (define x 42)
  case Constant(name: ChezName, value: Expr)

  // e.g. (define (f x y) ...)
  case Function(name: ChezName, params: List[ChezName], body: Block)

  case RawDef(raw: String)

  case Record(typeName: ChezName, constructorName: ChezName, predicateName: ChezName, uid: ChezName, fields: List[ChezName])
}
export Def.*

// smart constructors
def Call(name: ChezName, args: Expr*): Expr = Call(Variable(name), args.toList)

def Lambda(params: List[ChezName], body: Expr): Lambda = Lambda(params, Block(Nil, Nil, body))

def Function(name: ChezName, params: List[ChezName], body: Expr): Function = Function(name, params, Block(Nil, Nil, body))

def ChezString(chezString: String): Expr = RawExpr(s"\"${chezString}\"")

def Builtin(name: String, args: Expr*): Expr = Call(Variable(ChezName(name)), args.toList)

def curry(lam: chez.Lambda): chez.Lambda = lam.params.foldRight[chez.Lambda](chez.Lambda(Nil, lam.body)) {
  case (p, body) => chez.Lambda(List(p), body)
}