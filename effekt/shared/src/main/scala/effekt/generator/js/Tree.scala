package effekt
package generator
package js

// TODO choose appropriate representation and apply conversions
case class JSName(name: String)

val $effekt = Variable(JSName("$effekt"))

case class Import(name: JSName, file: String)
case class Export(name: JSName, expr: Expr)

case class Module(name: JSName, imports: List[Import], exports: List[Export], stmts: List[Stmt]) {
  def amdefine: List[Stmt] = {
    val prelude = RawStmt("if (typeof define !== 'function') { var define = require('amdefine')(module) }")
    val importFiles = ArrayLiteral(imports.map(i => JsString(s"./${i.file}")))
    val importNames = imports.map(i => i.name)

    List(js.ExprStmt(js.Call(Variable(JSName("define")), List(importFiles,
      js.Lambda(importNames, js.Block(moduleBody))))))
  }

  def commonjs: List[Stmt] = {
    val importStmts = imports.map { i =>
      // const MOD = require(PATH)
      js.Const(i.name, js.Call(Variable(JSName("require")), List(JsString(s"./${i.file}"))))
    }

    importStmts ++ moduleBody
  }

  def virtual : List[Stmt] = {
    val importStmts = imports.map { i =>
      // const MOD = load(PATH)
      js.Const(i.name, js.Call(Variable(JSName("load")), List(JsString(i.file))))
    }
    importStmts ++ moduleBody
  }

  def moduleBody: List[Stmt] = {
    val declaration = js.Const(name, js.Object())
    val exportStatement = js.ExprStmt(js.Call(RawExpr("module.exports = Object.assign"), List(
      js.Variable(name),
      js.Object(exports.map { e => e.name -> e.expr })
    )))

    (declaration :: stmts) :+ exportStatement
  }
}


/**
 * This file defines the syntax of JavaScript as it is the image of our translation.
 */
enum Expr {

  // e.g. <EXPR>(<EXPR>)
  case Call(callee: Expr, arguments: List[Expr])

  // e.g. 42 (represented as Scala string "42") and inserted verbatim
  case RawExpr(raw: String)

  // e.g. (<EXPR> ? <EXPR> : <EXPR>)
  case IfExpr(cond: Expr, thn: Expr, els: Expr)

  // e.g. (x, y) => <STMT>
  case Lambda(params: List[JSName], body: Stmt)

  // e.g. { x: <EXPR>, y: <EXPR>, ... }
  case Object(properties: List[(JSName, Expr)])

  // e.g. <EXPR>.<NAME>
  case Member(callee: Expr, selection: JSName)

  // e.g. [<EXPR>*] (we cannot call it "Array")
  case ArrayLiteral(elements: List[Expr])

  // e.g. x
  case Variable(name: JSName)
}
export Expr.*

enum Stmt {
  // e.g. { <STMT>* }
  case Block(stmts: List[Stmt])

  // e.g. return <EXPR>
  case Return(expr: Expr)

  // A raw JS String
  case RawStmt(raw: String)

  // e.g. const x = <EXPR>
  case Const(name: JSName, binding: Expr)

  case Switch(scrutinee: Expr, branches: List[(Expr, Stmt)], default: Option[Stmt])

  // e.g. function <NAME>(x, y) { <STMT>* }
  case Function(name: JSName, params: List[JSName], stmts: List[Stmt])

  // e.g. <EXPR>;
  case ExprStmt(expr: Expr)
}
export Stmt.*

// Some smart constructors
def MethodCall(receiver: Expr, method: JSName, args: Expr*): Expr = Call(Member(receiver, method), args.toList)

def Lambda(params: List[JSName], body: Expr): Expr = Lambda(params, Return(body))

def JsString(scalaString: String): Expr = RawExpr(s"\"${scalaString}\"")

def Object(properties: (JSName, Expr)*): Expr = Object(properties.toList)


object monadic {

  opaque type Control = Expr

  private val `then` = JSName("then")
  private val `run` = JSName("run")

  def Pure(expr: Expr): Control = Builtin("pure", expr)
  def Run(m: Control): Expr = MethodCall(m, `run`)

  def Bind(m: Control, body: Control): Control = MethodCall(m, `then`, js.Lambda(Nil, body))
  def Bind(m: Control, param: JSName, body: Control): Control = MethodCall(m, `then`, js.Lambda(List(param), body))

  def Call(callee: Expr, args: List[Expr]): Control = js.Call(callee, args)
  def If(cond: Expr, thn: Control, els: Control): Control = js.IfExpr(cond, thn, els)
  def While(cond: Control, body: Control): Control = Builtin("_while", js.Lambda(Nil, cond), js.Lambda(Nil, body))
  def Handle(handlers: List[Expr], body: Expr): Control = js.Call(Builtin("handle", js.ArrayLiteral(handlers)), List(body))

  def Builtin(name: String, args: Expr*): Control = js.MethodCall($effekt, JSName(name), args: _*)

  def Lambda(params: List[JSName], stmts: List[Stmt], ret: Control): Expr =
    js.Lambda(params, js.Block(stmts :+ js.Return(ret)))

  def Function(name: JSName, params: List[JSName], stmts: List[Stmt], ret: Control): Stmt =
    js.Function(name, params, stmts :+ js.Return(ret))
}
