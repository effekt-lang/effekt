package effekt
package generator
package js

// TODO choose appropriate representation and apply conversions
case class JSName(name: String)

val $effekt = Variable(JSName("$effekt"))

enum Import {
  // import * as <name> from "<file>";
  case All(name: JSName, file: String)

  // import {<members>, ...} from "<file>";
  case Selective(members: List[JSName], file: String)
}

case class Export(name: JSName, expr: Expr)

case class Module(name: JSName, imports: List[Import], exports: List[Export], stmts: List[Stmt]) {

  /**
   * Generates the Javascript module skeleton for whole program compilation
   */
  def commonjs: List[Stmt] = {
    val effekt = js.Const(List(JSName("$effekt")), js.Object())

    val importStmts = imports.map {
      // const MOD = require(PATH)
      case Import.All(name, file) =>
        js.Const(List(name), js.Call(Variable(JSName("require")), List(JsString(s"./${file}"))))

      // const {NAMES, ...} = require(PATH)
      case Import.Selective(names, file) =>
        js.Destruct(names, js.Call(Variable(JSName("require")), List(JsString(s"./${ file }"))))
    }

    val exportStatement = js.Assign(RawExpr("module.exports"),
      js.Object(exports.map { e => e.name -> e.expr })
    )

    List(effekt) ++ importStmts ++ stmts ++ List(exportStatement)
  }

  /**
   * Generates the Javascript module skeleton for virtual modules that are compiled separately
   *
   * {{{
   *   const MYMODULE = {}
   *   // ... contents of the module
   *   module.exports = Object.assign(MYMODULE, {
   *     // EXPORTS...
   *   })
   * }}}
   */
  def virtual : List[Stmt] = {
    val importStmts = imports.map {
      // const MOD = load(PATH)
      case Import.All(name, file) =>
        js.Const(List(name), js.Call(Variable(JSName("load")), List(JsString(file))))

      // const {NAMES, ...} = load(PATH)
      case Import.Selective(names, file) =>
        js.Destruct(names, js.Call(Variable(JSName("load")), List(JsString(file))))
    }

    val declaration = js.Const(List(name), js.Object())

    // module.exports = Object.assign(MODULE, { EXPORTS })
    val exportStatement = js.ExprStmt(js.Call(RawExpr("module.exports = Object.assign"), List(
      js.Variable(name),
      js.Object(exports.map { e => e.name -> e.expr })
    )))

    importStmts ++ List(declaration) ++ stmts ++ List(exportStatement)
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

  // e.g. return [<EXPR>*]
  case Return(exprs: List[Expr]) // TODO MRV 5

  // A raw JS String
  case RawStmt(raw: String)

  // e.g. const [x, y] = <EXPR>
  case Const(names: List[JSName], binding: Expr) // TODO MRV 5

  // e.g. <EXPR> = <EXPR>
  case Assign(target: Expr, value: Expr)

  // e.g. const {x,y,z} = <EXPR>
  case Destruct(names: List[JSName], binding: Expr)

  // e.g. switch (sc) { case <EXPR>: <STMT>; ...; default: <STMT> }
  case Switch(scrutinee: Expr, branches: List[(Expr, Stmt)], default: Option[Stmt])

  // e.g. function <NAME>(x, y) { <STMT>* }
  case Function(name: JSName, params: List[JSName], stmts: List[Stmt])

  // e.g. <EXPR>;
  case ExprStmt(expr: Expr)
}
export Stmt.*

// Some smart constructors
def MethodCall(receiver: Expr, method: JSName, args: Expr*): Expr = Call(Member(receiver, method), args.toList)

def Lambda(params: List[JSName], body: Expr): Expr = Lambda(params, Return(List(body)))

def JsString(scalaString: String): Expr = RawExpr(s"\"${scalaString}\"")

def Object(properties: (JSName, Expr)*): Expr = Object(properties.toList)


object monadic {

  opaque type Control = Expr

  private val `then` = JSName("then")
  private val `run` = JSName("run")

  def Pure(expr: Expr): Control = Builtin("pure", expr) // TODO MRV: List[Expr]
  def Run(m: Control): Expr = MethodCall(m, `run`)

  def Bind(m: Control, body: Control): Control = MethodCall(m, `then`, js.Lambda(Nil, body))
  def Bind(m: Control, params: List[JSName], body: Control): Control = MethodCall(m, `then`, js.Lambda(params, body)) // TODO MRV 5

  def Call(callee: Expr, args: List[Expr]): Control = js.Call(callee, args)
  def If(cond: Expr, thn: Control, els: Control): Control = js.IfExpr(cond, thn, els)
  def Handle(handlers: List[Expr], body: Expr): Control = js.Call(Builtin("handle", js.ArrayLiteral(handlers)), List(body))

  def Builtin(name: String, args: Expr*): Control = js.MethodCall($effekt, JSName(name), args: _*)

  def Lambda(params: List[JSName], stmts: List[Stmt], ret: List[Control]): Expr =
    js.Lambda(params, js.Block(stmts :+ js.Return(ret)))

  def Function(name: JSName, params: List[JSName], stmts: List[Stmt], ret: List[Control]): Stmt =
    js.Function(name, params, stmts :+ js.Return(ret))
}
