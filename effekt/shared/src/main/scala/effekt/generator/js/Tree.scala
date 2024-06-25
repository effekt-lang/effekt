package effekt
package generator
package js

import scala.collection.immutable.{ AbstractSeq, LinearSeq }

// TODO choose appropriate representation and apply conversions
case class JSName(name: String)

object $effekt {
  val namespace = Variable(JSName("$effekt"))
  def field(name: String): js.Expr =
    js.Member(namespace, JSName(name))
  def call(name: String, args: js.Expr*): js.Expr =
    js.MethodCall(namespace, JSName(name), args: _*)
}

enum Import {
  // import * as <name> from "<file>";
  case All(name: JSName, file: String)

  // import {<members>, ...} from "<file>";
  case Selective(members: List[JSName], file: String)
}

case class Export(name: JSName, expr: Expr)

case class Module(name: JSName, imports: List[Import], exports: List[Export], stmts: List[Stmt]) {

  /**
   * Generates the Javascript module skeleton for ES modules
   */
  def esModule: List[Stmt] = {
    val effekt = js.Const(JSName("$effekt"), js.Object())

    val importStmts = imports.map {
      case Import.All(name, file) =>
      // import * as MOD from PATH
        js.RawStmt(s"import * as ${name.name} from './${file}.mjs';")
      case Import.Selective(names, file) =>
      // import {NAMES, ...} from PATH
        js.RawStmt(s"import { ${names.map(_.name).mkString(", ")} } from './${file}.mjs';")
    }

    val exportStatements = exports.map { e =>
      RawExport(e.name, e.expr)
    }

    List(effekt) ++ importStmts ++ stmts ++ exportStatements
  }

  /**
   * Generates the Javascript module skeleton for virtual modules that are compiled separately
   */
  def virtual: List[Stmt] = {
    val importStmts = imports.map {
      case Import.All(name, file) =>
        js.RawStmt(s"const ${name.name} = await import('./${file}.mjs');")
      case Import.Selective(names, file) =>
        js.RawStmt(s"const { ${names.map(_.name).mkString(", ")} } = await import('./${file}.mjs');")
    }

    val exportStatements = exports.map { e =>
      RawExport(e.name, e.expr)
    }

    importStmts ++ stmts ++ exportStatements
  }
}


/**
 * This file defines the syntax of JavaScript as it is the image of our translation.
 */
enum Expr {

  // e.g. <EXPR>(<EXPR>)
  case Call(callee: Expr, arguments: List[Expr])

  // e.g. new <EXPR>(<EXPR>)
  case New(callee: Expr, arguments: List[Expr])

  // e.g. "" <EXPR> " + " <EXPR>
  //   raw JS splices, always start with a prefix string, then interleaved with arguments
  case RawExpr(raw: List[String], args: List[Expr])

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

def RawExpr(str: String): js.Expr = Expr.RawExpr(List(str), Nil)

implicit class JavaScriptInterpolator(private val sc: StringContext) extends AnyVal {
  def js(args: Expr*): Expr = RawExpr(sc.parts.toList, args.toList)
}


enum Pattern {
  case Variable(name: JSName)
  case Array(ps: List[Pattern])
}

enum Stmt {
  // e.g. { <STMT>* }
  case Block(stmts: List[Stmt])

  // e.g. return <EXPR>
  case Return(expr: Expr)

  // A raw JS String
  case RawStmt(raw: String)

  // e.g. const x = <EXPR>
  case Const(pattern: Pattern, binding: Expr)

  // e.g. let x = <EXPR>
  case Let(pattern: Pattern, binding: Expr)

  // e.g. <EXPR> = <EXPR>
  case Assign(target: Expr, value: Expr)

  // e.g. const {x,y,z} = <EXPR>
  case Destruct(names: List[JSName], binding: Expr)

  // e.g. switch (sc) { case <EXPR>: <STMT>; ...; default: <STMT> }
  case Switch(scrutinee: Expr, branches: List[(Expr, List[Stmt])], default: Option[List[Stmt]]) // TODO maybe flatten?

  // e.g. function <NAME>(x, y) { <STMT>* }
  case Function(name: JSName, params: List[JSName], stmts: List[Stmt])

  // e.g. class <NAME> {
  //        <NAME>(x, y) { <STMT>* }...
  //      }
  case Class(name: JSName, methods: List[Stmt.Function])

  // e.g. if (<EXPR>) { <STMT> } else { <STMT> }
  case If(cond: Expr, thn: Stmt, els: Stmt)

  // e.g. try { <STMT>* } catch(x) { <STMT>* }
  case Try(prog: List[Stmt], name: JSName, handler: List[Stmt], fin: List[Stmt] = Nil)

  // e.g. throw e
  case Throw(expr: Expr)

  // label : while (<EXPR>) { <STMT>* }
  case While(cond: Expr, stmts: List[Stmt], label: Option[JSName])

  // e.g. break
  case Break()

  // e.g. continue l
  case Continue(label: Option[JSName])

  // e.g. <EXPR>;
  case ExprStmt(expr: Expr)

  // e.g. export const <NAME> = <EXPR>;
  case RawExport(name: JSName, expr: Expr)
}
export Stmt.*

def Const(name: JSName, binding: Expr): Stmt = js.Const(Pattern.Variable(name), binding)
def Let(name: JSName, binding: Expr): Stmt = js.Let(Pattern.Variable(name), binding)

// Some smart constructors
def MethodCall(receiver: Expr, method: JSName, args: Expr*): Expr = Call(Member(receiver, method), args.toList)

def Lambda(params: List[JSName], body: Expr): Expr = Lambda(params, Return(body))

def JsString(scalaString: String): Expr = RawExpr(s"\"${scalaString}\"")

def Object(properties: (JSName, Expr)*): Expr = Object(properties.toList)

def MaybeBlock(stmts: List[Stmt]): Stmt = stmts match {
  case Nil => ???
  case head :: Nil => head
  case head :: next => js.Block(stmts)
}

val Undefined = RawExpr("undefined")

object monadic {

  opaque type Control = Expr

  private val `then` = JSName("then")
  private val `run` = JSName("run")

  def Pure(expr: Expr): Control = Builtin("pure", expr)
  def Run(m: Control): Expr = MethodCall(m, `run`)

  def State(id: JSName, init: Expr, stmts: List[Stmt], ret: Control): Control =
    Builtin("state", init, Lambda(List(id), stmts, ret))

  def Bind(m: Control, body: Control): Control = MethodCall(m, `then`, js.Lambda(Nil, body))
  def Bind(m: Control, param: JSName, body: Control): Control = MethodCall(m, `then`, js.Lambda(List(param), body))

  def Call(callee: Expr, args: List[Expr]): Control = js.Call(callee, args)
  def If(cond: Expr, thn: Control, els: Control): Control = js.IfExpr(cond, thn, els)
  def Handle(body: Expr): Control = Builtin("handleMonadic", body)

  def Builtin(name: String, args: Expr*): Control = $effekt.call(name, args: _*)

  def Lambda(params: List[JSName], stmts: List[Stmt], ret: Control): Expr =
    js.Lambda(params, js.Block(stmts :+ js.Return(ret)))

  def Function(name: JSName, params: List[JSName], stmts: List[Stmt], ret: Control): Stmt =
    js.Function(name, params, stmts :+ js.Return(ret))

  def asExpr(c: Control): Expr = c
}
