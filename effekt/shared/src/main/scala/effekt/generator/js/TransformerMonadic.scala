package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.{given, *}
import effekt.util.paths.*
import effekt.{ Compiled, CoreTransformed, symbols }
import effekt.symbols.{ Symbol, Module, Wildcard }

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import scala.language.implicitConversions

object TransformerMonadic extends Transformer {

  def transformModule(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using DeclarationContext, Context): js.Module =
    toJS(module, imports, exports)

  def toJS(p: Param): JSName = nameDef(p.id)

  // For externs, do not sanitize anything. We assume the programmer
  // knows what they are doing.
  def externParams(p: Param)(using C: Context): JSName = {
    val name = p.id.name.name
    if (reserved contains name) {
      C.warning(s"Identifier '${name}' is used in an extern function, but is a JS reserved keyword.")
    }
    JSName(name)
  }

  def toJS(e: core.Extern)(using Context): js.Stmt = e match {
    case Extern.Def(id, tps, cps, vps, bps, ret, capt, body) =>
      js.Function(nameDef(id), (vps ++ bps) map externParams, List(js.Return(js.RawExpr(body))))

    case Extern.Include(contents) =>
      js.RawStmt(contents)
  }

  def toJS(b: core.Block)(using DeclarationContext, Context): js.Expr = b match {
    case BlockVar(v, _, _) =>
      nameRef(v)
    case BlockLit(tps, cps, vps, bps, body) =>
      val (stmts, ret) = toJSStmt(body)
      monadic.Lambda((vps ++ bps) map toJS, stmts, ret) // TODO
    case Member(b, id, tpe) =>
      js.Member(toJS(b), memberNameRef(id))
    case Unbox(e)     => toJS(e)
    case New(handler) => toJS(handler)
  }

  def toJS(expr: core.Expr)(using DeclarationContext, Context): js.Expr = expr match {
    case Literal((), _) => js.Member($effekt, JSName("unit"))
    case Literal(s: String, _) => JsString(s)
    case literal: Literal => js.RawExpr(literal.value.toString)
    case ValueVar(id, tpe) => nameRef(id)
    case DirectApp(b, targs, vargs, bargs) => js.Call(toJS(b), vargs.map(toJS) ++ bargs.map(toJS))
    case PureApp(b, targs, args) => js.Call(toJS(b), args map toJS)
    case Select(target, field, _) => js.Member(toJS(target), memberNameRef(field))
    case Box(b, _) => toJS(b)
    case Run(s) => monadic.Run(toJSMonadic(s))
  }

  def toJS(handler: core.Implementation)(using DeclarationContext, Context): js.Expr =
    js.Object(handler.operations.map {
      case Operation(id, tps, cps, vps, bps, resume, body) =>
        val (stmts, ret) = toJSStmt(body)
        nameDef(id) -> monadic.Lambda((vps ++ bps ++ resume.toList) map toJS, stmts, ret)
    })

  def toJS(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using DeclarationContext, Context): js.Module = {
    val name    = JSName(jsModuleName(module.path))
    val externs = module.externs.map(toJS)
    val decls   = module.declarations.flatMap(toJS)
    val stmts   = module.definitions.map(toJS)
    val state   = generateStateAccessors
    js.Module(name, imports, exports, state ++ decls ++ externs ++ stmts)
  }


  /**
   * Translate the statement to a javascript expression in a monadic expression context.
   *
   * Not all statement types can be printed in this context!
   */
  def toJSMonadic(s: core.Stmt)(using DeclarationContext, Context): monadic.Control = s match {
    case Val(Wildcard(), binding, body) =>
      monadic.Bind(toJSMonadic(binding), toJSMonadic(body))

    case Val(id, binding, body) =>
      monadic.Bind(toJSMonadic(binding), nameDef(id), toJSMonadic(body))

    case Var(id, init, cap, body) =>
      val (stmts, ret) = toJSStmt(body)
      monadic.State(nameDef(id), toJS(init), stmts, ret)

    case App(b, targs, vargs, bargs) =>
      monadic.Call(toJS(b), vargs.map(toJS) ++ bargs.map(toJS))

    case Get(id, capt, tpe) => Context.panic("Should have been translated to direct style")
    case Put(id, capt, value) =>  Context.panic("Should have been translated to direct style")

    case If(cond, thn, els) =>
      monadic.If(toJS(cond), toJSMonadic(thn), toJSMonadic(els))

    case Return(e) =>
      monadic.Pure(toJS(e))

    case Try(body, hs) =>
      monadic.Handle(hs map toJS, toJS(body))

    case Region(body) =>
      monadic.Builtin("withRegion", toJS(body))

    case Hole() =>
      monadic.Builtin("hole")

    case other => toJSStmt(other) match {
      case (Nil, ret) => ret
      case (stmts, ret) => monadic.Call(monadic.Lambda(Nil, stmts, ret), Nil)
    }
  }

  def toJS(d: core.Declaration)(using Context): List[js.Stmt] = d match {
    case Data(did, tparams, ctors) =>
      ctors.zipWithIndex.map { case (ctor, index) => generateConstructor(ctor, index) }

    // interfaces are structurally typed at the moment, no need to generate anything.
    case Interface(id, tparams, operations) =>
      Nil
  }

  def toJS(d: core.Definition)(using DeclarationContext, Context): js.Stmt = d match {
    case Definition.Def(id, BlockLit(tps, cps, vps, bps, body)) =>
      val (stmts, jsBody) = toJSStmt(body)
      monadic.Function(nameDef(id), (vps++ bps) map toJS, stmts, jsBody)

    case Definition.Def(id, block) =>
      js.Const(nameDef(id), toJS(block))

    case Definition.Let(Wildcard(), binding) =>
      js.ExprStmt(toJS(binding))

    case Definition.Let(id, binding) =>
      js.Const(nameDef(id), toJS(binding))
  }

  /**
   * Translate the statement in a js "direct-style" statement context.
   *
   * That is, multiple statements that end in one monadic return
   */
  def toJSStmt(s: core.Stmt)(using DeclarationContext, Context): (List[js.Stmt], monadic.Control) = s match {
    case Scope(definitions, body) =>
      val (stmts, ret) = toJSStmt(body)
      (definitions.map(toJS) ++ stmts, ret)

    case Alloc(id, init, region, body) if region == symbols.builtins.globalRegion =>
      val (stmts, ret) = toJSStmt(body)
      (js.Const(nameDef(id), js.MethodCall($effekt, `fresh`, toJS(init))) :: stmts, ret)

    case Alloc(id, init, region, body) =>
      val (stmts, ret) = toJSStmt(body)
      (js.Const(nameDef(id), js.MethodCall(nameRef(region), `fresh`, toJS(init))) :: stmts, ret)

    // (function () { switch (sc.tag) {  case 0: return f17.apply(null, sc.data) }
    case Match(sc, clauses, default) =>
      val scrutinee = toJS(sc)

      val sw = js.Switch(js.Member(scrutinee, `tag`), clauses map {
        // f17.apply(null, sc.__data)
        case (c, block) =>
          (tagFor(c), List(js.Return(js.MethodCall(toJS(block), JSName("apply"), js.RawExpr("null"), js.Member(scrutinee, `data`)))))
      }, None)

      val (stmts, ret) = default.map(toJSStmt).getOrElse((Nil, monadic.Pure(js.RawExpr("null"))))
      (sw :: stmts, ret)


    case other =>
      (Nil, toJSMonadic(other))
  }

}
