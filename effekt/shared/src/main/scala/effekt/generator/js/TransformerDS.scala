package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.{ *, given }
import effekt.core.Variables
import effekt.core.Variables.{ all, bound, free }
import effekt.symbols.{ Module, Symbol, Wildcard }

import scala.collection.mutable


/**
 * Precondition: we assume that the core tree has been lambda-lifted and all anonymous blocks
 *   are bound.
 *
 * - objects are not supported, for now
 */
object TransformerDS {

  type Locals = core.Variables
  def bindingLocals[T](l: Locals)(prog: Locals ?=> T)(using L: Locals): T = {
    prog(using L ++ l)
  }

  type Continuations = mutable.ArrayBuffer[js.Function]
  def emitContinuation(name: JSName, result: JSName, locals: List[JSName], body: List[js.Stmt])(using K: Continuations): Unit =
    K += js.Function(name, result :: locals, body)

  def compile(input: CoreTransformed, mainSymbol: symbols.TermSymbol)(using Context): js.Module =
    val exports = List(js.Export(JSName("main"), nameRef(mainSymbol)))
    given DeclarationContext = new DeclarationContext(input.core.declarations)
    given Locals = core.Variables.empty
    toJS(input.core, Nil, exports)

  def toJS(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using DeclarationContext, Locals, Context): js.Module = {

    given ks: Continuations = mutable.ArrayBuffer.empty

    val name    = JSName(jsModuleName(module.path))
    val externs = module.externs.map(toJS)
    val decls   = module.declarations.flatMap(toJS)
    val stmts   = module.definitions.flatMap(toJS)
    val state   = generateStateAccessors

    js.Module(name, imports, exports, state ++ decls ++ externs ++ stmts ++ ks.toList)
  }

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

  def toJS(b: core.Block)(using DeclarationContext, Locals, Continuations, Context): js.Expr = b match {
    // [[ f ]] = f
    case BlockVar(v, _, _) => nameRef(v)

    // [[ b.m ]] = [[ b ]].m
    case Member(b, id, tpe) => js.Member(toJS(b), memberNameRef(id))

    // [[ unbox e ]] = [[ e ]]
    case Unbox(e)     => toJS(e)

    // [[ new impl ]] = [[ impl ]]
    case New(impl) => toJS(impl)

    // [[ { x => ... } ]] = ERROR
    case BlockLit(tps, cps, vps, bps, body) =>
      Context.panic("Should have been lambda lifted and explicitly bound")
  }

  /**
   * Translation of expressions is trivial
   */
  def toJS(expr: core.Expr)(using DeclarationContext, Locals, Continuations, Context): js.Expr = expr match {
    case Literal((), _) => js.Member($effekt, JSName("unit"))
    case Literal(s: String, _) => JsString(s)
    case literal: Literal => js.RawExpr(literal.value.toString)
    case ValueVar(id, tpe) => nameRef(id)
    case DirectApp(b, targs, vargs, bargs) => js.Call(toJS(b), vargs.map(toJS) ++ bargs.map(toJS))
    case PureApp(b, targs, args) => js.Call(toJS(b), args map toJS)
    case Select(target, field, _) => js.Member(toJS(target), memberNameRef(field))
    case Box(b, _) => toJS(b)
    case Run(s) => toJS(s)(x => js.Return(x)) match {
      case List(js.Return(e)) => e
      case stmts => js.Call(js.Lambda(Nil, js.Block(stmts)), Nil)
    }
  }

  type Bind[T] = (T => js.Stmt) => List[js.Stmt]
  def Return[T](t: T): Bind[T] = k => List(k(t))
  def Bind[T](b: Bind[T]): Bind[T] = b

  def entrypoint(result: JSName, k: JSName, vars: List[JSName], s: List[js.Stmt]): List[js.Stmt] =
    val suspension = freshName("suspension")
    val frame = js.Lambda(List(result), js.Call(js.Variable(k), js.Variable(result) :: vars.map(js.Variable.apply)))
    List(js.Try(s, suspension, List(js.Throw(js.builtin("push",js.Variable(suspension), frame)))))

//   List(js.Try(s, JSName("k"), List(RawStmt(
//    s""" /*
//       |  * result: ${uniqueName(result)}
//       |  * free value variables: ${vars.values.map(uniqueName).mkString(", ")}
//       |  * free block variables: ${vars.blocks.map(uniqueName).mkString(", ")}
//       |  */
//       |""".stripMargin))))

  def toJS(s: core.Stmt)(using DC: DeclarationContext, L: Locals, K: Continuations, C: Context): Bind[js.Expr] = s match {

    case Scope(definitions, body) =>
      var locals = L
      given Locals = locals

      val defs = definitions.flatMap {
        case d : Definition.Def => Context.panic("Local definitions should have been lambda lifted already.")
        case d : Definition.Let =>
          val stmts = toJS(d)
          locals = locals ++ bound(d)
          stmts
      }
      Bind { k => defs ++ toJS(body)(k) }

    case Alloc(id, init, region, body) if region == symbols.builtins.globalRegion =>
      //      val (stmts, ret) = toJS(body)
      //      (js.Const(nameDef(id), js.MethodCall($effekt, `fresh`, toJS(init))) :: stmts, ret)
      Context.panic("Not implemented yet")

    case Alloc(id, init, region, body) =>
      //      val (stmts, ret) = toJS(body)
      //      (js.Const(nameDef(id), js.MethodCall(nameRef(region), `fresh`, toJS(init))) :: stmts, ret)
      Context.panic("Not implemented yet")

    // (function () { switch (sc.tag) {  case 0: return f17.apply(null, sc.data) }
    case Match(sc, clauses, default) =>
      Context.panic("Not implemented yet")
      //      val scrutinee = toJS(sc)
      //
      //      val sw = js.Switch(js.Member(scrutinee, `tag`), clauses map {
      //        // f17.apply(null, sc.__data)
      //        case (c, block) =>
      //          (tagFor(c), js.Return(js.MethodCall(toJS(block), JSName("apply"), js.RawExpr("null"), js.Member(scrutinee, `data`))))
      //      }, None)
      //
      //      val (stmts, ret) = default.map(toJSStmt).getOrElse((Nil, monadic.Pure(js.RawExpr("null"))))
      //      (sw :: stmts, ret)


    // this is the whole reason for the Bind monad
    // [[ val x = bind; body ]](k) =
    //   let x = undefined;
    //   [[bind]](x = []);
    //   [[body]](k)
    case Val(id, binding, body) =>
      val free = Variables.free(body) intersect L
      // Here we fix the order of arguments
      val freeValues = free.values.toList
      val freeBlocks = free.blocks.toList
      val contId = freshName("k") // TODO improve name and prefix current function name
      val result = nameDef(id)

      // the last statement in the binding differs if it is bound to a wildcard
      // ...x = result...    vs.   ...result...
      val bindingStmts = id match {
        case Wildcard() => toJS(binding)(x => js.ExprStmt(x))
        case id         => toJS(binding)(x => js.Assign(nameRef(id), x))
      }

      val instrumented = entrypoint(result, contId, (freeValues ++ freeBlocks).map(uniqueName), bindingStmts)

      bindingLocals(L ++ Variables.value(id)) {
        Bind { k =>
          emitContinuation(contId, result, (freeValues ++ freeBlocks).map(nameDef), toJS(body)(x => js.Return(x)))
          instrumented ++ toJS(body)(k)
        }
      }

    case Var(id, init, cap, body) =>
      Context.panic("Not implemented yet")

    case App(b, targs, vargs, bargs) =>
      Return(js.Call(toJS(b), vargs.map(toJS) ++ bargs.map(toJS)))

    case If(cond, thn, els) =>
      Bind { k => List(js.If(toJS(cond), js.MaybeBlock(toJS(thn)(k)), js.MaybeBlock(toJS(els)(k)))) }

    case Return(e) =>
      Return(toJS(e))

    // const prompt = $effekt.freshPrompt()
    // const exc = { raise: ... }; try { }
    case Try(core.BlockLit(_, _, _, bps, body), hs) =>
      val suspension = freshName("suspension")
      val prompt = freshName("prompt")

      val promptDef = js.Const(prompt, js.builtin("freshPrompt"))

      val (handlerNames, handlerDefs) = (bps zip hs).map {
        case (param, handler) => (toJS(param), js.Const(toJS(param), toJS(handler, prompt)))
      }.unzip

      // TODO implement properly
      Bind { k => promptDef :: handlerDefs ++ List(js.Try(bindingLocals(all(bps, bound)) { toJS(body)(k) }, suspension,
        List(k(js.builtin("handle", js.Variable(prompt), js.Variable(suspension))))))
      }

    case Try(_, _) =>
      Context.panic("Body of the try is expected to be a block literal in core.")

    case Region(body) =>
      Context.panic("Not implemented yet")

    case Hole() =>
      Context.panic("Not implemented yet")

    case Get(id, capt, tpe) => Context.panic("Should have been translated to direct style")
    case Put(id, capt, value) =>  Context.panic("Should have been translated to direct style")

  }

  // TODO generate fresh prompt (int)
  //   pass prompt to handler variant of objects, not to others


  def toJS(handler: core.Implementation, prompt: JSName)(using DeclarationContext, Locals, Continuations, Context): js.Expr =
    js.Object(handler.operations.map {
      // () => $effekt.suspend(this, (resume_730) => { return $effekt.unit; })
      case Operation(id, tps, cps, vps, bps, Some(resume), body) =>
        val lambda = js.Lambda((vps ++ bps) map toJS, bindingLocals(all(vps, bound) ++ all(bps, bound) ++ bound(resume)) {
          js.Return(js.builtin("suspend", js.Variable(prompt), js.Lambda(List(toJS(resume)), js.Block(toJS(body)(x => js.Return(x))))))
        })

        nameDef(id) -> lambda

      case Operation(id, tps, cps, vps, bps, None, body) => Context.panic("Effect handler should take continuation")
    })

  def toJS(handler: core.Implementation)(using DeclarationContext, Locals, Continuations, Context): js.Expr =
    js.Object(handler.operations.map {
      case Operation(id, tps, cps, vps, bps, None, body) =>
        nameDef(id) -> js.Lambda((vps ++ bps) map toJS, bindingLocals(all(vps, bound) ++ all(bps, bound)) {
          js.Block(toJS(body)(x => js.Return(x)))
        })
      case Operation(id, tps, cps, vps, bps, Some(k), body) =>
        Context.panic("Object cannot take continuation")
    })

  def toJS(d: core.Definition)(using DC: DeclarationContext, L: Locals, K: Continuations, C: Context): List[js.Stmt] = d match {
    case Definition.Def(id, BlockLit(tps, cps, vps, bps, body)) =>
      List(js.Function(nameDef(id), (vps ++ bps) map toJS, bindingLocals(all(vps, bound) ++ all(bps, bound)) {
        toJS(body)(x => js.Return(x))
      }))

    case Definition.Def(id, block) =>
      List(js.Const(nameDef(id), toJS(block)))

    case Definition.Let(Wildcard(), core.Run(s)) =>
      toJS(s)(x => js.ExprStmt(x))

    case Definition.Let(Wildcard(), binding) =>
      List(js.ExprStmt(toJS(binding)))

    case Definition.Let(id, binding) =>
      List(js.Const(nameDef(id), toJS(binding)))
  }

  def toJS(d: core.Declaration)(using Context): List[js.Stmt] = d match {
    case Data(did, tparams, ctors) =>
      ctors.zipWithIndex.map { case (ctor, index) => generateConstructor(ctor, index) }

    // interfaces are structurally typed at the moment, no need to generate anything.
    case Interface(id, tparams, operations) =>
      Nil
  }

  // Representation of Data / Codata
  // ----
  def tagFor(constructor: Id)(using D: DeclarationContext, C: Context): js.Expr = {
    js.RawExpr(D.getConstructorTag(constructor).toString)
  }

  def generateConstructor(constructor: Constructor, tagValue: Int): js.Stmt = {
    val fields = constructor.fields
    js.Function(
      nameDef(constructor.id),
      fields.map { f => nameDef(f.id) },
      List(js.Return(js.Object(List(
        `tag`  -> js.RawExpr(tagValue.toString),
        `name` -> JsString(constructor.id.name.name),
        `data` -> js.ArrayLiteral(fields map { f => Variable(nameDef(f.id)) })
      ) ++ fields.map { f => (nameDef(f.id), Variable(nameDef(f.id))) })))
    )
  }

  // const $getOp = "get$1234"
  // const $putOp = "put$7554"
  def generateStateAccessors: List[js.Stmt] = {
    val getter = Const(JSName("$getOp"), JsString(nameDef(symbols.builtins.TState.get).name))
    val setter = Const(JSName("$putOp"), JsString(nameDef(symbols.builtins.TState.put).name))

    List(getter, setter)
  }

  // Names
  // -----

  val reserved = List(
    // reserved words (according to https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#keywords)
    "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else", "export",
    "extends", "false", "finally", "for", "function", "if", "import", "in", "instanceof", "let", "new", "null", "return",
    "static", "super", "switch", "this", "throw", "true", "try", "typeof", "var", "void", "while", "with", "yield",

    // future reserved words
    "enum", "implements", "interface", "package", "private", "protected", "public",

    // identifiers with special meanings
    "get", "set", "arguments", "async", "eval",

    // special names in CommonJS module systems
    "module", "exports", "require",

    // other special names
    "window", "document", "alert", "console", "this"
  )

  def jsEscape(name: String): String = if (reserved contains name) "$" + name else name

  def jsModuleName(path: String): String = "$" + path.replace('/', '_').replace('-', '_')

  def jsModuleFile(path: String): String = path.replace('/', '_').replace('-', '_') + ".js"

  val `fresh` = JSName("fresh")
  val `tag` = JSName("__tag")
  val `name` = JSName("__name")
  val `data` = JSName("__data")

  def nameDef(id: Symbol): JSName = uniqueName(id)

  def uniqueName(sym: Symbol): JSName = JSName(jsEscape(sym.name.toString + "_" + sym.id))

  def nameRef(id: Symbol)(using C: Context): js.Expr = Variable(uniqueName(id))

  // name references for fields and methods
  def memberNameRef(id: Symbol): JSName = uniqueName(id)

  def freshName(s: String): JSName =
    JSName(s + Symbol.fresh.next())

}
