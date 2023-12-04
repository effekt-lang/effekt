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
 * Precondition: we assume that in the core tree all named definitions have been lambda-lifted
 *
 * - objects are not supported, for now
 * - lambda lifting of known functions is essential, since closures are expensive in JS
 */
object TransformerDirect extends Transformer {
  def transformModule(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using DeclarationContext, Context): js.Module =
    given Locals = new Locals(module)
    toJS(module, imports, exports)

  type Continuations = mutable.ArrayBuffer[js.Function]
  def emitContinuation(name: JSName, result: JSName, locals: List[JSName], body: List[js.Stmt])(using K: Continuations): Unit =
    K += js.Function(name, result :: locals, body)

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
      js.Lambda(vps.map(toJS) ++ bps.map(toJS), js.Block(toJS(body)(x => js.Return(x))))
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

  def toJS(s: core.Stmt)(using DC: DeclarationContext, L: Locals, K: Continuations, C: Context): Bind[js.Expr] = s match {

    case Scope(definitions, body) =>
      Bind { k => definitions.flatMap { toJS } ++ toJS(body)(k) }

    case Alloc(id, init, region, body) =>
      val jsRegion = if region == symbols.builtins.globalRegion then js.Member($effekt, JSName("global")) else nameRef(region)
      Bind { k =>
        js.Const(nameDef(id), js.MethodCall(jsRegion, `fresh`, toJS(init))) :: toJS(body)(k)
      }

    // (function () { switch (sc.tag) {  case 0: return f17.apply(null, sc.data) }
    case Match(sc, clauses, default) =>
      Bind { k =>
        val scrutinee = toJS(sc)
        js.Switch(js.Member(scrutinee, `tag`), clauses map {
          case (c, core.BlockLit(_, _, Nil, _, body)) =>
            (tagFor(c), toJS(body)(k) :+ js.Break())
          case (c, core.BlockLit(_, _, vparams, _, body)) =>
            (tagFor(c), {
              // { const [x, y, z] = sc.__data; [[ body ]] }; break
              val const = js.Const(js.Pattern.Array(vparams.map { p => js.Pattern.Variable(nameDef(p.id)) }), js.Member(scrutinee, `data`))
              (const :: toJS(body)(k)) :+ js.Break()
            })
        }, default.map(s => toJS(s)(k))) :: Nil
      }


    // this is the whole reason for the Bind monad
    // [[ val x = bind; body ]](k) =
    //   let x = undefined;
    //   [[bind]](x = []);
    //   [[body]](k)
    case d @ Val(id, binding, body) =>
      // Here we fix the order of arguments
      val free = L.apply(d).toList
      val freeValues = free.collect { case core.Variable.Value(id, tpe) => id }
      val freeBlocks = free.collect { case core.Variable.Block(id, tpe, capt) => id }
      val freeIds = freeValues ++ freeBlocks

      val contId = freshName(s"k_${id.name}") // TODO improve name and prefix current function name
      val result = nameDef(id)

      // the last statement in the binding differs if it is bound to a wildcard
      // ...x = result...    vs.   ...result...
      val (maybeLet, bindingStmts) = id match {
        case Wildcard() => (Nil, toJS(binding)(x => js.ExprStmt(x)))
        case id         => (List(js.Let(nameDef(id), js.Undefined)), toJS(binding)(x => js.Assign(nameRef(id), x)))
      }

      val instrumented = entrypoint(result, contId, freeIds.map(uniqueName), bindingStmts)

      Bind { k =>
        emitContinuation(contId, result, freeIds.map(nameDef), toJS(body)(x => js.Return(x)))
        maybeLet ++ instrumented ++ toJS(body)(k)
      }

    case Var(id, init, cap, body) =>
      Bind { k =>
        js.Const(nameDef(id), js.builtin("fresh", toJS(init))) :: toJS(body)(k)
      }

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
      val freshRegion = js.ExprStmt(js.builtin("freshRegion"))
      val regionCleanup = js.ExprStmt(js.builtin("leaveRegion"))

      val (handlerNames, handlerDefs) = (bps zip hs).map {
        case (param, handler) => (toJS(param), js.Const(toJS(param), toJS(handler, prompt)))
      }.unzip

      // TODO implement properly
      Bind { k => promptDef :: handlerDefs ::: (js.Try(freshRegion :: toJS(body)(k), suspension,
        List(k(js.builtin("handle", js.Variable(prompt), js.Variable(suspension)))), List(regionCleanup)) :: Nil)
      }

    case Try(_, _) =>
      Context.panic("Body of the try is expected to be a block literal in core.")

    case Region(core.BlockLit(_, _, _, List(r), body)) =>
      val suspension = freshName("suspension")
      val region = nameDef(r.id)

      val freshRegion = js.Const(region, js.builtin("freshRegion"))
      val regionCleanup = js.ExprStmt(js.builtin("leaveRegion"))

      Bind { k =>
        js.Try(freshRegion :: toJS(body)(k), suspension,
          List(k(js.builtin("handle", js.Undefined, js.Variable(suspension)))), List(regionCleanup)) :: Nil
      }

    case Region(_) =>
      Context.panic("Body of the region is expected to be a block literal in core.")

    case Hole() =>
      Return(js.builtin("hole"))

    case Get(id, capt, tpe) => Context.panic("Should have been translated to direct style")
    case Put(id, capt, value) =>  Context.panic("Should have been translated to direct style")

  }

  // TODO generate fresh prompt (int)
  //   pass prompt to handler variant of objects, not to others


  def toJS(handler: core.Implementation, prompt: JSName)(using DeclarationContext, Locals, Continuations, Context): js.Expr =
    js.Object(handler.operations.map {
      // (args...cap...) => $effekt.suspend(prompt, (resume) => { ... body ... resume((cap...) => { ... }) ... })
      case Operation(id, tps, cps, vps, bps,
          Some(BlockParam(resume, core.BlockType.Function(_, _, _, List(core.BlockType.Function(_, _, _, bidirectionalTpes, _)), _), _)),
          body) =>
        // add parameters for bidirectional arguments
        val biParams = bidirectionalTpes.map { _ => freshName("cap") }
        val biArgs   = biParams.map { p => js.Variable(p) }

        val lambda = js.Lambda((vps ++ bps).map(toJS) ++ biParams,
          js.Return(js.builtin("suspend_bidirectional", js.Variable(prompt), js.ArrayLiteral(biArgs), js.Lambda(List(nameDef(resume)),
            js.Block(toJS(body)(x => js.Return(x)))))))

        nameDef(id) -> lambda

      // (args...) => $effekt.suspend(prompt, (resume) => { ... BODY ... resume(v) ... })
      case Operation(id, tps, cps, vps, bps, Some(resume), body) =>
        val lambda = js.Lambda((vps ++ bps) map toJS,
          js.Return(js.builtin("suspend", js.Variable(prompt), js.Lambda(List(toJS(resume)), js.Block(toJS(body)(x => js.Return(x)))))))

        nameDef(id) -> lambda

      case Operation(id, tps, cps, vps, bps, None, body) => Context.panic("Effect handler should take continuation")
    })

  def toJS(handler: core.Implementation)(using DeclarationContext, Locals, Continuations, Context): js.Expr =
    js.Object(handler.operations.map {
      case Operation(id, tps, cps, vps, bps, None, body) =>
        nameDef(id) -> js.Lambda((vps ++ bps) map toJS, js.Block(toJS(body)(x => js.Return(x))))
      case Operation(id, tps, cps, vps, bps, Some(k), body) =>
        Context.panic("Object cannot take continuation")
    })

  def toJS(d: core.Definition)(using DC: DeclarationContext, L: Locals, K: Continuations, C: Context): List[js.Stmt] = d match {
    case Definition.Def(id, BlockLit(tps, cps, vps, bps, body)) =>
      List(js.Function(nameDef(id), (vps ++ bps) map toJS, toJS(body)(x => js.Return(x))))

    case Definition.Def(id, block) =>
      List(js.Const(nameDef(id), toJS(block)))

    case Definition.Let(Wildcard(), core.Run(s)) =>
      toJS(s)(x => js.ExprStmt(x))

    case Definition.Let(id, core.Run(s)) =>
      js.Let(nameDef(id), js.Undefined) :: toJS(s)(x => js.Assign(nameRef(id), x))

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
}
