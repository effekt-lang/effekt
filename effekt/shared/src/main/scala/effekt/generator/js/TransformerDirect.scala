package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.{ *, given }
import effekt.core.Variables
import effekt.core.Variables.{ all, bound, free }
import effekt.core.substitutions.{ Substitution, substitute }
import effekt.symbols.{ Module, Symbol, Wildcard }

import scala.collection.mutable

/**
 * Precondition: we assume that in the core tree all named definitions have been lambda-lifted
 *
 * - objects are not supported, for now
 * - lambda lifting of known functions is essential, since closures are expensive in JS
 */
object TransformerDirect extends Transformer {

  override val jsFeatureFlags: List[String] = List("jsDirect", "js", FeatureFlag.simpleAtom)

  /**
   * Aggregates the contextual information required by the transformation
   */
  case class TransformerContext(
    // the toplevel declarations, used to generate pattern matches
    declarations: DeclarationContext,
    // free variables used to generate continuations
    locals: Locals,
    // continuations emitted by the transformer
    continuations: Continuations,
    // currently, lexically enclosing functions and their parameters (used to determine whether a call is recursive)
    enclosingFunctions: Map[Id, List[core.Variable]],
    // used to register functions that are recognized as tail recursive
    tailCalled: mutable.Set[Id],
    // the usual compiler context
    compiler: Context
  ) {
    def binding[T](fun: Id, params: List[core.Variable])(body: TransformerContext ?=> T): T =
      body(using this.copy(enclosingFunctions = enclosingFunctions.updated(fun, params)))
    def clearingScope[T](body: TransformerContext ?=> T): T =
      body(using this.copy(enclosingFunctions = Map.empty))
  }
  // auto extractors
  given (using C: TransformerContext): DeclarationContext = C.declarations
  given (using C: TransformerContext): Context = C.compiler

  def run(body: js.Expr): js.Stmt =
    js.Return(body)
    //    // return $effekt.run(() => body)
    //    js.Return(builtin("run", js.Lambda(Nil, js.Return(body))))

  def transformModule(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using DeclarationContext, Context): js.Module =
    given Locals = new Locals(module)
    toJS(module, imports, exports)

  type Continuations = mutable.ArrayBuffer[js.Function]
  def emitContinuation(name: JSName, result: JSName, locals: List[JSName], body: List[js.Stmt])(using C: TransformerContext): Unit =
    C.continuations += js.Function(name, result :: locals, body)

  def toJS(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using D: DeclarationContext, L: Locals, C: Context): js.Module = {

    given ks: Continuations = mutable.ArrayBuffer.empty
    given TransformerContext = TransformerContext(D, L, ks, Map.empty, mutable.Set.empty, C)

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

  def toJS(e: core.Extern)(using TransformerContext): js.Stmt = e match {
    case Extern.Def(id, tps, cps, vps, bps, ret, capt, bodies) =>
      bodies.forFeatureFlags(jsFeatureFlags).getOrElse{ ??? /* TODO insert hole */ } match {
        case ExternBody.StringExternBody(_, body) =>
          js.Function(nameDef(id), (vps ++ bps) map externParams, List(js.Return(toJS(body))))
        case ExternBody.EffektExternBody(_, body) => sys error "Effekt extern body should have been removed"
      }

    case Extern.Include(ff, contents) if ff.matches(jsFeatureFlags) =>
      js.RawStmt(contents)

    case Extern.Include(_, _) => js.RawStmt("") // ignore, not meant for us
  }

  def toJS(t: Template[Pure])(using TransformerContext): js.Expr = js.RawExpr(t.strings, t.args.map(toJS))

  def toJS(b: core.Block)(using C: TransformerContext): js.Expr = b match {
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
      js.Lambda(vps.map(toJS) ++ bps.map(toJS), C.clearingScope { js.Block(toJS(body)(Continuation.Return)) })
  }

  /**
   * Translation of expressions is trivial
   */
  def toJS(expr: core.Expr)(using TransformerContext): js.Expr = expr match {
    case Literal((), _) => $effekt.field("unit")
    case Literal(s: String, _) => JsString(s)
    case literal: Literal => js.RawExpr(literal.value.toString)
    case ValueVar(id, tpe) => nameRef(id)
    case DirectApp(b, targs, vargs, bargs) => js.Call(toJS(b), vargs.map(toJS) ++ bargs.map(toJS))
    case PureApp(b, targs, vargs) => js.Call(toJS(b), vargs map toJS)
    case Make(tpe, tag, vargs) => js.Call(nameRef(tag), vargs map toJS)
    case Select(target, field, _) => js.Member(toJS(target), memberNameRef(field))
    case Box(b, _) => toJS(b)
    case Run(s) => toJS(s)(Continuation.Return) match {
      case List(js.Return(e)) => e
      case stmts => js.Call(js.Lambda(Nil, js.Block(stmts)), Nil)
    }
  }

  enum Continuation {
    case Return
    case Ignore
    case Assign(id: Id)

    def apply(result: js.Expr): js.Stmt = this match {
      case Continuation.Return     => js.Return(result)
      case Continuation.Ignore     => js.ExprStmt(result)
      case Continuation.Assign(id) => js.Assign(nameRef(id), result)
    }
  }

  type Bind = Continuation => List[js.Stmt]
  def Return(result: js.Expr): Bind = k => List(k(result))
  def Bind(b: Bind): Bind = b

  def entrypoint(result: JSName, k: JSName, vars: List[JSName], s: List[js.Stmt]): List[js.Stmt] =
    val suspension = freshName("suspension")
    val frame = js.Lambda(List(result), js.Call(js.Variable(k), js.Variable(result) :: vars.map(js.Variable.apply)))
    List(js.Try(s, suspension, List(js.Return($effekt.call("push",js.Variable(suspension), frame)))))

  def toJS(s: core.Stmt)(using C: TransformerContext): Bind = s match {

    case Scope(definitions, body) =>
      Bind { k => definitions.flatMap { toJS } ++ toJS(body)(k) }

    case Alloc(id, init, region, body) =>
      val jsRegion = if region == symbols.builtins.globalRegion then $effekt.field("global") else nameRef(region)
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
      val free = C.locals(d).toList
      val freeValues = free.collect { case core.Variable.Value(id, tpe) => id }
      val freeBlocks = free.collect { case core.Variable.Block(id, tpe, capt) => id }
      val freeIds = freeValues ++ freeBlocks

      val contId = freshName(s"k_${id.name}") // TODO improve name and prefix current function name
      val result = nameDef(id)

      // the last statement in the binding differs if it is bound to a wildcard
      // ...x = result...    vs.   ...result...
      val (maybeLet, bindingStmts) = id match {
        case Wildcard() => (Nil, toJS(binding)(Continuation.Ignore))
        case id         => (List(js.Let(nameDef(id), js.Undefined)), toJS(binding)(Continuation.Assign(id)))
      }

      val instrumented = entrypoint(result, contId, freeIds.map(uniqueName), bindingStmts)

      Bind { k =>
        emitContinuation(contId, result, freeIds.map(nameDef), C.clearingScope { toJS(body)(Continuation.Return) })
        maybeLet ++ instrumented ++ toJS(body)(k)
      }

    case Var(id, init, cap, body) =>
      Bind { k =>
        js.Const(nameDef(id), $effekt.call("fresh", toJS(init))) :: toJS(body)(k)
      }

    // obviously recursive calls
    case App(b : BlockVar, targs, vargs, bargs) if C.enclosingFunctions.isDefinedAt(b.id) =>
      Bind {
        // Tail call! (cannot be supported like this)
        // [[ rec(foo, bar) ]] =  { x = foo; y = bar; continue rec }
        case Continuation.Return =>
          C.tailCalled += b.id
          // continue
          val params = C.enclosingFunctions(b.id)
          val stmts = mutable.ListBuffer.empty[js.Stmt]

          stmts.append(js.RawStmt("/* prepare tail call */"))

          // to prevent accidentally recursive bindings like `x = () => x`, we need to see which parameters occur in
          // the arguments
          val freeVars = (vargs.flatMap(Variables.free) ++ bargs.flatMap(Variables.free)).toSet.intersect(params.toSet)
          val valueSubst = freeVars.collect { case core.Variable.Value(id, tpe) =>
            val tmp = Id(s"tmp")
            stmts.append(js.Const(uniqueName(tmp), nameRef(id)))
            id -> core.Pure.ValueVar(tmp, tpe)
          }
          val blockSubst = freeVars.collect { case core.Variable.Block(id, tpe, capt) =>
            val tmp = Id(s"tmp")
            stmts.append(js.Const(uniqueName(tmp), nameRef(id)))
            id -> core.BlockVar(tmp, tpe, capt)
          }

          given Substitution = Substitution(Map.empty, Map.empty, valueSubst.toMap, blockSubst.toMap)
          val args = vargs.map(v => toJS(substitute(v))) ++ bargs.map(b => toJS(substitute(b)))

          (params zip args) foreach {
            case (param, arg) => stmts.append(js.Assign(nameRef(param.id), arg))
          }

          stmts.append(js.Continue(Some(uniqueName(b.id))))
          stmts.toList
        case k => List(k(js.Call(toJS(b), vargs.map(toJS) ++ bargs.map(toJS))))
      }

    case App(b, targs, vargs, bargs) => Return(js.Call(toJS(b), vargs.map(toJS) ++ bargs.map(toJS)))

    case If(cond, thn, els) =>
      Bind { k => List(js.If(toJS(cond), js.MaybeBlock(toJS(thn)(k)), js.MaybeBlock(toJS(els)(k)))) }

    case Return(e) =>
      Return(toJS(e))

    // const prompt = $effekt.freshPrompt()
    // const exc = { raise: ... }; try { }
    case Try(core.BlockLit(_, _, _, bps, body), hs) =>
      val suspension = freshName("suspension")
      val prompt = freshName("prompt")

      val promptDef = js.Const(prompt, $effekt.call("freshPrompt"))
      val freshRegion = js.ExprStmt($effekt.call("freshRegion"))
      val regionCleanup = js.ExprStmt($effekt.call("leaveRegion"))

      val (handlerNames, handlerDefs) = (bps zip hs).map {
        case (param, handler) => (toJS(param), js.Const(toJS(param), toJS(handler, prompt)))
      }.unzip

      Bind { k => promptDef :: handlerDefs ::: (js.Try(freshRegion :: toJS(body)(k), suspension,
        List(k($effekt.call("handle", js.Variable(prompt), js.Variable(suspension)))), List(regionCleanup)) :: Nil)
      }

    case Try(_, _) =>
      Context.panic("Body of the try is expected to be a block literal in core.")

    case Region(core.BlockLit(_, _, _, List(r), body)) =>
      val suspension = freshName("suspension")
      val region = nameDef(r.id)

      val freshRegion = js.Const(region, $effekt.call("freshRegion"))
      val regionCleanup = js.ExprStmt($effekt.call("leaveRegion"))

      Bind { k =>
        js.Try(freshRegion :: toJS(body)(k), suspension,
          List(k($effekt.call("handle", js.Undefined, js.Variable(suspension)))), List(regionCleanup)) :: Nil
      }

    case Region(_) =>
      Context.panic("Body of the region is expected to be a block literal in core.")

    case Hole() =>
      Return($effekt.call("hole"))

    case Get(id, capt, tpe) => Context.panic("Should have been translated to direct style")
    case Put(id, capt, value) =>  Context.panic("Should have been translated to direct style")

  }

  def toJS(handler: core.Implementation, prompt: JSName)(using C: TransformerContext): js.Expr =
    js.Object(handler.operations.map {
      // (args...cap...) => $effekt.suspend(prompt, (resume) => { ... body ... resume((cap...) => { ... }) ... })
      case Operation(id, tps, cps, vps, bps,
          Some(BlockParam(resume, core.BlockType.Function(_, _, _, List(core.BlockType.Function(_, _, _, bidirectionalTpes, _)), _), _)),
          body) =>
        // add parameters for bidirectional arguments
        val biParams = bidirectionalTpes.map { _ => freshName("cap") }
        val biArgs   = biParams.map { p => js.Variable(p) }

        val lambda = js.Lambda((vps ++ bps).map(toJS) ++ biParams,
          js.Return($effekt.call("suspend_bidirectional", js.Variable(prompt), js.ArrayLiteral(biArgs), js.Lambda(List(nameDef(resume)),
            C.clearingScope { js.Block(toJS(body)(Continuation.Return)) }))))

        nameDef(id) -> lambda

      // (args...) => $effekt.suspend(prompt, (resume) => { ... BODY ... resume(v) ... })
      case Operation(id, tps, cps, vps, bps, Some(resume), body) =>
        val lambda = js.Lambda((vps ++ bps) map toJS,
          js.Return($effekt.call("suspend", js.Variable(prompt),
            js.Lambda(List(toJS(resume)), C.clearingScope {  js.Block(toJS(body)(Continuation.Return)) }))))

        nameDef(id) -> lambda

      case Operation(id, tps, cps, vps, bps, None, body) => Context.panic("Effect handler should take continuation")
    })

  def toJS(handler: core.Implementation)(using C: TransformerContext): js.Expr =
    js.Object(handler.operations.map {
      case Operation(id, tps, cps, vps, bps, None, body) =>
        nameDef(id) -> js.Lambda((vps ++ bps) map toJS, C.clearingScope { js.Block(toJS(body)(Continuation.Return)) })
      case Operation(id, tps, cps, vps, bps, Some(k), body) =>
        Context.panic("Object cannot take continuation")
    })

  def toJS(d: core.Definition)(using C: TransformerContext): List[js.Stmt] = d match {
    case d @ Definition.Def(id, BlockLit(tps, cps, vps, bps, body)) =>
      C.binding (id, vps.flatMap(Variables.bound) ++ bps.flatMap(Variables.bound)) {
        val translatedBody = toJS(body)(Continuation.Return)
        val isRecursive = C.tailCalled.contains(id)

        if (isRecursive) {
          // function ID(params) { ID : while(true) { BODY } }
          List(js.Function(nameDef(id), (vps ++ bps) map toJS,
            List(js.While(RawExpr("true"), translatedBody, Some(uniqueName(id))))))
        } else {
          // function ID(params) { BODY }
          List(js.Function(nameDef(id), (vps ++ bps) map toJS, translatedBody))
        }
      }

    case Definition.Def(id, block) =>
      List(js.Const(nameDef(id), toJS(block)))

    case Definition.Let(Wildcard(), core.Run(s)) =>
      toJS(s)(Continuation.Ignore)

    case Definition.Let(id, core.Run(s)) =>
      js.Let(nameDef(id), js.Undefined) :: toJS(s)(Continuation.Assign(id))

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
