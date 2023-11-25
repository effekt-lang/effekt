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
 * - lambda lifting of known functions is essential, since closures are expensive in JS
 */
object TransformerDS {

  type Continuations = mutable.ArrayBuffer[js.Function]
  def emitContinuation(name: JSName, result: JSName, locals: List[JSName], body: List[js.Stmt])(using K: Continuations): Unit =
    K += js.Function(name, result :: locals, body)

  def compile(input: CoreTransformed, mainSymbol: symbols.TermSymbol)(using Context): js.Module =
    val exports = List(js.Export(JSName("main"), nameRef(mainSymbol)))
    given DeclarationContext = new DeclarationContext(input.core.declarations)

    val lifted = LambdaLifting.lift(input.core)
    given Locals = new Locals(lifted)
    toJS(lifted, Nil, exports)

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

      val defs = definitions.flatMap {
        case d : Definition.Def =>
          //Context.panic("Local definitions should have been lambda lifted already.")
          val stmts = toJS(d)
          stmts
        case d : Definition.Let =>
          val stmts = toJS(d)
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
    case d @ Val(id, binding, body) =>
      val free = L.apply(d)
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

      Bind { k =>
        emitContinuation(contId, result, (freeValues ++ freeBlocks).map(nameDef), toJS(body)(x => js.Return(x)))
        instrumented ++ toJS(body)(k)
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
      Bind { k => promptDef :: handlerDefs ++ List(js.Try(toJS(body)(k), suspension,
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


/**
 * Free variable computation that annotates Val and Def trees with the free variables of
 * their continuation / body, respectively.
 *
 * Use like:
 *
 *     val locals = new Locals(mod);
 *     ...
 *     locals(myValDef) // Variables(Set(), Set(f17))
 *
 * WARNING: the mapping is performed by object identity, so rewriting the tree looses the annotations.
 * WARNING: since the local-context is lost, do NOT use it by querying on demand (e.g. `locals.query(myTree)`)
 */
class Locals(mod: ModuleDecl)(using Context) extends core.Tree.Query[Variables, Variables] {

  // DB
  // --
  import effekt.context.{Annotations, Annotation}

  private val LocallyFree = Annotation[core.Tree, core.Variables](
    "LocallyFree",
    "the free variables of the tree, only considering local and not toplevel definitions"
  )

  private val db = Annotations.empty

  def apply(t: core.Val | core.Definition): core.Variables = db.apply(LocallyFree, t)

  // Monoid
  // ------
  def empty = Variables.empty
  def combine = _ ++ _

  // Scoping
  // -------
  def freeBlock(id: Id)(using L: Variables): Variables = Variables.block(id) intersect L
  def freeValue(id: Id)(using L: Variables): Variables = Variables.value(id) intersect L
  def binding(bound: Variables)(prog: Variables ?=> Variables)(using L: Variables): Variables =
    prog(using L ++ bound) -- bound

  override def pure(using Variables) = {
    case core.ValueVar(id, annotatedType) => freeValue(id)
  }

  override def block(using Variables) = {
    case core.BlockVar(id, annotatedTpe, annotatedCapt) => freeBlock(id)
    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      binding(all(vparams, bound) ++ all(bparams, bound)) { query(body) }
  }

  override def operation(using Variables) = {
    case core.Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      binding(all(vparams, bound) ++ all(bparams, bound) ++ all(resume, bound)) { query(body) }
  }

  override def defn(using Variables) = {
    case d @ core.Definition.Def(id, block) =>
      val freeInDefinition = binding(bound(d)) { query(block) }
      // we annotate free variables for each definition (Def)
      db.update(LocallyFree, d, freeInDefinition)
      freeInDefinition
  }

  override def stmt(using Variables) = {
    case Stmt.Scope(defs, body) =>
      var stillFree = Variables.empty
      var boundSoFar = Variables.empty
      defs.foreach { d =>
        boundSoFar = boundSoFar ++ bound(d)
        stillFree = stillFree ++ binding(boundSoFar) { query(d) }
      }
      stillFree ++ binding(boundSoFar) { query(body) }

    case d @ Stmt.Val(id, rhs, body) =>
      query(rhs) ++ binding(Variables.value(id)) {
        // we annotate the free variables of the continuation
        val freeInBody = query(body)
        db.update(LocallyFree, d, freeInBody)
        freeInBody
      }

    case core.Alloc(id, init, region, body) => query(init) ++ freeBlock(region) ++ binding(Variables.block(id)) { query(body) }
    case core.Var(id, init, capture, body) => query(init) ++ binding(Variables.block(id)) { query(body) }
    case core.Get(id, annotatedCapt, annotatedTpe) => freeBlock(id)
    case core.Put(id, annotatedCapt, value) => freeBlock(id)
  }

  // Initialize
  // ----------
  mod.definitions.foreach(d => query(d)(using Variables.empty))

  // maps block ids to their transitive closure
  val transitiveClosure: mutable.Map[Id, Variables] = mutable.Map.empty

  // compute transitive closure
  val freeVariablesOfDefs = db.annotationsAt(LocallyFree).collect {
    case (Annotations.Key(core.Definition.Def(id, b: core.BlockLit)), vars) => id -> vars
  }

  // saturate free variables transitively
  def resolveFreeVariables(vars: Variables): Variables =
    var transitiveVars = Variables(vars.values, Set.empty)

    vars.blocks.foreach { id =>
      transitiveVars = transitiveVars ++ transitiveClosure.getOrElseUpdate(id,
        // this is ok, since local definitions cannot be mutual at the moment (otherwise this will not terminate)
        resolveFreeVariables(freeVariablesOfDefs.getOrElse(id, Variables.block(id))))
    }
    transitiveVars

  freeVariablesOfDefs.foreach {
    case (id, vars) => transitiveClosure.getOrElseUpdate(id, resolveFreeVariables(vars))
  }
}

class LambdaLifting(m: core.ModuleDecl)(using Context) extends core.Tree.Rewrite {

  val locals = Locals(m)

  /**
   * fixes the order of free variables, can vary from compilation to compilation
   */
  case class Info(values: List[(Id, core.ValueType)], blocks: List[(Id, core.BlockType, core.Captures)]) {
    def valueParams: List[core.ValueParam] = values.map { case (id, tpe) => core.ValueParam(id, tpe) }
    def blockParams: List[core.BlockParam] = blocks.map { case (id, tpe, capt) => core.BlockParam(id, tpe) }
    def valueArgs   = values.map { case (id, tpe) => core.ValueVar(id, tpe) }
    def blockArgs   = blocks.map { case (id, tpe, capt) => core.BlockVar(id, tpe, capt) }
  }
  val infos = locals.transitiveClosure.map {
    // TODO maybe we need typed free variables, instead of translating here again. Or remember the core types for symbols.
    case (id, vars) => (id, Info(
      vars.values.toList.map { x => (x, core.Transformer.transform(Context.valueTypeOf(x))) },
      vars.blocks.toList.map { f => (f,
        core.Transformer.transform(Context.blockTypeOf(f)),
        core.Transformer.transform(Context.captureOf(f.asBlockSymbol)))
      }
    ))
  }
  val lifted: mutable.ListBuffer[core.Definition] = mutable.ListBuffer.empty

  // only needs adaptation if it is a closure
  def needsCallsiteAdaptation(id: Id) = infos.get(id) match {
    case Some(vars) => vars != Variables.empty
    case None => false
  }

  override def stmt = {
    case core.Scope(defs, body) =>
      core.Scope(defs.flatMap {
        // we lift named local definitions to the toplevel
        case Definition.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
          lifted.append(Definition.Def(id, BlockLit(tparams, cparams, vparams ++ infos(id).valueParams, bparams ++ infos(id).blockParams, rewrite(body))))
          Nil
        case other => List(rewrite(other))
      }, rewrite(body))

    case core.App(b: BlockVar, targs, vargs, bargs) if needsCallsiteAdaptation(b.id) =>
      core.App(b, targs, vargs ++ infos(b.id).valueArgs, bargs ++ infos(b.id).blockArgs)
  }

  override def block = {
    // Here we now need to eta expand
    case core.BlockVar(id, tpe, capt) if needsCallsiteAdaptation(id) =>
      println(id)
      ???
  }

  override def expr = {
    case core.DirectApp(b: BlockVar, targs, vargs, bargs) if needsCallsiteAdaptation(b.id) =>
      core.DirectApp(b, targs, vargs ++ infos(b.id).valueArgs, bargs ++ infos(b.id).blockArgs)
    case core.PureApp(b: BlockVar, targs, vargs) if needsCallsiteAdaptation(b.id) =>
      core.PureApp(b, targs, vargs ++ infos(b.id).valueArgs)
  }
}
object LambdaLifting {
  def lift(m: core.ModuleDecl)(using Context): core.ModuleDecl =
    val lifting = LambdaLifting(m)
    val transformed = lifting.rewrite(m)
    transformed.copy(definitions = transformed.definitions ++ lifting.lifted)
}
