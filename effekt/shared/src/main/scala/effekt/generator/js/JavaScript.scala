package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.*
import effekt.util.paths.*
import effekt.{ Compiled, CoreTransformed, symbols }
import effekt.symbols.{ Symbol, Module, Wildcard }

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import scala.language.implicitConversions

object JavaScript extends Backend {

  case class GeneratorContext(
    // for error messages
    context: Context,

    // for (type) declarations
    declarations: List[Declaration]
  )
  given (using C: GeneratorContext): Context = C.context

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(input: CoreTransformed, mainSymbol: symbols.TermSymbol)(using C: Context) = {

    assert(input.core.imports.isEmpty, "All dependencies should have been inlined by now.")

    val module = input.mod
    val mainFile = path(module)
    val exports = List(js.Export(JSName("main"), nameRef(mainSymbol)))
    given GeneratorContext = GeneratorContext(C, input.core.declarations)

    val result = js.PrettyPrinter.format(toJS(input.core, Nil, exports).commonjs)
    Some(Compiled(mainFile, Map(mainFile -> result)))
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: AllTransformed)(using C: Context) = {
    val module = input.main.mod

    val allDeclarations = input.dependencies.foldLeft(input.main.core.declarations) {
      case (decls, dependency) => decls ++ dependency.core.declarations
    }

    given GeneratorContext = GeneratorContext(C, allDeclarations)

    def shouldExport(sym: Symbol) = sym match {
      // do not export fields, since they are no defined functions
      case _: symbols.Field => false
      // do not export effect operations, since they are translated to field selection as well.
      case _: symbols.Operation => false

      // all others are fine
      case _ => true
    }

    // also search all mains and use last one (shadowing), if any.
    val allMains = input.main.core.exports.collect {
      case sym if sym.name.name == "main" => js.Export(JSName("main"), nameRef(sym))
    }

    val required = usedImports(input.main)

    // this is mostly to import $effekt
    val dependencies = module.dependencies.map {
      d => js.Import.All(JSName(jsModuleName(d.path)), jsModuleFile(d.path))
    }
    val imports = dependencies ++ required.toList.map {
      case (mod, syms) =>
        js.Import.Selective(syms.filter(shouldExport).toList.map(uniqueName), jsModuleFile(mod.path))
    }

    val provided = module.terms.values.flatten.toList.distinct
    val exports = allMains.lastOption.toList ++ provided.collect {
      case sym if shouldExport(sym) => js.Export(nameDef(sym), nameRef(sym))
    }

    C.using(module = input.main.mod) {
      val result = toJS(input.main.core, imports, exports).virtual
      Some(js.PrettyPrinter.format(result))
    }
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / jsModuleFile(m.path)).unixPath

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

  def toJS(b: core.Block)(using GeneratorContext): js.Expr = b match {
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

  def toJS(args: List[Argument])(using GeneratorContext): List[js.Expr] = args map {
    case b: Block => toJS(b)
    case e: Expr => toJS(e)
  }

  def toJS(expr: core.Expr)(using GeneratorContext): js.Expr = expr match {
    case Literal((), _) => js.Member($effekt, JSName("unit"))
    case Literal(s: String, _) => JsString(s)
    case literal: Literal => js.RawExpr(literal.value.toString)
    case ValueVar(id, tpe) => nameRef(id)
    case DirectApp(b, targs, vargs, bargs) => js.Call(toJS(b), toJS(vargs) ++ toJS(bargs))
    case PureApp(b, targs, args) => js.Call(toJS(b), args map toJS)
    case Select(target, field, _) => js.Member(toJS(target), memberNameRef(field))
    case Box(b, _) => toJS(b)
    case Run(s) => monadic.Run(toJSMonadic(s))
  }

  def toJS(handler: core.Implementation)(using GeneratorContext): js.Expr =
    js.Object(handler.operations.map {
      case Operation(id, tps, cps, vps, bps, resume, body) =>
        val (stmts, ret) = toJSStmt(body)
        nameDef(id) -> monadic.Lambda((vps ++ bps ++ resume.toList) map toJS, stmts, ret)
    })

  def toJS(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using GeneratorContext): js.Module = {
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
  def toJSMonadic(s: core.Stmt)(using GeneratorContext): monadic.Control = s match {
    case Val(Wildcard(), binding, body) =>
      monadic.Bind(toJSMonadic(binding), toJSMonadic(body))

    case Val(id, binding, body) =>
      monadic.Bind(toJSMonadic(binding), nameDef(id), toJSMonadic(body))

    case App(b, targs, vargs, bargs) =>
      monadic.Call(toJS(b), toJS(vargs) ++ toJS(bargs))

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

  def toJS(d: core.Definition)(using GeneratorContext): js.Stmt = d match {
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
  def toJSStmt(s: core.Stmt)(using GeneratorContext): (List[js.Stmt], monadic.Control) = s match {
    case Scope(definitions, body) =>
      val (stmts, ret) = toJSStmt(body)
      (definitions.map(toJS) ++ stmts, ret)

    case State(id, init, region, body) if region == symbols.builtins.globalRegion =>
      val (stmts, ret) = toJSStmt(body)
      (js.Const(nameDef(id), js.MethodCall($effekt, `fresh`, toJS(init))) :: stmts, ret)

    case State(id, init, region, body) =>
      val (stmts, ret) = toJSStmt(body)
      (js.Const(nameDef(id), js.MethodCall(nameRef(region), `fresh`, toJS(init))) :: stmts, ret)

    // (function () { switch (sc.tag) {  case 0: return f17.apply(null, sc.data) }
    case Match(sc, clauses, default) =>
      val scrutinee = toJS(sc)

      val sw = js.Switch(js.Member(scrutinee, `tag`), clauses map {
        // f17.apply(null, sc.__data)
        case (c, block) =>
          (tagFor(c), js.Return(js.MethodCall(toJS(block), JSName("apply"), js.RawExpr("null"), js.Member(scrutinee, `data`))))
      }, None)

      val (stmts, ret) = default.map(toJSStmt).getOrElse((Nil, monadic.Pure(js.RawExpr("null"))))
      (sw :: stmts, ret)


    case other =>
      (Nil, toJSMonadic(other))
  }

  // TODO replace this by a lookup in the global declarations
  def tagFor(constructor: Id)(using C: GeneratorContext): js.Expr = {
    val position = C.declarations.collectFirst {
      case Declaration.Data(_, _, cs) if cs.exists(c => c.id == constructor) =>
        cs.indexWhere(c => c.id == constructor)
    }.getOrElse {
      C.context.panic(s"Cannot find constructor ${constructor} in data type declarations.")
    }
    js.RawExpr(position.toString)
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


  // Separate Compilation (Website)
  // ------------------------------

  /**
   * Analyse core to find references to symbols defined in other modules.
   *
   * Necessary for generating the linker code (separate compilation for the web)
   */
  private def usedImports(input: CoreTransformed): Map[Module, Set[Symbol]] = {
    val dependencies = input.mod.dependencies

    // Create a mapping Termsymbol -> Module
    val publicDependencySymbols = dependencies.flatMap {
      m => m.terms.values.flatten.map(sym => (sym : Symbol) -> m)
    }.toMap

    var usedFrom: Map[Module, Set[Symbol]] = Map.empty

    def register(m: Module, sym: Symbol) = {
      val before = usedFrom.getOrElse(m, Set.empty)
      usedFrom = usedFrom.updated(m, before + sym)
    }

    // Traverse tree once more to find all used symbols, defined in other modules.
    def findUsedDependencies(t: Definition) =
      Tree.visit(t) {
        case BlockVar(x, tpe, capt) if publicDependencySymbols.isDefinedAt(x) =>
          register(publicDependencySymbols(x), x)
        case ValueVar(x, tpe) if publicDependencySymbols.isDefinedAt(x) =>
          register(publicDependencySymbols(x), x)
      }

    input.core.definitions.foreach(findUsedDependencies)

    usedFrom
  }
}
