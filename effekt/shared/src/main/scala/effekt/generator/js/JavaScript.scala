package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.*
import effekt.symbols.{ LocalName, Module, Name, NoName, QualifiedName, Symbol, TermSymbol, TypeConstructor, TypeSymbol, Wildcard }
import effekt.util.paths.*
import effekt.{ Compiled, CoreTransformed, symbols }
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import scala.language.implicitConversions

object JavaScript extends Backend {

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(input: CoreTransformed, mainSymbol: TermSymbol)(using C: Context) = {

    assert(input.core.imports.isEmpty, "All dependencies should have been inlined by now.")

    val module = input.mod
    val mainFile = path(module)
    val exports = List(js.Export(JSName("main"), nameRef(mainSymbol)))

    val result = js.PrettyPrinter.format(toJS(input.core, Nil, exports).commonjs)
    Some(Compiled(mainFile, Map(mainFile -> result)))
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(using C: Context) = {
    val module = input.mod

    def shouldExport(sym: Symbol) = sym match {
      // do not export fields, since they are no defined functions
      case _: symbols.Field => false
      // do not export effect operations, since they are translated to field selection as well.
      case _: symbols.Operation => false

      // all others are fine
      case _ => true
    }

    // also search all mains and use last one (shadowing), if any.
    val allMains = input.core.exports.collect {
      case sym if sym.name.name == "main" => js.Export(JSName("main"), nameRef(sym))
    }

    val required = usedImports(input)

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

    C.using(module = input.mod) {
      val result = toJS(input.core, imports, exports).virtual
      Some(js.PrettyPrinter.format(result))
    }
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
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
    case Extern.Def(id, tpe, vps, bps, body) =>
      js.Function(nameDef(id), (vps ++ bps) map externParams, List(js.Return(js.RawExpr(body))))

    case Extern.Include(contents) =>
      js.RawStmt(contents)
  }

  def toJS(b: core.Block)(using Context): js.Expr = b match {
    case BlockVar(v, _, _) =>
      nameRef(v)
    case BlockLit(vps, bps, body) =>
      val (stmts, ret) = toJSStmt(body)
      monadic.Lambda((vps ++ bps) map toJS, stmts, ret) // TODO
    case Member(b, id) =>
      js.Member(toJS(b), memberNameRef(id))
    case Unbox(e)     => toJS(e)
    case New(handler) => toJS(handler)
  }

  def toJS(args: List[Argument])(using Context): List[js.Expr] = args map {
    case b: Block => toJS(b)
    case e: Expr => toJS(e)
  }

  def toJS(expr: core.Expr)(using Context): js.Expr = expr match {
    case Literal((), _) => js.Member($effekt, JSName("unit"))
    case Literal(s: String, _) => JsString(s)
    case literal: Literal => js.RawExpr(literal.value.toString)
    case ValueVar(id, tpe) => nameRef(id)
    case DirectApp(b, targs, args) => js.Call(toJS(b), toJS(args))
    case PureApp(b, targs, args) => js.Call(toJS(b), args map toJS)
    case Select(target, field) => js.Member(toJS(target), memberNameRef(field))
    case Box(b) => toJS(b)
    case Run(s) => monadic.Run(toJSMonadic(s))
  }

  def toJS(handler: core.Implementation)(using Context): js.Expr =
    js.Object(handler.operations.map { case Operation(id, b) => nameDef(id) -> toJS(b) })

  def toJS(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using Context): js.Module = {
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
  def toJSMonadic(s: core.Stmt)(using Context): monadic.Control = s match {
    case core.Val(Wildcard(), binding, body) =>
      monadic.Bind(toJSMonadic(binding), toJSMonadic(body))

    case core.Val(id, binding, body) =>
      monadic.Bind(toJSMonadic(binding), nameDef(id), toJSMonadic(body))

    case core.App(b, targs, args) =>
      monadic.Call(toJS(b), toJS(args))

    case core.If(cond, thn, els) =>
      monadic.If(toJS(cond), toJSMonadic(thn), toJSMonadic(els))

    case core.Return(e) =>
      monadic.Pure(toJS(e))

    case core.Try(body, hs) =>
      monadic.Handle(hs map toJS, toJS(body))

    case core.Region(body) =>
      monadic.Builtin("withRegion", toJS(body))

    case core.Hole =>
      monadic.Builtin("hole")

    case other => toJSStmt(other) match {
      case (Nil, ret) => ret
      case (stmts, ret) => monadic.Call(monadic.Lambda(Nil, stmts, ret), Nil)
    }
  }

  def toJS(d: core.Declaration)(using Context): List[js.Stmt] = d match {
    case core.Data(did, ctors) =>
      ctors.map { ctor => generateConstructor(ctor.asConstructor) }

    case core.Record(did, fields) =>
      List(generateConstructor(did, fields))

    // interfaces are structurally typed at the moment, no need to generate anything.
    case core.Interface(id, operations) =>
      Nil
  }

  def toJS(d: core.Definition)(using Context): js.Stmt = d match {
    case Definition.Def(id, BlockLit(vps, bps, body)) =>
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
  def toJSStmt(s: core.Stmt)(using Context): (List[js.Stmt], monadic.Control) = s match {
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
    case core.Match(sc, clauses, default) =>
      val scrutinee = toJS(sc)

      val sw = js.Switch(js.Member(scrutinee, `tag`), clauses map {
        // f17()
        case (c: symbols.Constructor, block) if c.fields.isEmpty =>
          (tagFor(c), js.Return(js.Call(toJS(block), Nil)))

        // f17.apply(null, sc.__data)
        case (c: symbols.Constructor, block) =>
          (tagFor(c), js.Return(js.MethodCall(toJS(block), JSName("apply"), js.RawExpr("null"), js.Member(scrutinee, `data`))))
      }, None)

      val (stmts, ret) = default.map(toJSStmt).getOrElse((Nil, monadic.Pure(js.RawExpr("null"))))
      (sw :: stmts, ret)


    case other =>
      (Nil, toJSMonadic(other))
  }

  def generateConstructor(ctor: symbols.Constructor): js.Stmt =
    generateConstructor(ctor, ctor.fields)

  def tagFor(c: symbols.Constructor): js.Expr = c.tpe match {
    case TypeConstructor.DataType(name, tparams, constructors) => js.RawExpr(constructors.indexOf(c).toString)
    case TypeConstructor.Record(name, tparams, constructor) => js.RawExpr("0")
    case TypeConstructor.ExternType(name, tparams) => ???
  }

  def generateConstructor(ctor: Symbol, fields: List[Symbol]): js.Stmt = {

    // TODO we really need to stop using records for capabilities in core!
    val tagValue = ctor match {
      case c: symbols.Constructor => tagFor(c)
      case _ => js.RawExpr("0") // this case is only necessary since records are also used to represent capabilities
    }

    val constructor = ctor match {
      case c: symbols.Constructor => c
      case c: symbols.Record => c.constructor
      case _ => ???
    }

    js.Function(
      nameDef(constructor),
      fields.map { f => nameDef(f) },
      List(js.Return(js.Object(List(
        `tag`  -> tagValue,
        `name` -> JsString(ctor.name.name),
        `data` -> js.ArrayLiteral(fields map { f => Variable(nameDef(f)) })
      ) ++ fields.map { f => (nameDef(f), Variable(nameDef(f))) })))
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
