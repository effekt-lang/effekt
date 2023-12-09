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
 * Parent of the monadic and direct-style transformer.
 *
 * Shares generic JS specific definitions
 */
trait Transformer {

  def transformModule(module: core.ModuleDecl, imports: List[js.Import], exports: List[js.Export])(using DeclarationContext, Context): js.Module

  def run(body: js.Expr): js.Stmt

  /**
   * Entrypoint used by the compiler to compile whole programs
   */
  def compile(input: CoreTransformed, mainSymbol: symbols.TermSymbol)(using Context): js.Module =
    val exports = List(js.Export(JSName("main"), js.Lambda(Nil, run(js.Call(nameRef(mainSymbol), Nil)))))

    val moduleDecl = input.core
    given DeclarationContext = new DeclarationContext(moduleDecl.declarations)
    transformModule(moduleDecl, Nil, exports)

  /**
   * Entrypoint used by the LSP server to show the compiled output AND used by
   * the website.
   */
  def compileSeparate(input: AllTransformed)(using Context) = {
    val module = input.main.mod

    val allDeclarations = input.dependencies.foldLeft(input.main.core.declarations) {
      case (decls, dependency) => decls ++ dependency.core.declarations
    }

    given D: DeclarationContext = new DeclarationContext(allDeclarations)

    def shouldExport(sym: Symbol) = sym match {
      // do not export fields, since they are no defined functions
      case fld if D.findField(fld).isDefined => false
      // do not export effect operations, since they are translated to field selection as well.
      case op if D.findProperty(op).isDefined => false
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

    transformModule(input.main.core, imports, exports)
  }


  // Representation of Data / Codata
  // ----
  def tagFor(constructor: Id)(using D: DeclarationContext, C: Context): js.Expr = {
    js.RawExpr(D.getConstructorTag(constructor).toString)
  }

  def generateConstructor(constructor: Constructor, tagValue: Int): js.Stmt = {
    val fields = constructor.fields
    // class Id {
    //   constructor(param...) { this.param = param; ...  }
    //   __reflect() { return { name: "NAME", data: [this.param...] }
    //   __equals(other) { ... }
    // }
    val params = fields.map { f => nameDef(f.id) }

    def set(field: JSName, value: js.Expr): js.Stmt = js.Assign(js.Member(js"this", field), value)
    def get(field: JSName): js.Expr = js.Member(js"this", field)

    val initParams = params.map { param => set(param, js.Variable(param))  }
    val initTag    = set(`tag`, js.RawExpr(tagValue.toString))
    val jsConstructor: js.Function = js.Function(JSName("constructor"), params, initTag :: initParams)

    val jsReflect: js.Function = js.Function(`reflect`, Nil, List(js.Return(js.Object(List(
      `tag`  -> js.RawExpr(tagValue.toString),
      `name` -> JsString(constructor.id.name.name),
      `data` -> js.ArrayLiteral(fields map { f => get(memberNameRef(f.id)) }))))))

    val other = freshName("other")
    def otherGet(field: JSName): js.Expr = js.Member(js.Variable(other), field)
    def compare(field: JSName): js.Expr = js"${get(field)} !== ${otherGet(field)}"
    val noop    = js.Block(Nil)
    val abort   = js.Return(js"false")
    val succeed = js.Return(js"true")
    val otherExists   = js.If(js"!${js.Variable(other)}", abort, noop)
    val compareTags   = js.If(compare(`tag`), abort, noop)
    val compareFields = params.map(f => js.If(compare(f), abort, noop))

    val jsEquals: js.Function = js.Function(`equals`, List(other), otherExists :: compareTags :: compareFields ::: List(succeed))

    js.Class(nameDef(constructor.id), List(jsConstructor, jsReflect, jsEquals))
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
  val `ref`   = JSName("ref")
  val `tag`   = JSName("__tag")
  val `name`  = JSName("__name")
  val `data`  = JSName("__data")
  val `reflect`  = JSName("__reflect")
  val `equals`  = JSName("__equals")

  def nameDef(id: Symbol): JSName = uniqueName(id)

  def uniqueName(sym: Symbol): JSName = JSName(jsEscape(sym.name.toString + "_" + sym.id))

  def nameRef(id: Symbol): js.Expr = js.Variable(uniqueName(id))

  // name references for fields and methods
  def memberNameRef(id: Symbol): JSName = uniqueName(id)

  def freshName(s: String): JSName =
    JSName(s + Symbol.fresh.next())


  // Separate Compilation (Website)
  // ------------------------------

  /**
   * Analyse core to find references to symbols defined in other modules.
   *
   * Necessary for generating the linker code (separate compilation for the web)
   */
  def usedImports(input: CoreTransformed): Map[Module, Set[Symbol]] = {
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
