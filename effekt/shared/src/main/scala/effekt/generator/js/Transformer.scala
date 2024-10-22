package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.{ *, given }
import effekt.core.Variables
import effekt.core.Variables.{ all, bound, free }
import effekt.symbols.{ Module, Symbol, Wildcard, Bindings }

import scala.collection.mutable

/**
 * Parent all JS transformers.
 *
 * Shares generic JS specific definitions
 */
trait Transformer {

  val jsFeatureFlags: List[String] = List("js")

  val escapeSeqs: Map[Char, String] = Map('\'' -> raw"'", '\"' -> raw"\"", '\\' -> raw"\\", '\n' -> raw"\n", '\t' -> raw"\t", '\r' -> raw"\r")

  def shouldExport(id: Id)(using D: DeclarationContext): Boolean = true

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
    def compare(field: JSName): js.Expr = js"!${$effekt.call("equals", get(field), otherGet(field))}"
    val noop    = js.Block(Nil)
    val abort   = js.Return(js"false")
    val succeed = js.Return(js"true")
    val otherExists   = js.If(js"!${js.Variable(other)}", abort, noop)
    val compareTags   = js.If(compare(`tag`), abort, noop)
    val compareFields = params.map(f => js.If(compare(f), abort, noop))

    val jsEquals: js.Function = js.Function(`equals`, List(other), otherExists :: compareTags :: compareFields ::: List(succeed))

    js.Class(nameDef(constructor.id), List(jsConstructor, jsReflect, jsEquals))
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

  def jsEscape(name: String): String =
    if (reserved contains name) "$" + name else name.replace("?", "").replace("!", "")

  def jsModuleName(path: String): String = "$" + path.replace('/', '_').replace('-', '_')

  def jsModuleFile(path: String): String = path.replace('/', '_').replace('-', '_') + ".js"

  val `fresh` = JSName("fresh")
  val `ref`   = JSName("ref")
  val `tag`   = JSName("__tag")
  val `name`  = JSName("__name")
  val `data`  = JSName("__data")
  val `reflect`  = JSName("__reflect")
  val `equals`  = JSName("__equals")

  def nameDef(id: Id): JSName = uniqueName(id)

  // attempt to have better / shorter names
  val usedNames: mutable.Map[String, Int] = mutable.Map.empty
  val names: mutable.Map[Id, String] = mutable.Map.empty
  val baseNameRx = """([A-Za-z$]*(?:_[A-Za-z]+)*)""".r // extracts the base number up until the first number

  def uniqueName(sym: Id): JSName = {
    def uniqueNameFor(base: String): String =
      val nextId = usedNames.getOrElse(base, 0)
      usedNames.update(base, nextId + 1)
      s"${base}_${nextId}"

    val name = names.getOrElseUpdate(sym, baseNameRx.findFirstIn(sym.name.name) match {
      case Some(base) => uniqueNameFor(base)
      case None =>
        println(sym.name)
        uniqueNameFor("tmp")
    })
    JSName(jsEscape(name))
  }

  def nameRef(id: Id): js.Expr = js.Variable(uniqueName(id))

  // name references for fields and methods
  def memberNameRef(id: Id): JSName = uniqueName(id)

  def freshName(s: String): JSName =
    JSName(s + Symbol.fresh.next())

  def escape(scalaString: String): String =
    scalaString.foldLeft(StringBuilder()) { (acc, c) =>
      escapeSeqs.get(c) match {
        case Some(s) => acc ++= s
        case None => acc += c
      }
    }.toString()


  // Separate Compilation (Website)
  // ------------------------------

  /**
   * Analyse core to find references to symbols defined in other modules.
   *
   * Necessary for generating the linker code (separate compilation for the web)
   */
  def usedIncludes(input: CoreTransformed): Map[Module, Set[Id]] = {
    val dependencies = input.mod.dependencies

    // Create a mapping Termsymbol -> Module
    def definedIn(m: Module, b: Bindings): Map[Id, Module] =
      b.terms.values.flatten.map { sym => (sym : Id) -> m }.toMap ++
        b.namespaces.values.flatMap(bs => definedIn(m, bs))

    val publicDependencySymbols = dependencies.flatMap(m => definedIn(m, m.exports)).toMap

    var usedFrom: Map[Module, Set[Id]] = Map.empty

    def register(m: Module, sym: Id) = {
      val before = usedFrom.getOrElse(m, Set.empty)
      usedFrom = usedFrom.updated(m, before + sym)
    }

    // Traverse tree once more to find all used symbols, defined in other modules.
    def findUsedDependencies(t: Definition) =
      def go(t: Any): Unit = Tree.visit(t) {
        case BlockVar(x, tpe, capt) if publicDependencySymbols.isDefinedAt(x) =>
          register(publicDependencySymbols(x), x)
        case ValueVar(x, tpe) if publicDependencySymbols.isDefinedAt(x) =>
          register(publicDependencySymbols(x), x)
        case Make(tpe, id, args) if publicDependencySymbols.isDefinedAt(id) =>
          register(publicDependencySymbols(id), id)
          args.foreach(go)
      }
      go(t)

    input.core.definitions.foreach(findUsedDependencies)

    usedFrom
  }
}
