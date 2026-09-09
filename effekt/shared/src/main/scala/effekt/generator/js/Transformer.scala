package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.{ *, given }
import effekt.symbols.{ Module, Symbol, Wildcard, Bindings }

import scala.collection.mutable


val `fresh` = JSName("fresh")
val `ref`   = JSName("ref")
val `__tag` = JSName("__tag")

/** How the values of one constructor are written, and where its fields then live. */
sealed trait Encoding {
  def name: JSName

  /** The members a value has, in declaration order, or none when it is not an object. */
  def members: List[JSName] = this match {
    case Encoding.AsObject(_, _, fields)           => fields
    case Encoding.AsTaggedObject(_, _, fields)     => fields
    case Encoding.AsNull(_) | Encoding.AsTag(_, _) => Nil
  }

  /** A value of this constructor, ascribed with it, since the value itself does not say which. */
  def make(values: List[js.Expr]): js.Expr = this match {
    case e: Encoding.AsNull                 => e.value                 // null
    case e: Encoding.AsTag                  => e.label                 // 1, the value being its tag
    case Encoding.AsObject(name, _, fields) => js.Ascription(name, js.Object(fields zip values))
    case Encoding.AsTaggedObject(name, tag, fields) =>
      js.Ascription(name, js.Object((`__tag` -> js.RawExpr(tag.toString)) :: (fields zip values)))
  }
}

/**
 * An encoding that writes a tag, which is what a `case` label selects on.
 *
 * [[Encoding.AsNull]] is the one that writes none, and it carries no tag to write (told apart by an `if`)
 */
sealed trait Tagged extends Encoding {
  def tag: Int

  /** The `case` label that selects this constructor. */
  def label: js.Expr = js.Ascription(name, js.RawExpr(tag.toString))
}

object Encoding {
  /** `null`, being the only field-free constructor of a data type that has objects too. */
  case class AsNull(name: JSName) extends Encoding {
    def value: js.Expr = js.Ascription(name, js.Null)
  }

  /** `1`, being one field-free constructor of several, or of a data type with no objects. */
  case class AsTag(name: JSName, tag: Int) extends Tagged

  /** `{ head: h, tail: t }`, the only object, so nothing has to say which constructor it is. */
  case class AsObject(name: JSName, tag: Int, fields: List[JSName]) extends Tagged

  /** `{ __tag: 1, head: h, tail: t }`, one object of several. */
  case class AsTaggedObject(name: JSName, tag: Int, fields: List[JSName]) extends Tagged
}

/** Where the tag of a value comes from (for a value that has one) */
enum Tag {

  /** the value is its tag: it is a number */
  case Itself

  /** stored in the object */
  case Stored

  /** fixed, needing no read, as no other constructor of the data type is an object */
  case Fixed(label: js.Expr)

  /** a number is its own tag, and anything else is an object, whose tag is `ofObject`. */
  case NumberOr(ofObject: Tag)

  /** Reads the tag off a value, which a `switch` then selects on. */
  def read(value: js.Expr): js.Expr = this match {
    case Itself             => value
    case Stored             => js.Member(value, `__tag`)
    case Fixed(label)       => label
    case NumberOr(ofObject) => js.IfExpr(js"typeof ${value} === ${JsString("number")}", value, ofObject.read(value))
  }
}

/** What a match does to find out which constructor a value is which */
enum Dispatch {

  /** The constructor is known: the data type is a record. */
  case Known

  /** A `switch` selects on the tag. */
  case ByTag(tag: Tag)

  /** A test against the one value that carries no tag; whatever fails it, `rest` tells apart. */
  case ByTest(absent: js.Expr, rest: Dispatch)
}

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
  case class Layout(dispatch: Dispatch, encodings: Map[Id, Encoding])

  val layouts: mutable.Map[Id, Layout] = mutable.Map.empty

  /** The layout of the data type that declares this constructor, decided from its shape alone. */
  def layoutFor(constructor: Id)(using D: DeclarationContext, C: Context): Layout =
    val data = D.findConstructor(constructor).flatMap(D.findData).getOrElse {
      C.panic(s"No data type declares the constructor ${constructor.name.name}")
    }
    layouts.getOrElseUpdate(data.id, {
      val constructors = data.constructors.zipWithIndex

      // 1. the shape of the whole data type decides how its constructors are written
      val (objects, singletons) = constructors.partition { case (c, _) => c.fields.nonEmpty }
      val encoded = constructors.map { case (c, tag) =>
        val name = nameDef(c.id)
        val members = c.fields.map { f => memberNameRef(f.id) }
        c.id -> ((c.fields, singletons, objects) match {
          // a) only singleton constructor, when there are full objects ~> null
          case (Nil, _ :: Nil, _ :: _) => Encoding.AsNull(name)
          // b) any other singleton constructor:                        ~> 1
          case (Nil, _, _)             => Encoding.AsTag(name, tag)
          // c) only constructor that is an object (no tag needed)      ~> { head_0: h }
          case (_, _, _ :: Nil)        => Encoding.AsObject(name, tag, members)
          // d) one object of several (tag is needed)                   ~> { __tag: 1, head_0: h }
          case _                       => Encoding.AsTaggedObject(name, tag, members)
        })
      }

      // 2. match tells constructors apart based on the encodings above, building the dispatch
      val absent     = encoded.collectFirst { case (_, e: Encoding.AsNull)   => e }
      val onlyObject = encoded.collectFirst { case (_, e: Encoding.AsObject) => e }

      val (tagOfObject, amongObjects) = onlyObject match {
        // either it is the only object, so the dispatch is clear
        case Some(only) => (Tag.Fixed(only.label), Dispatch.Known)
        // or there are several objects, so the dispatch is by `switch (x.__tag)`
        case None       => (Tag.Stored,            Dispatch.ByTag(Tag.Stored))
      }

      val dispatch = (absent, singletons, objects) match {
        // a) one is `null`, other are objects                              ~> if (x === null) ... else ...
        case (Some(nothing), _, _) => Dispatch.ByTest(nothing.value, amongObjects)
        // b) every constructor is a singleton                              ~> switch (x)
        case (_, _, Nil)           => Dispatch.ByTag(Tag.Itself)
        // c) every constructor is an object (potentially carrying its tag) ~> switch (x.__tag) (or nothing when there is one)
        case (_, Nil, _)           => amongObjects
        // d) several are numbers, the rest are objects                     ~> switch (typeof x === "number" ? x : x.__tag)
        case _                     => Dispatch.ByTag(Tag.NumberOr(tagOfObject))
      }

      Layout(dispatch, encoded.toMap)
    })

  def dispatchFor(constructor: Id)(using D: DeclarationContext, C: Context): Dispatch =
    layoutFor(constructor).dispatch

  def encodingFor(constructor: Id)(using D: DeclarationContext, C: Context): Encoding =
    layoutFor(constructor).encodings(constructor)

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
    def findUsedDependencies(t: Toplevel) =
      def go(t: Any): Unit = Tree.visit(t) {
        case BlockVar(x, tpe, capt) if publicDependencySymbols.isDefinedAt(x) =>
          register(publicDependencySymbols(x), x)
        case ValueVar(x, tpe) if publicDependencySymbols.isDefinedAt(x) =>
          register(publicDependencySymbols(x), x)
        case Make(tpe, id, targs, args) if publicDependencySymbols.isDefinedAt(id) =>
          register(publicDependencySymbols(id), id)
          args.foreach(go)
      }
      go(t)

    input.core.definitions.foreach(findUsedDependencies)

    usedFrom
  }
}
