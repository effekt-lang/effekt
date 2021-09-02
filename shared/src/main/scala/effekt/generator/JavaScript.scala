package effekt.generator

import effekt.context.Context
import effekt.context.assertions._
import effekt.core._
import effekt.symbols.{ Module, Name, NoName, LocalName, QualifiedName, Symbol, Wildcard }
import effekt.symbols
import org.bitbucket.inkytonik.kiama
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import effekt.util.paths._

import scala.language.implicitConversions

class JavaScript extends Generator {

  val prettyPrinter: JavaScriptPrinter = new JavaScriptPrinter {}

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / prettyPrinter.moduleFile(m.path)).unixPath

  /**
   * This is only called on the main entry point, we have to manually traverse the dependencies
   * and write them.
   */
  def run(src: Source)(implicit C: Context): Option[Document] = for {
    mod <- C.frontend(src)
    _ = mod.dependencies.flatMap(compile)
    doc <- compile(mod)
  } yield doc

  /**
   * Compiles only the given module, does not compile dependencies
   */
  def compile(mod: Module)(implicit C: Context): Option[Document] = for {
    core <- C.backend(mod.source)
    // setting the scope to mod is important to generate qualified names
    doc = C.using(module = mod) { prettyPrinter.format(core) }
    _ = C.saveOutput(doc.layout, path(mod))
  } yield doc
}

/**
 * A JavaScript PrettyPrinter that generates code using the
 * control monad.
 */
trait JavaScriptPrinter extends JavaScriptBase {

  def toDoc(b: Block)(implicit C: Context): Doc = link(b, b match {
    case BlockVar(v) =>
      nameRef(v)
    case BlockLit(ps, body) =>
      jsLambda(ps map toDoc, toDoc(body))
    //    case Member(b, id) =>
    //      toDoc(b) <> "." <> nameDef(id)
    case Extern(pure, ps, body) =>
      jsLambda(ps map toDoc, body)
    case Unbox(e) => toDoc(e)
    case _        => sys error "Unsupported block in plain JS pretty printer"
  })

  // pretty print the statement in a javascript expression context
  // not all statement types can be printed in this context!
  def toDocExpr(s: Stmt)(implicit C: Context): Doc = s match {
    case Val(Wildcard(_), tpe, binding, body) =>
      toDocDelayed(binding) <> ".then" <> parens(jsLambda(Nil, toDoc(body)))

    case Val(id, tpe, binding, body) =>
      toDocDelayed(binding) <> ".then" <> parens(jsLambda(List(nameDef(id)), toDoc(body)))

    case App(b, targs, args) =>
      jsCall(toDoc(b), args map argToDoc)

    case If(cond, thn, els) =>
      parens(toDoc(cond)) <+> "?" <+> toDocDelayed(thn) <+> ":" <+> toDocDelayed(els)

    case While(cond, body) =>
      jsCall(
        "$effekt._while",
        jsLambda(Nil, toDoc(cond)),
        jsLambda(Nil, toDoc(body))
      )

    case Ret(e) =>
      jsCall("$effekt.pure", toDoc(e))

    //    case State(id, tpe, get, put, init, body) =>
    //      toDocDelayed(init) <> ".state" <> parens(toDoc(body))
    //
    //    case Handle(body, hs) =>
    //      val handlers = hs map { handler => jsObject(handler.clauses.map { case (id, b) => nameDef(id) -> toDoc(b) }) }
    //      val cs = parens(jsArray(handlers))
    //      "$effekt.handle" <> cs <> parens(nest(line <> toDoc(body)))

    case Match(sc, clauses) =>
      val cs = jsArray(clauses map {
        case (pattern, b) => jsObject(
          text("pattern") -> toDoc(pattern),
          text("exec") -> toDoc(b)
        )
      })
      jsCall("$effekt.match", toDoc(sc), cs)

    case Hole =>
      jsCall("$effekt.hole", Nil)

    case Exports(path, exports) =>
      jsCall(
        "module.exports = Object.assign",
        jsModuleName(path),
        jsObject(exports.map { e => toDoc(e.name) -> toDoc(e.name) })
      )

    case other =>
      sys error s"Cannot print ${other} in expression position"
  }
}

trait JavaScriptBase extends ParenPrettyPrinter {

  def moduleFile(path: String): String = path.replace('/', '_') + ".js"

  def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(commonjs(t))

  val prelude = "if (typeof define !== 'function') { var define = require('amdefine')(module) }"

  val emptyline: Doc = line <> line

  def amdefine(m: ModuleDecl)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = brackets(hsep(deps.map { i => "'./" + moduleFile(i) + "'" }, comma))
    prelude <> line <> "define" <>
      parens(imports <> comma <+> jsFunction("", deps.map { d => jsModuleName(d) }, toDoc(m)))
  }

  def commonjs(m: ModuleDecl)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = vsep(deps.map { i =>
      "const" <+> jsModuleName(i) <+> "=" <+> jsCall("require", "'./" + moduleFile(i) + "'")
    }, semi)

    imports <> emptyline <> toDoc(m)
  }

  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    "var" <+> jsModuleName(m.path) <+> "=" <+> "{};" <> emptyline <> toDocTopLevel(m.defs)

  def toDoc(b: Block)(implicit C: Context): Doc

  def toDoc(p: Param)(implicit C: Context): Doc = link(p, nameDef(p.id))

  def toDoc(n: Name)(implicit C: Context): Doc = link(n, n.toString)

  // we prefix op$ to effect operations to avoid clashes with reserved names like `get` and `set`
  def nameDef(id: Symbol)(implicit C: Context): Doc = id match {
    // case _: symbols.Capability => id.name.toString + "_" + id.id
    // case _: symbols.EffectOp   => "op$" + id.name.toString
    case _ => toDoc(id.name)
  }

  def nameRef(id: Symbol)(implicit C: Context): Doc = jsNameRef(id.name)
  // case _: symbols.Effect     => toDoc(id.name)
  // case _: symbols.Capability => id.name.toString + "_" + id.id
  // case _: symbols.EffectOp   => "op$" + id.name.toString

  def toDoc(e: Expr)(implicit C: Context): Doc = link(e, e match {
    case UnitLit()     => "$effekt.unit"
    case StringLit(s)  => jsString(s)
    case l: Literal[t] => l.value.toString
    case ValueVar(id)  => nameRef(id)

    case PureApp(b, targs, args) => toDoc(b) <> parens(hsep(args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    }, comma))

    case Select(b, field) =>
      toDoc(b) <> "." <> nameDef(field)

    case Closure(e) => toDoc(e)
  })

  def argToDoc(e: Argument)(implicit C: Context): Doc = e match {
    case e: Expr  => toDoc(e)
    case b: Block => toDoc(b)
  }

  def toDoc(s: Stmt)(implicit C: Context): Doc =
    if (requiresBlock(s))
      link(s, jsBlock(toDocStmt(s)))
    else
      link(s, toDocExpr(s))

  def toDocDelayed(s: Stmt)(implicit C: Context): Doc =
    if (requiresBlock(s))
      jsCall("$effekt.delayed", jsLambda(Nil, jsBlock(toDocStmt(s))))
    else
      parens(toDocExpr(s))

  // pretty print the statement in a javascript expression context
  // not all statement types can be printed in this context!
  def toDocExpr(s: Stmt)(implicit C: Context): Doc

  def toDoc(p: Pattern)(implicit C: Context): Doc = p match {
    case IgnorePattern()   => "$effekt.ignore"
    case AnyPattern()      => "$effekt.any"
    case LiteralPattern(l) => jsCall("$effekt.literal", toDoc(l))
    case TagPattern(id, ps) =>
      val tag = jsString(nameDef(id))
      val childMatchers = ps map { p => toDoc(p) }
      jsCall("$effekt.tagged", tag :: childMatchers)
  }

  def toDocStmt(s: Stmt)(implicit C: Context): Doc = s match {
    case Def(id, tpe, BlockLit(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, toDocStmt(body)) <> emptyline <> toDocStmt(rest)

    case Def(id, tpe, Extern(false, ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, "return" <+> body) <> emptyline <> toDocStmt(rest)

    case Def(id, tpe, Extern(true, ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, "return" <+> jsCall("$effekt.pure", body)) <> emptyline <> toDocStmt(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      vsep(cs, ";") <> ";" <> line <> line <> toDocStmt(rest)

    case Record(did, fields, rest) =>
      generateConstructor(did, fields) <> ";" <> emptyline <> toDocStmt(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocStmt(rest)

    case other => "return" <+> toDocExpr(other)
  }

  def generateConstructor(ctor: symbols.Record)(implicit C: Context): Doc =
    generateConstructor(ctor, ctor.fields)

  def generateConstructor(ctor: Symbol, fields: List[Symbol])(implicit C: Context): Doc = {
    jsFunction(nameDef(ctor), fields.map { f => nameDef(f) }, "return" <+> jsCall("$effekt.pure", jsObject(List(
      text("__tag") -> jsString(nameDef(ctor)),
      text("__data") -> jsArray(fields map { f => nameDef(f) }),
    ) ++ fields.map { f =>
        (nameDef(f), nameDef(f))
      })))
  }

  /**
   * This is an alternative statement printer, rendering toplevel value bindings
   * as variables, instead of using the monadic bind.
   */
  def toDocTopLevel(s: Stmt)(implicit C: Context): Doc = s match {
    case Val(id, tpe, binding, body) =>
      "var" <+> nameDef(id) <+> "=" <+> toDoc(binding) <> ".run()" <> ";" <> emptyline <> toDocTopLevel(body)

    case Def(id, tpe, BlockLit(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, toDocStmt(body)) <> emptyline <> toDocTopLevel(rest)

    case Def(id, tpe, Extern(false, ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, "return" <+> body) <> emptyline <> toDocTopLevel(rest)

    case Def(id, tpe, Extern(true, ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, "return" <+> jsCall("$effekt.pure", body)) <> emptyline <> toDocTopLevel(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      vsep(cs, ";") <> ";" <> emptyline <> toDocTopLevel(rest)

    case Record(did, fields, rest) =>
      generateConstructor(did, fields) <> ";" <> emptyline <> toDocTopLevel(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocTopLevel(rest)

    case other => "return" <+> toDocExpr(other)
  }

  def jsNameRef(name: Name): String = name match {
    case LocalName(name)           => name
    case QualifiedName(Nil, name)  => name
    case QualifiedName(path, name) => "$" + path.mkString("_") + "." + name
    case NoName                    => sys error "Trying to generate code for an anonymous entity"
  }

  def jsModuleName(path: String): String = "$" + path.replace('/', '_')

  def jsLambda(params: List[Doc], body: Doc) =
    parens(hsep(params, comma)) <+> "=>" <> group(nest(line <> body))

  def jsFunction(name: Doc, params: List[Doc], body: Doc): Doc =
    "function" <+> name <> parens(hsep(params, comma)) <+> jsBlock(body)

  def jsObject(fields: (Doc, Doc)*): Doc =
    jsObject(fields.toList)

  def jsObject(fields: List[(Doc, Doc)]): Doc =
    group(jsBlock(vsep(fields.map { case (n, d) => jsString(n) <> ":" <+> d }, comma)))

  def jsBlock(content: Doc): Doc = braces(nest(line <> content) <> line)

  def jsArray(els: List[Doc]): Doc =
    brackets(hsep(els, comma))

  def jsString(contents: Doc): Doc =
    "\"" <> contents <> "\""

  def jsCall(fun: Doc, args: Doc*): Doc = jsCall(fun, args.toList)
  def jsCall(fun: Doc, args: List[Doc]): Doc = fun <> parens(hsep(args, comma))

  def requiresBlock(s: Stmt): Boolean = s match {
    case Data(did, ctors, rest) => true
    case Record(did, fields, rest) => true
    case Def(id, tpe, d, rest) => true
    case _ => false
  }
}
