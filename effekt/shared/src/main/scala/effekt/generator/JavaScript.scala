package effekt
package generator

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.*
import effekt.symbols.{ LocalName, Module, Name, NoName, QualifiedName, Symbol, Wildcard }
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import effekt.util.paths.*

import scala.language.implicitConversions

object JavaScriptMonadic extends JavaScript {
  val prettyPrinter: JavaScriptPrinter = new JavaScriptPrinter {}
}

trait JavaScript extends Backend {

  def prettyPrinter: JavaScriptPrinter

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(implicit C: Context) = {
    val compiledDependencies = dependencies.flatMap { dep => compile(dep) }.toMap
    compile(main).map {
      case (mainFile, result) =>
        Compiled(mainFile, compiledDependencies.updated(mainFile, result))
    }
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(implicit C: Context) =
    C.using(module = input.mod) { Some(prettyPrinter.format(input.core)) }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / prettyPrinter.moduleFile(m.path)).unixPath

  /**
   * Compiles only the given module, does not compile dependencies
   *
   * Writes the compiled result into a file.
   */
  private def compile(in: CoreTransformed)(implicit C: Context): Option[(String, Document)] =
    compileSeparate(in).map { doc =>
      (path(in.mod), doc)
    }
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
    case Member(b, id) =>
      toDoc(b) <> "." <> nameRef(id)
    case Extern(ps, body) =>
      jsLambda(ps map toDoc, body)
    case Unbox(e)     => toDoc(e)
    case New(handler) => toDoc(handler)
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
      parens(parens(toDoc(cond)) <+> "?" <+> toDocDelayed(thn) <+> ":" <+> toDocDelayed(els))

    case While(cond, body) =>
      jsCall(
        "$effekt._while",
        jsLambda(Nil, toDoc(cond)),
        jsLambda(Nil, toDoc(body))
      )

    case Ret(e) =>
      jsCall("$effekt.pure", toDoc(e))

    case Handle(body, hs) =>
      val handlers = hs.map(toDoc)
      val cs = parens(jsArray(handlers))
      "$effekt.handle" <> cs <> parens(nest(line <> toDoc(body)))

    case Region(body) =>
      jsCall("$effekt.withRegion", toDoc(body))

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

    case other => jsCall(parens(jsLambda(Nil, jsBlock(toDocStmt(other)))), Nil)
  }

  def toDoc(handler: Handler)(using Context): Doc =
    jsObject(handler.clauses.map { case (id, b) => nameDef(id) -> toDoc(b) })
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
    "var" <+> jsModuleName(m.path) <+> "=" <+> "{};" <> emptyline <> toDocTopLevel(m.defs) <> emptyline <> jsCall(
      "module.exports = Object.assign",
      jsModuleName(m.path),
      jsObject(m.exports.map { e => nameDef(e) -> nameDef(e) })
    )

  def toDoc(b: Block)(implicit C: Context): Doc

  def toDoc(p: Param)(implicit C: Context): Doc = link(p, nameDef(p.id))

  def toDoc(n: Name)(implicit C: Context): Doc = link(n, n.toString)

  def nameDef(id: Symbol)(implicit C: Context): Doc = id match {
    case b: symbols.BlockParam if b.tpe.isInstanceOf[symbols.InterfaceType] => id.name.toString + "_" + id.id
    case _: symbols.Operation => "op$" + id.name.toString
    case _ => jsEscape(id.name.toString)
  }

  def nameRef(id: Symbol)(implicit C: Context): Doc = id match {
    case _: symbols.InterfaceType => toDoc(id.name)
    case b: symbols.BlockParam if b.tpe.isInstanceOf[symbols.InterfaceType] => id.name.toString + "_" + id.id
    case _: symbols.Operation => "op$" + id.name.toString
    case _: symbols.Field => jsEscape(id.name.name)
    case _ => jsEscape(jsNameRef(id.name))
  }

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
      toDoc(b) <> "." <> nameRef(field)

    case Box(e) => toDoc(e)

    case Run(s) => toDoc(s) <> ".run()"
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

    case Def(id, tpe, Extern(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, "return" <+> body) <> emptyline <> toDocStmt(rest)

    case Def(id, tpe, block, rest) =>
      "const" <+> nameDef(id) <+> "=" <+> toDoc(block) <> ";" <> emptyline <> toDocStmt(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      vsep(cs, ";") <> ";" <> line <> line <> toDocStmt(rest)

    case Record(did, fields, rest) =>
      generateConstructor(did, fields) <> ";" <> emptyline <> toDocStmt(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocStmt(rest)

    case Let(Wildcard(_), tpe, binding, body) =>
      toDoc(binding) <> ";" <> emptyline <> toDocStmt(body)

    case Let(id, tpe, binding, body) =>
      "const" <+> nameDef(id) <+> "=" <+> toDoc(binding) <> ";" <> emptyline <> toDocStmt(body)

    case State(id, init, region, body) if region == symbols.builtins.globalRegion =>
      "const" <+> nameDef(id) <+> "=" <+> jsCall("$effekt.fresh", toDoc(init)) <> ";" <> emptyline <> toDocStmt(body)

    case State(id, init, region, body) =>
      "const" <+> nameDef(id) <+> "=" <+> jsCall(nameRef(region) <> ".fresh", toDoc(init)) <> ";" <> emptyline <> toDocStmt(body)

    case other => "return" <+> toDocExpr(other)
  }

  def generateConstructor(ctor: symbols.Record)(implicit C: Context): Doc =
    generateConstructor(ctor, ctor.fields)

  def generateConstructor(ctor: Symbol, fields: List[Symbol])(implicit C: Context): Doc = {
    jsFunction(nameDef(ctor), fields.map { f => nameDef(f) }, "return" <+> jsObject(List(
      text("__tag") -> jsString(nameDef(ctor)),
      text("__data") -> jsArray(fields map { f => nameDef(f) }),
    ) ++ fields.map { f =>
        (nameDef(f), nameDef(f))
      }))
  }

  /**
   * This is an alternative statement printer, rendering toplevel value bindings
   * as variables, instead of using the monadic bind.
   */
  def toDocTopLevel(s: Stmt)(implicit C: Context): Doc = s match {
    case Val(id, tpe, binding, body) =>
      "const" <+> nameDef(id) <+> "=" <+> toDoc(binding) <> ".run()" <> ";" <> emptyline <> toDocTopLevel(body)

    case Let(id, tpe, binding, body) =>
      "const" <+> nameDef(id) <+> "=" <+> toDoc(binding) <> ";" <> emptyline <> toDocTopLevel(body)

    case Def(id, tpe, BlockLit(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, toDocStmt(body)) <> emptyline <> toDocTopLevel(rest)

    case Def(id, tpe, Extern(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, "return" <+> body) <> emptyline <> toDocTopLevel(rest)

    case Def(id, tpe, block, rest) =>
      "const" <+> nameDef(id) <+> "=" <+> toDoc(block) <> ";" <> emptyline <> toDocStmt(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      vsep(cs, ";") <> ";" <> emptyline <> toDocTopLevel(rest)

    case Record(did, fields, rest) =>
      generateConstructor(did, fields) <> ";" <> emptyline <> toDocTopLevel(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocTopLevel(rest)

    case other => toDocExpr(other)
  }

  val reserved = List("get", "set", "yield", "delete", "new", "catch", "in", "finally", "switch", "case", "this")
  def jsEscape(name: String): String = if (reserved contains name) "$" + name else name

  def jsNameRef(name: Name)(implicit C: Context): String = name match {
    case LocalName(name) => name
    case QualifiedName(Nil, name) => name
    // TODO this is rather fragile...
    case QualifiedName(path, name) if C.module.path == path.mkString("/") => name
    case QualifiedName(path, name) => "$" + path.mkString("_") + "." + jsEscape(name)
    case NoName => sys error "Trying to generate code for an anonymous entity"
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
