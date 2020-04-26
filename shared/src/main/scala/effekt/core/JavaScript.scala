package effekt
package core

import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter
import effekt.context.Context

import scala.language.implicitConversions
import effekt.symbols.{ Name, QualifiedName, Symbol, builtins, moduleFile, moduleName }
import effekt.context.assertions._
import effekt.util.Task
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

class JavaScript extends ParenPrettyPrinter with Phase[ModuleDecl, Document] {

  def run(t: ModuleDecl)(implicit C: Context): Option[Document] =
    Some(format(t))

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(commonjs(t))

  val prelude = "if (typeof define !== 'function') { var define = require('amdefine')(module) }"

  val emptyline: Doc = line <> line

  def amdefine(m: ModuleDecl)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = brackets(hsep(deps.map { i => "'./" + moduleFile(i) + "'" }, comma))
    prelude <> line <> "define" <>
      parens(imports <> comma <+> jsFunction("", deps.map { d => moduleName(d) }, toDoc(m)))
  }

  def commonjs(m: ModuleDecl)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = vsep(deps.map { i =>
      "const" <+> moduleName(i) <+> "=" <+> jsCall("require", "'./" + moduleFile(i) + "'")
    }, semi)

    imports <> emptyline <> toDoc(m)
  }

  // TODO print all top level value declarations as "var"
  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    "var" <+> moduleName(m.path) <+> "=" <+> "{};" <> emptyline <> toDocTopLevel(m.defs)

  def toDoc(b: Block)(implicit C: Context): Doc = link(b, b match {
    case BlockVar(v) =>
      nameRef(v)
    case BlockDef(ps, body) =>
      jsLambda(ps map toDoc, toDoc(body))
    case Lift(b) =>
      jsCall("$effekt.lift", toDoc(b))
    case Extern(ps, body) =>
      jsLambda(ps map toDoc, body)
  })

  def toDoc(p: Param)(implicit C: Context): Doc = link(p, nameDef(p.id))

  def toDoc(n: Name)(implicit C: Context): Doc = link(n, n.toString)

  // we prefix op$ to effect operations to avoid clashes with reserved names like `get` and `set`
  def nameDef(id: Symbol)(implicit C: Context): Doc = id match {
    case _: symbols.EffectOp => "op$" + id.name.toString
    case _                   => toDoc(id.name)
  }

  def nameRef(id: Symbol)(implicit C: Context): Doc = id match {
    case _: symbols.Effect   => toDoc(id.name)
    case _: symbols.EffectOp => "op$" + id.name.toString
    case _ => id.name match {
      case name: QualifiedName if name.module != C.module => link(name, name.qualified)
      case name => toDoc(name)
    }
  }

  def toDoc(e: Expr)(implicit C: Context): Doc = link(e, e match {
    case UnitLit()     => "$effekt.unit"
    case StringLit(s)  => jsString(s)
    case l: Literal[t] => l.value.toString
    case ValueVar(id)  => nameRef(id)

    case Deref(id)     => jsCall(nameRef(id) <> ".value")
    case Assign(id, e) => jsCall(nameRef(id) <> ".value", toDoc(e))

    case PureApp(b, args) => toDoc(b) <> parens(hsep(args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    }, comma))

    case Select(b, field) =>
      toDoc(b) <> "." <> nameDef(field)
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
      toDocExpr(s)

  // pretty print the statement in a javascript expression context
  // not all statement types can be printed in this context!
  def toDocExpr(s: Stmt)(implicit C: Context): Doc = s match {
    case Val(Wildcard(_), binding, body) =>
      toDocDelayed(binding) <> ".then" <> parens(jsLambda(Nil, toDoc(body)))
    case Val(id, binding, body) =>
      toDocDelayed(binding) <> ".then" <> parens(jsLambda(List(nameDef(id)), toDoc(body)))
    case Var(id, binding, body) =>
      toDocDelayed(binding) <> ".state" <> parens(jsLambda(List(nameDef(id)), toDoc(body)))
    case App(b, args) =>
      jsCall(toDoc(b), args map argToDoc)
    case Do(b, id, args) =>
      jsCall(toDoc(b) <> "." <> nameRef(id), args map argToDoc)
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
    case Exports(path, exports) =>
      jsCall(
        "module.exports = Object.assign",
        moduleName(path),
        jsObject(exports.map { e => toDoc(e.name) -> toDoc(e.name) })
      )
    case Handle(body, hs) =>
      val handlers = hs map { handler => jsObject(handler.clauses.map { case (id, b) => nameDef(id) -> toDoc(b) }) }
      val cs = parens(jsArray(handlers))
      "$effekt.handle" <> cs <> parens(nest(line <> toDoc(body)))
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
    case other =>
      sys error s"Cannot print ${other} in expression position"
  }

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
    case Def(id, BlockDef(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, toDocStmt(body)) <> emptyline <> toDocStmt(rest)

    case Def(id, Extern(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, "return" <+> body) <> emptyline <> toDocStmt(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      vsep(cs, ";") <> ";" <> line <> line <> toDocStmt(rest)

    case Record(did, fields, rest) =>
      generateConstructor(did.asConstructor) <> ";" <> emptyline <> toDocStmt(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocStmt(rest)

    case other => "return" <+> toDocExpr(other)
  }

  def generateConstructor(ctor: symbols.Record)(implicit C: Context): Doc = {
    val fields = ctor.fields
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
    case Val(id, binding, body) =>
      "var" <+> nameDef(id) <+> "=" <+> toDoc(binding) <> ".run()" <> ";" <> emptyline <> toDocTopLevel(body)

    case Def(id, BlockDef(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, toDocStmt(body)) <> emptyline <> toDocTopLevel(rest)

    case Def(id, Extern(ps, body), rest) =>
      jsFunction(nameDef(id), ps map toDoc, "return" <+> body) <> emptyline <> toDocTopLevel(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      vsep(cs, ";") <> ";" <> emptyline <> toDocTopLevel(rest)

    case Record(did, fields, rest) =>
      generateConstructor(did.asConstructor) <> ";" <> emptyline <> toDocTopLevel(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocTopLevel(rest)

    case other => "return" <+> toDocExpr(other)
  }

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
    case Def(id, d, rest) => true
    case _ => false
  }

}
