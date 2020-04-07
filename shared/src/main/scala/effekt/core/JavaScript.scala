package effekt
package core

import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter
import effekt.context.Context

import scala.language.implicitConversions
import effekt.symbols.{ Symbol, Name, builtins, moduleFile, moduleName }
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

class JavaScript extends ParenPrettyPrinter with Phase[ModuleDecl, Document] {

  val phaseName = "code-generator"

  def run(t: ModuleDecl)(implicit C: Context): Option[Document] =
    Some(format(t))

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(amdefine(t))

  val prelude = "if (typeof define !== 'function') { var define = require('amdefine')(module) }"

  val emptyline: Doc = line <> line

  def amdefine(m: ModuleDecl)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = brackets(hsep(deps.map { i => "'./" + moduleFile(i) + "'" }, comma))
    prelude <> line <> "define" <>
      parens(imports <> comma <+> "function" <> parens(hsep(deps.map { d => moduleName(d) }, comma)) <+>
        braces(nest(line <> toDoc(m)) <> line))
  }

  // TODO print all top level value declarations as "var"
  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    "var" <+> moduleName(m.path) <+> "=" <+> "{};" <> emptyline <> toDocTopLevel(m.defs)

  def toDoc(b: Block)(implicit C: Context): Doc = link(b, b match {
    case BlockVar(v) => nameRef(v)
    case BlockDef(ps, body) =>
      parens(hsep(ps map toDoc, comma)) <+> "=>" <+> toDoc(body)
    case Lift(b) => "$effekt.lift" <> parens(toDoc(b))
    case Extern(ps, body) =>
      parens(hsep(ps map toDoc, comma)) <+> "=>" <+> body
  })

  def toDoc(p: Param)(implicit C: Context): Doc = link(p, nameDef(p.id))

  def toDoc(n: Name)(implicit C: Context): Doc = link(n, n.toString)

  def nameDef(id: Symbol)(implicit C: Context): Doc = toDoc(id.name)

  def nameRef(id: Symbol)(implicit C: Context): Doc = id match {
    case _: symbols.Effect | _: symbols.EffectOp => toDoc(id.name)
    case _ if id.name.module != C.module => link(id.name, id.name.qualified)
    case _ => toDoc(id.name)
  }

  def toDoc(e: Expr)(implicit C: Context): Doc = link(e, e match {
    case UnitLit()     => "$effekt.unit"
    case StringLit(s)  => "\"" + s + "\""
    case l: Literal[t] => l.value.toString
    case ValueVar(id)  => nameRef(id)

    case Deref(id)     => nameRef(id) <> ".value()"
    case Assign(id, e) => nameRef(id) <> ".value" <> parens(toDoc(e))

    case PureApp(b, args) => toDoc(b) <> parens(hsep(args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    }, comma))
  })

  def argToDoc(e: Argument)(implicit C: Context): Doc = e match {
    case e: Expr  => toDoc(e)
    case b: Block => toDoc(b)
  }

  def toDoc(s: Stmt)(implicit C: Context): Doc =
    if (requiresBlock(s))
      link(s, braces(nest(line <> toDocStmt(s)) <> line))
    else
      link(s, toDocExpr(s))

  def toDocDelayed(s: Stmt)(implicit C: Context): Doc =
    if (requiresBlock(s))
      "$effekt.delayed" <> parens("() => " <+> braces(nest(line <> toDocStmt(s)) <> line))
    else
      toDocExpr(s)

  // pretty print the statement in a javascript expression context
  // not all statement types can be printed in this context!
  def toDocExpr(s: Stmt)(implicit C: Context): Doc = s match {
    case Val(Wildcard(_), binding, body) =>
      toDocDelayed(binding) <> ".then" <> parens("()" <+> "=>" <+> nest(line <> toDoc(body)))
    case Val(id, binding, body) =>
      toDocDelayed(binding) <> ".then" <> parens(nameDef(id) <+> "=>" <+> nest(line <> toDoc(body)))
    case Var(id, binding, body) =>
      toDocDelayed(binding) <> ".state" <> parens(nameDef(id) <+> "=>" <+> nest(line <> toDoc(body)))
    case App(b, args) =>
      toDoc(b) <> parens(hsep(args map argToDoc, comma))
    case If(cond, thn, els) =>
      parens(toDoc(cond)) <+> "?" <+> toDocDelayed(thn) <+> ":" <+> toDocDelayed(els)
    case While(cond, body) =>
      "$effekt._while" <> parens(
        "() =>" <+> toDoc(cond) <> comma <+>
          "() =>" <+> toDoc(body)
      )
    case Ret(e) =>
      "$effekt.pure" <> parens(toDoc(e))
    case Exports(path, exports) =>
      "Object.assign" <> parens(moduleName(path) <> comma <+> braces(nest(line <> vsep(exports.map { e =>
        toDoc(e.name) <> ":" <+> toDoc(e.name)
      }, comma)) <> line))
    case Handle(body, handler) =>
      val cs = parens("[" <> nest(line <> vsep(handler.clauses map { case (_, b) => toDoc(b) }, comma)) <> "]")
      "$effekt.handle" <> cs <> parens(nest(line <> toDoc(body)))
    case Match(sc, clauses) =>
      // TODO using the unqualified name here might lead to wrong operational behavior
      val cs = braces(nest(line <> vsep(clauses map { case (id, b) => nameDef(id) <> ":" <+> toDoc(b) }, comma)) <> line)
      "$effekt.match" <> parens(toDoc(sc) <> comma <+> cs)
    case other =>
      sys error s"Cannot print ${other} in expression position"
  }

  def toDocStmt(s: Stmt)(implicit C: Context): Doc = s match {
    case Def(id, BlockDef(ps, body), rest) =>
      "function" <+> nameDef(id) <> parens(hsep(ps map toDoc, comma)) <+>
        braces(nest(line <> toDocStmt(body)) <> line) <> emptyline <> toDocStmt(rest)

    case Def(id, Extern(ps, body), rest) =>
      "function" <+> nameDef(id) <> parens(hsep(ps map toDoc, comma)) <+>
        braces(nest(line <> "return" <+> body) <> line) <> emptyline <> toDocStmt(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { id =>
        val datastr = "\"" <> nameDef(did) <> "\""
        val consstr = "\"" <> nameDef(id) <> "\""
        "const" <+> nameDef(id) <+> "=" <+> "$effekt.constructor" <> parens(datastr <> comma <+> consstr)
      }
      vsep(cs, ";") <> ";" <> line <> line <> toDocStmt(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocStmt(rest)

    case other => "return" <+> toDocExpr(other)
  }

  /**
   * This is an alternative statement printer, rendering toplevel value bindings
   * as variables, instead of using the monadic bind.
   */
  def toDocTopLevel(s: Stmt)(implicit C: Context): Doc = s match {
    case Val(id, binding, body) =>
      "var" <+> nameDef(id) <+> "=" <+> toDoc(binding) <> ".run()" <> ";" <> emptyline <> toDocTopLevel(body)

    case Def(id, BlockDef(ps, body), rest) =>
      "function" <+> nameDef(id) <> parens(hsep(ps map toDoc, comma)) <+>
        braces(nest(line <> toDocStmt(body)) <> line) <> emptyline <> toDocTopLevel(rest)

    case Def(id, Extern(ps, body), rest) =>
      "function" <+> nameDef(id) <> parens(hsep(ps map toDoc, comma)) <+>
        braces(nest(line <> "return" <+> body) <> line) <> emptyline <> toDocTopLevel(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { id =>
        val datastr = "\"" <> nameDef(did) <> "\""
        val consstr = "\"" <> nameDef(id) <> "\""
        "const" <+> nameDef(id) <+> "=" <+> "$effekt.constructor" <> parens(datastr <> comma <+> consstr)
      }
      vsep(cs, ";") <> ";" <> emptyline <> toDocTopLevel(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocTopLevel(rest)

    case other => "return" <+> toDocExpr(other)
  }

  def requiresBlock(s: Stmt): Boolean = s match {
    case Data(did, ctors, rest) => true
    case Def(id, d, rest) => true
    case _ => false
  }

}
