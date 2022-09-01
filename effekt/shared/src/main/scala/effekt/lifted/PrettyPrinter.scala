package effekt
package lifted

import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{ Name, Wildcard, builtins }

object PrettyPrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  override val defaultIndent = 2

  def format(t: ModuleDecl): Document =
    pretty(toDoc(t), 4)

  def format(s: Stmt): String =
    pretty(toDocStmt(s), 60).layout

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    "module" <+> m.path <> emptyline <> vsep(m.imports.map { im => "import" <+> im }, line) <>
      emptyline <> toDocStmt(m.defs)
  }

  def toDoc(b: Block): Doc = b match {
    case BlockVar(v) => v.name.toString
    case BlockLit(ps, body) =>
      braces { space <> parens(hsep(ps map toDoc, comma)) <+> "=>" <+> nest(line <> toDoc(body)) <> line }
    case Member(b, id) =>
      toDoc(b) <> "." <> id.name.toString
    case Extern(ps, body) => parens(hsep(ps map toDoc, comma)) <+> "=>" <+> braces(nest(line <> body) <> line)
    case Unbox(e)         => parens("unbox" <+> toDoc(e))
    case New(handler)     => "new" <+> toDoc(handler)
  }

  def toDoc(p: Param): Doc = p.id.name.toString

  def toDoc(n: Name): Doc = n.toString

  def toDoc(e: Expr): Doc = e match {
    case UnitLit()               => "()"
    case StringLit(s)            => "\"" + s + "\""
    case l: Literal[t]           => l.value.toString
    case ValueVar(id)            => id.name.toString

    case PureApp(b, targs, args) => toDoc(b) <> parens(hsep(args map argToDoc, comma))

    case Select(b, field) =>
      toDoc(b) <> "." <> toDoc(field.name)

    case Closure(b) => parens("box" <+> toDoc(b))
    case Run(s)     => "run" <+> block(toDocStmt(s))
  }

  def argToDoc(e: Argument): Doc = e match {
    case e: Expr     => toDoc(e)
    case b: Block    => toDoc(b)
    case e: Evidence => toDoc(e)
  }

  def toDoc(e: Evidence): Doc = e match {
    case Evidence(Nil)  => "<>"
    case Evidence(list) => angles(hsep(list.map { ev => toDoc(ev.name) }, ","))
  }

  def toDoc(s: Stmt): Doc =
    if (requiresBlock(s))
      braces(nest(line <> toDocStmt(s)) <> line)
    else
      toDocStmt(s)

  def toDoc(p: Pattern): Doc = p match {
    case IgnorePattern()          => "_"
    case LiteralPattern(l)        => toDoc(l)
    case AnyPattern()             => "*"
    case TagPattern(id, patterns) => toDoc(id.name) <> parens(hsep(patterns map toDoc, comma))
  }

  def toDoc(handler: Handler): Doc = {
    val handlerName = toDoc(handler.id.name)
    val clauses = handler.clauses.map {
      case (id, BlockLit(params, body)) =>
        "def" <+> toDoc(id.name) <> parens(params map toDoc) <+> "=" <+> nested(toDocStmt(body))
    }
    handlerName <+> block(vsep(clauses))
  }

  def toDocStmt(s: Stmt): Doc = s match {
    case Def(id, tpe, Extern(ps, body), rest) =>
      "extern def" <+> toDoc(id.name) <+> "=" <+> parens(hsep(ps map toDoc, comma)) <+> "=" <+> "\"" <> body <> "\"" <> emptyline <>
        toDocStmt(rest)

    case Def(id, tpe, BlockLit(params, body), rest) =>
      "def" <+> toDoc(id.name) <> parens(params map toDoc) <+> "=" <> nested(toDocStmt(body)) <> emptyline <>
        toDocStmt(rest)

    case Def(id, tpe, b, rest) =>
      "def" <+> toDoc(id.name) <+> "=" <+> toDoc(b) <> emptyline <>
        toDocStmt(rest)

    case Data(did, ctors, rest) =>
      "type" <+> toDoc(did.name) <> parens(ctors.map { id => toDoc(id.name) }) <> emptyline <>
        toDocStmt(rest)

    case Record(did, fields, rest) =>
      "record" <+> toDoc(did.name) <> parens(fields.map { f => toDoc(f.name) }) <> emptyline <>
        toDocStmt(rest)

    case Val(Wildcard(_), tpe, binding, body) =>
      toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case Val(id, tpe, binding, body) =>
      "val" <+> toDoc(id.name) <+> "=" <+> toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case Let(id, tpe, binding, body) =>
      "let" <+> toDoc(id.name) <+> "=" <+> toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case App(b, targs, args) =>
      toDoc(b) <> parens(hsep(args map argToDoc, comma))

    case If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> block(toDocStmt(thn)) <+> "else" <+> block(toDocStmt(els))

    case While(cond, body) =>
      "while" <+> parens(toDoc(cond)) <+> block(toDoc(body)) <+> line

    case Return(e) =>
      toDoc(e)

    case Handle(body, hs) =>
      "try" <+> toDoc(body) <+> "with" <+> hsep(hs.map(toDoc), " with")

    case State(id, init, region, body) =>
      "var" <+> toDoc(id.name) <+> "in" <+> toDoc(region.name) <+> "=" <+> toDoc(init) <+> ";" <> line <> toDoc(body)

    case Region(body) =>
      "region" <+> toDoc(body)

    case Match(sc, clauses) =>
      val cs = braces(nest(line <> vsep(clauses map { case (p, b) => "case" <+> toDoc(p) <+> "=>" <+> toDoc(b) })) <> line)
      toDoc(sc) <+> "match" <+> cs

    case Hole =>
      "<>"

    // for now, don't print includes
    case Include(contents, rest) =>
      toDocStmt(rest)
  }

  def requiresBlock(s: Stmt): Boolean = s match {
    case Data(did, ctors, rest) => true
    case Def(id, tpe, d, rest) => true
    case _ => false
  }

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))

}
