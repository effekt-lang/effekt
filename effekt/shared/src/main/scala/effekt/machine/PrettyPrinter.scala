package effekt
package machine

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def format(stmt: Statement): Document = pretty(toDoc(stmt), 2)

  implicit def toDoc(v: Variable): Doc = string(v.name)

  implicit def toDoc(v: Label): Doc = string(v.name)

  def toDoc(clause: Clause): Doc = clause match {
    case Clause(parameters, body) => braces(space <> toDoc(parameters) <+> "=>" <> group(nest(line <> toDocStmts(body)) <> line))
  }

  def toDoc(e: Environment): Doc = parens(e map {
    case Variable(name, tpe) => name <+> ":" <+> toDoc(tpe)
  })

  def toDoc(tpe: Type): Doc = tpe match {
    case Positive()          => "Positive"
    case Negative()          => "Negative"
    case Type.Prompt()       => "Prompt"
    case Type.Stack()        => "Stack"
    case Type.Int()          => "Int"
    case Type.Byte()         => "Byte"
    case Type.Double()       => "Double"
    case Type.Reference(tpe) => toDoc(tpe) <> "*"
  }

  def toDoc(stmt: Statement): Doc =
    stmt match {
      // terminators that do not require a block to be readable:
      case _ : (Return | Jump | Invoke | Switch) => toDocStmts(stmt)
      case other => block(toDocStmts(stmt))
    }


  def toDocStmts(stmt: Statement): Doc = stmt match {
    case Def(label, body, rest) =>
      "def" <+> label <+> "=" <+> block(toDocStmts(body)) <> ";" <> line <> toDocStmts(rest)

    case Jump(label) =>
      "jump" <+> label

    case Substitute(List((left, right)), rest) =>
      "let" <+> left <+> "=" <+> right <> ";" <> line <> toDocStmts(rest)

    case Substitute(bindings, rest) =>
      "let" <+> braces(bindings map { case (left, right) => left <+> "=" <+> right }) <> ";" <> line <> toDocStmts(rest)

    case Construct(name, tag, arguments, rest) =>
      "let" <+> name <+> "=" <+> tag.toString <> parens(arguments map toDoc) <> ";" <> line <> toDocStmts(rest)

    case Switch(scrutinee, clauses, default) =>
      val cls = clauses.map { case (idx, cl) => idx.toString <+> ":" <+> toDoc(cl) }
      val d = default.map(d => space <> "else" <+> toDoc(d)).getOrElse(emptyDoc)
      "switch" <+> scrutinee <+> line <> indent(vcat(cls)) <> d

    case New(name, operations, rest) =>
      "let" <+> name <+> "=" <+> "new" <+> block(operations map toDoc) <> ";" <> line <> toDocStmts(rest)

    case Invoke(receiver, tag, arguments) =>
      "invoke" <+> receiver <> "." <> tag.toString <> parens(arguments map toDoc)

    case Var(name, init, _, rest) =>
      "var" <+> name <+> "=" <+> toDoc(init) <> ";" <> line <> toDocStmts(rest)

    case LoadVar(name, reference, rest) =>
      "let" <+> name <+> "=" <+> "loadVar" <> parens(toDoc(reference)) <> ";" <> line <> toDocStmts(rest)

    case StoreVar(reference, value, rest) =>
      "storeVar" <> parens(List(reference, value) map toDoc) <> ";" <> line <> toDocStmts(rest)

    case PushFrame(Clause(parameters, body), rest) =>
      "val" <+> toDoc(parameters) <+> "=" <+> toDoc(rest) <> ";" <> line <>
        toDocStmts(body)

    case Return(arguments) =>
      "return" <+> hsep(arguments map toDoc, ",")

    case Reset(prompt, frame, rest) =>
      "let" <+> prompt <+> "=" <+> "reset" <+> toDoc(frame) <> ";" <> line <> toDocStmts(rest)

    case Resume(stack, rest) =>
      "resume" <+> stack <> ";" <> line <> toDocStmts(rest)

    case Shift(name, prompt, rest) =>
      "let" <+> name <+> "=" <+> "shift0p" <+> prompt <> ";" <> line <> toDocStmts(rest)

    case ForeignCall(name, builtin, arguments, rest) =>
      "let" <+> name <+> "=" <+> builtin <> parens(arguments map toDoc) <> ";" <> line <> toDocStmts(rest)

    case LiteralInt(name, value, rest) =>
      "let" <+> name <+> "=" <+> value.toString <> ";" <> line <> toDocStmts(rest)

    case LiteralDouble(name, value, rest) =>
      "let" <+> name <+> "=" <+> value.toString <> ";" <> line <> toDocStmts(rest)

    case LiteralUTF8String(name, utf8, rest) =>
      "let" <+> name <+> "=" <+> ("\"" + (utf8.map { b => "\\" + f"$b%02x" }).mkString + "\"") <> ";" <> line <> toDocStmts(rest)

    case Hole => "<>"
  }

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def braces(docs: List[Doc]): Doc = braces(hsep(docs, comma))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))
}
