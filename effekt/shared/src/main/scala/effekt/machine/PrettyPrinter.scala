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
    case Clause(parameters, body) => braces(space <> toDoc(parameters) <+> "=>" <> group(nest(line <> toDoc(body)) <> line))
  }

  def toDoc(e: Environment): Doc = parens(e map {
    case Variable(name, tpe) => name <+> ":" <+> toDoc(tpe)
  })

  def toDoc(tpe: Type): Doc = tpe match {
    case Positive(List(Nil))      => "Unit"
    case Positive(List(Nil, Nil)) => "Bool"
    case Type.Int()               => "Int"
    case Type.Double()            => "Double"
    case Positive(alternatives)   => brackets(hsep(alternatives map signatureToDoc, ";"))
    case Negative(alternatives)   => braces(hsep(alternatives map signatureToDoc, ";"))
    case Type.Stack()             => "Stack"
  }

  def signatureToDoc(sig: Signature): Doc = parens(sig map toDoc)

  def toDoc(stmt: Statement): Doc = stmt match {
    case Def(label, body, rest) =>
      "def" <+> label <+> "=" <+> block(toDoc(body)) <> ";" <> line <> toDoc(rest)

    case Jump(label) =>
      "jump" <+> label

    case Substitute(bindings, rest) =>
      "subst" <+> brackets(bindings map { case (to, from) => from <+> "!->" <+> to }) <> ";" <> line <> toDoc(rest)

    case Construct(name, tag, arguments, rest) =>
      "let" <+> name <+> "=" <+> tag.toString <> parens(arguments map toDoc) <> ";" <> line <> toDoc(rest)

    // TODO add tags to variants.
    case Switch(scrutinee, clauses) =>
      val cls = clauses.zipWithIndex.map { case (cl, idx) => idx.toString <+> ":" <+> toDoc(cl) }
      "switch" <+> scrutinee <+> line <> indent(vcat(cls))

    case New(name, operations, rest) =>
      "let" <+> name <+> "=" <+> "new" <+> block(operations map toDoc) <> ";" <> line <> toDoc(rest)

    case Invoke(receiver, tag, arguments) =>
      "invoke" <+> receiver <> "." <> tag.toString <> parens(arguments map toDoc)

    case PushFrame(frame, rest) =>
      "push" <+> toDoc(frame) <> ";" <> line <> toDoc(rest)

    case Return(arguments) =>
      "return" <+> hsep(arguments map toDoc, ",")

    case NewStack(name, frame, rest) =>
      "let" <+> name <+> "=" <+> "stack" <+> toDoc(frame) <> ";" <> line <> toDoc(rest)

    case PushStack(stack, rest) =>
      "push stack" <+> stack <> ";" <> line <> toDoc(rest)

    case PopStack(name, rest) =>
      "let" <+> name <+> "=" <+> "shift0" <> ";" <> line <> toDoc(rest)

    case ForeignCall(name, builtin, arguments, rest) =>
      "let" <+> name <+> "=" <+> builtin <> parens(arguments map toDoc) <> ";" <> line <> toDoc(rest)

    case LiteralInt(name, value, rest) =>
      "let" <+> name <+> "=" <+> value.toString <> ";" <> line <> toDoc(rest)

    case LiteralDouble(name, value, rest) =>
      "let" <+> name <+> "=" <+> value.toString <> ";" <> line <> toDoc(rest)
  }

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))
}
