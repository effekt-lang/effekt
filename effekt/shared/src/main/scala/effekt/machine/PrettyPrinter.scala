package effekt
package machine

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def format(stmt: Statement): Document = pretty(toDoc(stmt), 2)

  def evidenceToDoc(value: Evidence): Doc = value match {
    case builtins.Here => "Here"
    case builtins.There => "There"
    case n => "There + " + evidenceToDoc(n)
  }

  implicit def toDoc(v: Variable): Doc = string(v.name)

  implicit def toDoc(v: Label): Doc = string(v.name)

  def toDoc(clause: Clause): Doc = clause match {
    case Clause(parameters, body) => braces(space <> toDoc(parameters) <+> "=>" <> group(nest(line <> toDoc(body)) <> line))
  }

  def toDoc(e: Environment): Doc = parens(e map {
    case Variable(name, tpe) => name <+> ":" <+> toDoc(tpe)
  })

  def toDoc(tpe: Type): Doc = tpe match {
    case Positive()          => "Positive"
    case Negative()          => "Negative"
    case Type.Int()          => "Int"
    case Type.Byte()         => "Byte"
    case Type.Double()       => "Double"
    case Type.String()       => "String"
    case Type.Stack()        => "Stack"
    case Type.Reference(tpe) => toDoc(tpe) <> "*"
  }

  def toDoc(stmt: Statement): Doc = stmt match {
    case Def(label, body, rest) =>
      "def" <+> label <+> "=" <+> block(toDoc(body)) <> ";" <> line <> toDoc(rest)

    case Jump(label) =>
      "jump" <+> label

    case Substitute(bindings, rest) =>
      "subst" <+> brackets(bindings map { case (left, right) => left <+> "!->" <+> right }) <> ";" <> line <> toDoc(rest)

    case Construct(name, tag, arguments, rest) =>
      "let" <+> name <+> "=" <+> tag.toString <> parens(arguments map toDoc) <> ";" <> line <> toDoc(rest)

    case Switch(scrutinee, clauses, default) =>
      val cls = clauses.map { case (idx, cl) => idx.toString <+> ":" <+> toDoc(cl) }
      val d = default.map(d => space <> "else" <+> toDoc(d)).getOrElse(emptyDoc)
      "switch" <+> scrutinee <+> line <> indent(vcat(cls)) <> d

    case New(name, operations, rest) =>
      "let" <+> name <+> "=" <+> "new" <+> block(operations map toDoc) <> ";" <> line <> toDoc(rest)

    case Invoke(receiver, tag, arguments) =>
      "invoke" <+> receiver <> "." <> tag.toString <> parens(arguments map toDoc)

    case Allocate(name, init, ev, rest) =>
      "let" <+> name <+> "=" <+> "allocate" <> parens(List(init, ev) map toDoc) <> ";" <> line <> toDoc(rest)

    case Load(name, ref, ev, rest) =>
      "let" <+> name <+> "=" <+> "load" <> parens(List(ref, ev) map toDoc) <> ";" <> line <> toDoc(rest)

    case Store(ref, value, ev, rest) =>
      "store" <> parens(List(ref, ev, value) map toDoc) <> ";" <> line <> toDoc(rest)

    case PushFrame(frame, rest) =>
      "push" <+> toDoc(frame) <> ";" <> line <> toDoc(rest)

    case Return(arguments) =>
      "return" <+> hsep(arguments map toDoc, ",")

    case NewStack(name, prompt, frame, rest) =>
      "let" <+> name <+> "=" <+> "stack" <> parens(toDoc(prompt)) <+> toDoc(frame) <> ";" <> line <> toDoc(rest)

    case PushStack(stack, rest) =>
      "push stack" <+> stack <> ";" <> line <> toDoc(rest)

    case PopStacks(name, n, rest) =>
      "let" <+> name <+> "=" <+> "shift0" <+> n <> ";" <> line <> toDoc(rest)

    case PopStacksPrompt(name, prompt, rest) =>
      "let" <+> name <+> "=" <+> "shift0p" <+> prompt <> ";" <> line <> toDoc(rest)

    case FreshPrompt(name, rest) =>
      "let" <+> name <+> "=" <+> "freshPrompt" <> ";" <> line <> toDoc(rest)

    case ComposeEvidence(name, ev1, ev2, rest) =>
      "let" <+> name <+> "=" <+> ev1 <+> "+" <+> ev2 <> ";" <> line <> toDoc(rest)

    case ForeignCall(name, builtin, arguments, rest) =>
      "let" <+> name <+> "=" <+> builtin <> parens(arguments map toDoc) <> ";" <> line <> toDoc(rest)

    case LiteralInt(name, value, rest) =>
      "let" <+> name <+> "=" <+> value.toString <> ";" <> line <> toDoc(rest)

    case LiteralDouble(name, value, rest) =>
      "let" <+> name <+> "=" <+> value.toString <> ";" <> line <> toDoc(rest)

    case LiteralUTF8String(name, utf8, rest) =>
      "let" <+> name <+> "=" <+> ("\"" + (utf8.map { b => "\\" + f"$b%02x" }).mkString + "\"") <> ";" <> line <> toDoc(rest)

    case LiteralEvidence(name, evidence, rest) =>
      "let" <+> name <+> "=" <+> evidenceToDoc(evidence) <> ";" <> line <> toDoc(rest)

    case Hole => "<>"
  }

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))
}
