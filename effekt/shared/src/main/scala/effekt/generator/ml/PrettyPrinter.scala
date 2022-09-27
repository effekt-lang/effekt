package effekt
package generator
package ml

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  val prelude = "#!/usr/local/bin/scheme --script\n\n(import (chezscheme))\n\n"

  def toDoc(name: MLName): Doc = name.name

  def format(defs: List[Def]): Document =
    pretty(vsep(defs map toDoc, line <> line))

  def toDoc(binding: Binding): Doc = brackets(toDoc(binding.name) <+> toDoc(binding.expr))

  def toDoc(expr: Expr): Doc = expr match {
    case Call(callee, Nil)       => parens(toDoc(callee))
    case Call(callee, arguments) => parens(toDoc(callee) <+> group(align(hsep(arguments map toDoc, line))))
    case RawExpr(raw)            => string(raw)
    case RawValue(raw)           => string(raw)
    case Let(bindings, body)     => parens("let" <+> parens(align(vcat(bindings map toDoc))) <> toDoc(body))
    case Let_*(bindings, body)   => parens("let*" <+> parens(align(vcat(bindings map toDoc))) <> toDoc(body))
    case Lambda(params, body)    => parens("lambda" <+> parens(params.map(toDoc)) <> toDoc(body))
    case If(cond, thn, els)      => parens("if" <+> toDoc(cond) <> nest(line <> toDoc(thn)) <> nest(line <> toDoc(els)))
    case Variable(name)          => toDoc(name)
    case Match(sc, clauses) => parens("pattern-match" <+> toDoc(sc) <> nest(line <> parens(align(vcat(clauses.map {
      case (pattern, branch) => brackets(toDoc(pattern) <+> toDoc(branch))
    })))))
    case Handle(handlers, body) => parens("handle" <+> parens(align(vcat(handlers map toDoc))) <+> nest(line <> toDoc(body)))
  }

  def toDoc(definition: Def): Doc = definition match {
    case Def.Constant(name, value) => parens("define" <+> toDoc(name) <+> toDoc(value))
    case Def.Function(name, Nil, body) => parens("define" <+> parens(toDoc(name)) <> toDoc(body))
    case Def.Function(name, params, body) => parens("define" <+> parens(toDoc(name) <+> hsep(params map toDoc)) <> toDoc(body))
    case RawDef(raw) => string(raw)

    case Record(typeName, constructorName, predicateName, uid, fields) =>
      parens("define-record-type" <+> parens(toDoc(typeName) <+> toDoc(constructorName) <+> toDoc(predicateName)) <>
        nest(line <> parens("fields" <+> align(vcat(fields map { f => brackets("immutable" <+> toDoc(f) <+> toDoc(f)) })))) <>
        nest(line <> parens("nongenerative" <+> toDoc(uid))))
  }

  def toDoc(block: Block): Doc = block match {
    case Block(Nil, Nil, result) => nest(line <> toDoc(result))
    case Block(definitions, expressions, result) =>
      nest(line <>
        vsep(definitions map toDoc, line) <>
        vcat(expressions map toDoc) <>
        line <>
        toDoc(result))
  }

  def toDoc(h: Handler): Doc = h match {
    case Handler(constructorName, operations) =>
      brackets(toDoc(constructorName) <+> align(vcat(operations.map {
        case Operation(name, params, k, impl) =>
          parens(toDoc(name) <+> parens(hsep(params map toDoc, space)) <+> toDoc(k) <+> nest(line <> toDoc(impl)))
      })))
  }

  def parens(docs: List[Doc]): Doc = parens(hsep(docs))
}
