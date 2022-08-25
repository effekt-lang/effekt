package effekt
package generator

import effekt.{ CompilationUnit, CoreTransformed, Phase }
import effekt.context.Context
import effekt.core.*
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol, Wildcard }
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import effekt.context.assertions.*

import scala.language.implicitConversions
import effekt.util.paths.*

trait ChezSchemePrinterUtils extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  val prelude = "#!/usr/local/bin/scheme --script\n\n(import (chezscheme))\n\n"

  def moduleFile(path: String): String = path.replace('/', '_') + ".ss"

  val emptyline: Doc = line <> line

  // we prefix op$ to effect operations to avoid clashes with reserved names like `get` and `set`
  def nameDef(id: Symbol)(implicit C: Context): Doc =
    id.name.toString + "_" + id.id

  def nameRef(id: Symbol)(implicit C: Context): Doc =
    id.name.toString + "_" + id.id

  def generateConstructor(ctor: effekt.symbols.Record)(implicit C: Context): Doc =
    generateConstructor(ctor, ctor.fields)

  // https://www.scheme.com/csug8/objects.html
  // https://scheme.com/tspl4/records.html
  def generateConstructor(did: Symbol, fields: List[Symbol])(implicit C: Context): Doc = {
    val pred = nameDef(did) <> "?"
    val matcher = "match-" <> nameDef(did)
    val recordType = did.name.toString <> "$Type" <> did.id.toString
    // rethrowing an effect causes some unexpected shadowing since the constructor and effect names are called the same.
    val constructor = if (did.isInstanceOf[effekt.symbols.InterfaceType]) "make-" <> nameDef(did) else nameDef(did)

    val definition =
      parens("define-record-type" <+> parens(recordType <+> constructor <+> pred) <>
        nest(line <> parens("fields" <+> nest(line <> vsep(fields.map { f => parens("immutable" <+> nameDef(f) <+> nameDef(f)) }))) <> line <>
          parens("nongenerative" <+> nameDef(did))))

    var fresh = 0;

    val showRecord = {
      val tpe = '"' <> did.name.toString <> '"'
      val fieldNames = fields.map { f => space <> parens(nameRef(f) <+> "r") <> space }
      parens("define" <+> parens("show" <> recordType <+> "r") <>
        nest(line <>
          "(string-append" <+> tpe <+> "\"(\"" <> hsep(fieldNames, " \", \" ") <> "\")\"" <> ")"))
    }

    val matcherDef =
      parens("define-matcher" <+> matcher <+> pred <>
        nest(line <> parens(vsep(fields.map { f =>
          fresh += 1
          brackets(s"p${fresh}" <+> nameDef(f))
        }))))

    s";;; Record definition for ${did.name}" <> line <> definition <> line <> matcherDef <> line
  }

  def defineValue(name: Doc, binding: Doc): Doc =
    parens("define" <+> name <+> binding)

  def defineFunction(name: Doc, params: List[Doc], body: Doc): Doc =
    parens("define" <+> name <+> schemeLambda(params, body))

  def schemeLambda(params: List[Doc], body: Doc): Doc =
    parens("lambda" <+> parens(hsep(params, space)) <> group(nest(line <> body)))

  def schemeLet(name: Doc, binding: Doc)(body: Doc): Doc =
    parens("let" <+> parens(brackets(name <+> binding)) <> group(nest(line <> body)))

  def jsString(contents: Doc): Doc =
    "\"" <> contents <> "\""

  def schemeCall(fun: Doc, args: Doc*): Doc = schemeCall(fun, args.toList)
  def schemeCall(fun: Doc, args: List[Doc]): Doc = parens(hsep(fun :: args, space))

}
