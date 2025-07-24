package effekt.util

import effekt.context.{ Annotations, Context }
import effekt.source.*
import kiama.util.Source
import scala.collection.immutable.HashMap

trait DocumentationGenerator {
  type Documentation = HashMap[String, DocValue]

  // A recursive structure that resembles JSON
  enum DocValue {
    case DocNumber(value: Int)
    case DocString(value: String)
    case DocArray(value: List[DocValue])
    case DocObject(value: Documentation)
  }
  import DocValue.*

  def empty = DocObject(HashMap.empty)
  def num(n: Int) = DocNumber(n)
  def str(s: String) = DocString(s)
  def arr(docs: List[DocValue]) = DocArray(docs)
  def obj(doc: Documentation) = DocObject(doc)

  def generate(doc: Doc): DocValue = str(doc.getOrElse("").replace("\"", "\\\""))

  def generate(span: Span): DocValue = obj(HashMap(
    "file" -> str(span.source.name),
    "lineStart" -> num(span.range.from.line),
    "lineEnd" -> num(span.range.to.line),
    "columnStart" -> num(span.range.from.column),
    "columnEnd" -> num(span.range.to.column),
  ))

  def generate(effects: Effects)(using C: Context): DocValue = arr(effects.effs.map(generate))

  def generate(tpe: Type)(using C: Context): DocValue = tpe match {
    case TypeRef(id, args, span) => obj(HashMap(
      "kind" -> str("TypeRef"),
      "id" -> generate(id),
      "args" -> arr(args.unspan.map(generate)),
      "span" -> generate(span),
    ))

    case Effectful(tpe, eff, span) => obj(HashMap(
      "kind" -> str("Effectful"),
      "tpe" -> generate(tpe),
      "eff" -> generate(eff),
    ))

    // block params
    case BlockTypeTree(eff, _) => ???
    case FunctionType(tparams, vparams, bparams, result, effects, span) => obj(HashMap(
      "kind" -> str("FunctionType"),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> arr(vparams.unspan.map(generate)),
      "bparams" -> arr(bparams.unspan.map((id, tpe) =>
        obj(HashMap(
          "kind" -> str("FunctionBlockParam"),
          "id" -> id.map(generate).getOrElse(str("")),
          "tpe" -> generate(tpe),
        ))
      )),
      "result" -> generate(result),
      "effects" -> generate(effects),
      "span" -> generate(span),
    ))

    // value params
    case ValueTypeTree(tpe, _) => ???
    case BoxedType(tpe, capt, span) => obj(HashMap(
      "kind" -> str("BoxedType"),
      "tpe" -> generate(tpe),
      "capt" -> generate(capt),
      "span" -> generate(span),
    ))
  }

  // from LSP Intelligence
  def getDefinitionOf(s: effekt.symbols.Symbol)(using C: Context): Option[Span] = s match {
    case u: effekt.symbols.UserFunction => Some(u.decl.span)
    case u: effekt.symbols.Binder       => Some(u.decl.span)
    case d: effekt.symbols.Operation    => C.definitionTreeOption(d.interface).map(_.span)
    case a: effekt.symbols.Anon         => Some(a.decl.span)
    case m: effekt.symbols.Module       => Some(m.decl.span)
    case u => C.definitionTreeOption(u).map(_.span)
  }

  def findOrigin(id: Id)(using C: Context): Option[DocValue] = for {
    sym <- C.symbolOption(id)
    dfn <- getDefinitionOf(id.symbol)
  } yield generate(dfn)

  def generate(id: Id)(using C: Context): DocValue = obj(HashMap(
    "name" -> str(id.name),
    "source" -> generate(id.span),
    "origin" -> findOrigin(id).getOrElse(empty),
  ))

  def generate(capt: CaptureSet)(using C: Context): DocValue =
    arr(capt.captures.map(generate))

  def generateTparams(list: List[Id])(using C: Context): DocValue =
    arr(list.map(generate))

  def generateVparams(list: List[ValueParam])(using C: Context): DocValue = arr(list.map {
    case ValueParam(id, tpe, span) =>
      obj(HashMap(
        "kind" -> str("ValueParam"),
        "id" -> generate(id),
        "tpe" -> tpe.map(generate).getOrElse(empty),
        "span" -> generate(span),
      ))
  })

  def generateBparams(list: List[BlockParam])(using C: Context): DocValue = arr(list.map {
    case BlockParam(id, tpe, span) =>
      obj(HashMap(
        "kind" -> str("BlockParam"),
        "id" -> generate(id),
        "tpe" -> tpe.map(generate).getOrElse(empty),
        "span" -> generate(span),
      ))
  })

  def generate(constructor: Constructor)(using C: Context): DocValue = constructor match {
    case Constructor(id, tparams, vparams, doc, span) => obj(HashMap(
      "kind" -> str("Constructor"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(vparams.unspan),
      "doc" -> generate(doc),
      "span" -> generate(span),
    ))
  }

  def generate(operation: Operation)(using C: Context): DocValue = operation match {
    case Operation(id, tparams, vparams, bparams, ret, doc, span) => obj(HashMap(
      "kind" -> str("Operation"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(vparams),
      "bparams" -> generateBparams(bparams),
      "ret" -> generate(ret),
      "doc" -> generate(doc),
      "span" -> generate(span),
    ))
  }

  def generate(module: Include): DocValue = module match {
    case Include(path, span) => obj(HashMap(
      "kind" -> str("Include"),
      "path" -> str(path),
      "span" -> generate(span),
    ))
  }

  def generate(definition: Def)(using C: Context): DocValue = definition match {
    case Def.FunDef(id, tparams, vparams, bparams, ret, body, info, span) => obj(HashMap(
      "kind" -> str("FunDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(vparams.unspan),
      "bparams" -> generateBparams(bparams.unspan),
      "ret" -> ret.map(generate).getOrElse(empty),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.ValDef(id, annot, binding, info, span) => obj(HashMap(
      "kind" -> str("ValDef"),
      "id" -> generate(id),
      "annot" -> annot.map(generate).getOrElse(empty),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.RegDef(id, annot, region, binding, info, span) => obj(HashMap(
      "kind" -> str("RegDef"),
      "id" -> generate(id),
      "annot" -> annot.map(generate).getOrElse(empty),
      "region" -> generate(region),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.VarDef(id, annot, binding, info, span) => obj(HashMap(
      "kind" -> str("VarDef"),
      "id" -> generate(id),
      "annot" -> annot.map(generate).getOrElse(empty),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.DefDef(id, annot, block, info, span) => obj(HashMap(
      "kind" -> str("DefDef"),
      "id" -> generate(id),
      "annot" -> annot.map(generate).getOrElse(empty),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.NamespaceDef(id, definitions, info, span) => obj(HashMap(
      "kind" -> str("NamespaceDef"),
      "id" -> generate(id),
      "definitions" -> arr(definitions.map(generate)),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.InterfaceDef(id, tparams, ops, info, span) => obj(HashMap(
      "kind" -> str("InterfaceDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "ops" -> arr(ops.map(generate)),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.DataDef(id, tparams, ctors, info, span) => obj(HashMap(
      "kind" -> str("DataDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "ctors" -> arr(ctors.map(generate)),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.RecordDef(id, tparams, fields, info, span) => obj(HashMap(
      "kind" -> str("RecordDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(fields.unspan),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.TypeDef(id, tparams, tpe, info, span) => obj(HashMap(
      "kind" -> str("TypeDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "tpe" -> generate(tpe),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.EffectDef(id, tparams, effs, info, span) => obj(HashMap(
      "kind" -> str("EffectDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "effs" -> generate(effs),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.ExternType(id, tparams, info, span) => obj(HashMap(
      "kind" -> str("ExternType"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.ExternDef(capture, id, tparams, vparams, bparams, ret, bodies, info, span) => obj(HashMap(
      "kind" -> str("ExternDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(vparams.unspan),
      "bparams" -> generateBparams(bparams.unspan),
      "ret" -> generate(ret),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.ExternResource(id, tpe, info, span) => obj(HashMap(
      "kind" -> str("ExternResource"),
      "id" -> generate(id),
      "tpe" -> generate(tpe),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.ExternInterface(id, tparams, info, span) => obj(HashMap(
      "kind" -> str("ExternInterface"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))

    case Def.ExternInclude(featureFlag, path, contents, id, info, span) => obj(HashMap(
      "kind" -> str("ExternInclude"),
      "featureFlag" -> str(featureFlag.toString),
      "path" -> str(path),
      "id" -> generate(id),
      "doc" -> generate(info.doc),
      "span" -> generate(span),
    ))
  }

  def generate(module: ModuleDecl)(using C: Context): DocValue = module match {
    case ModuleDecl(path, includes, defs, doc, span) => obj(HashMap(
      "kind" -> str("ModuleDecl"),
      "path" -> str(path),
      "includes" -> arr(includes.map(generate)),
      "defs" -> arr(defs.map(generate)),
      "doc" -> generate(doc),
      "span" -> generate(span),
    ))
  }
}

case class JSONDocumentationGenerator(ast: ModuleDecl, name: String = "")(using C: Context) extends Source, DocumentationGenerator {
  def toJSON(values: List[DocValue]): String = s"[${values.map(toJSON).mkString(",")}]"

  def toJSON(docValue: DocValue): String = docValue match {
    case DocValue.DocNumber(num) => num.toString
    case DocValue.DocString(str) => s"\"${str}\""
    case DocValue.DocObject(obj) => toJSON(obj)
    case DocValue.DocArray(arr) => toJSON(arr)
  }

  def toJSON(doc: Documentation): String = {
    val jsonPairs = doc.map { (key, docValue) =>
      val value = toJSON(docValue)
      s"\"$key\":$value"
    }
    s"{${jsonPairs.mkString(",")}}"
  }

  lazy val content = {
    val docs = toJSON(generate(ast))
    s"{\"source\": \"${name}\", \"module\": ${docs}}"
  }
}
