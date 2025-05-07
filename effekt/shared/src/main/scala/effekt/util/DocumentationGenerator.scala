package effekt.util

import effekt.source.*
import kiama.util.Source
import scala.collection.immutable.HashMap

// TODO: include spans
trait DocumentationGenerator {
  type Documentation = HashMap[String, DocValue]

  // A recursive structure that resembles JSON
  enum DocValue {
    case DocString(value: String)
    case DocArray(value: List[DocValue])
    case DocObject(value: Documentation)
  }
  import DocValue.*

  def empty = DocObject(HashMap.empty)
  def str(s: String) = DocString(s)
  def arr(docs: List[DocValue]) = DocArray(docs)
  def obj(doc: Documentation) = DocObject(doc)

  def generate(doc: Doc): DocValue = str(doc.getOrElse("").replace("\"", "\\\""))

  def generate(effects: Effects): DocValue = arr(effects.effs.map(generate))

  def generate(tpe: Type): DocValue = tpe match {
    case TypeRef(id, args) => obj(HashMap(
      "kind" -> str("TypeRef"),
      "id" -> generate(id),
      "args" -> arr(args.unspan.map(generate))
    ))

    case Effectful(tpe, eff, span) => obj(HashMap(
      "kind" -> str("Effectful"),
      "tpe" -> generate(tpe),
      "eff" -> generate(eff),
    ))

    // block params
    case BlockTypeTree(eff) => ??? // ignore?
    case FunctionType(tparams, vparams, bparams, result, effects) => obj(HashMap(
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
    ))

    // value params
    case ValueTypeTree(tpe) => ??? // ignore?
    case BoxedType(tpe, capt) => obj(HashMap(
      "kind" -> str("BoxedType"),
      "tpe" -> generate(tpe),
      "capt" -> generate(capt),
    ))
  }

  // TODO: should we provide more information here?
  def generate(id: IdDef): DocValue = str(id.name)
  def generate(id: IdRef): DocValue = str(id.name)
  def generate(id: Id): DocValue = str(id.name)

  def generate(capt: CaptureSet): DocValue = arr(capt.captures.map(generate))

  def generateTparams(list: List[Id]): DocValue = arr(list.map(generate))

  def generateVparams(list: List[ValueParam]): DocValue = arr(list.map {
    case ValueParam(id, tpe) =>
      obj(HashMap(
        "kind" -> str("ValueParam"),
        "id" -> generate(id),
        "tpe" -> tpe.map(generate).getOrElse(empty),
      ))
  })

  def generateBparams(list: List[BlockParam]): DocValue = arr(list.map {
    case BlockParam(id, tpe) =>
      obj(HashMap(
        "kind" -> str("BlockParam"),
        "id" -> generate(id),
        "tpe" -> tpe.map(generate).getOrElse(empty),
      ))
  })

  def generate(constructor: Constructor): DocValue = constructor match {
    case Constructor(id, tparams, vparams, doc) => obj(HashMap(
      "kind" -> str("Constructor"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(vparams.unspan),
      "doc" -> generate(doc),
    ))
  }

  def generate(operation: Operation): DocValue = operation match {
    case Operation(id, tparams, vparams, bparams, ret, doc) => obj(HashMap(
      "kind" -> str("Operation"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(vparams),
      "bparams" -> generateBparams(bparams),
      "ret" -> generate(ret),
      "doc" -> generate(doc),
    ))
  }

  def generate(module: Include): DocValue = module match {
    case Include(path) => obj(HashMap(
      "kind" -> str("Include"),
      "path" -> str(path),
    ))
  }

  def generate(definition: Def): DocValue = definition match {
    case Def.FunDef(id, tparams, vparams, bparams, ret, body, doc, span) => obj(HashMap(
      "kind" -> str("FunDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(vparams.unspan),
      "bparams" -> generateBparams(bparams.unspan),
      "ret" -> ret.map(generate).getOrElse(empty),
      "doc" -> generate(doc),
    ))

    case Def.ValDef(id, annot, binding, doc) => obj(HashMap(
      "kind" -> str("ValDef"),
      "id" -> generate(id),
      "annot" -> annot.map(generate).getOrElse(empty),
      "doc" -> generate(doc),
    ))

    case Def.RegDef(id, annot, region, binding, doc) => obj(HashMap(
      "kind" -> str("RegDef"),
      "id" -> generate(id),
      "annot" -> annot.map(generate).getOrElse(empty),
      "region" -> generate(region),
      "doc" -> generate(doc),
    ))

    case Def.VarDef(id, annot, binding, doc) => obj(HashMap(
      "kind" -> str("VarDef"),
      "id" -> generate(id),
      "annot" -> annot.map(generate).getOrElse(empty),
      "doc" -> generate(doc),
    ))

    case Def.DefDef(id, annot, block, doc) => obj(HashMap(
      "kind" -> str("DefDef"),
      "id" -> generate(id),
      "annot" -> annot.map(generate).getOrElse(empty),
      "doc" -> generate(doc),
    ))

    case Def.NamespaceDef(id, definitions, doc) => obj(HashMap(
      "kind" -> str("NamespaceDef"),
      "id" -> generate(id),
      "definitions" -> arr(definitions.map(generate)),
      "doc" -> generate(doc),
    ))

    case Def.InterfaceDef(id, tparams, ops, doc) => obj(HashMap(
      "kind" -> str("InterfaceDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "ops" -> arr(ops.map(generate)),
      "doc" -> generate(doc),
    ))

    case Def.DataDef(id, tparams, ctors, doc) => obj(HashMap(
      "kind" -> str("DataDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "ctors" -> arr(ctors.map(generate)),
      "doc" -> generate(doc),
    ))

    case Def.RecordDef(id, tparams, fields, doc) => obj(HashMap(
      "kind" -> str("RecordDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(fields.unspan),
      "doc" -> generate(doc),
    ))

    case Def.TypeDef(id, tparams, tpe, doc) => obj(HashMap(
      "kind" -> str("TypeDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "tpe" -> generate(tpe),
      "doc" -> generate(doc),
    ))

    case Def.EffectDef(id, tparams, effs, doc) => obj(HashMap(
      "kind" -> str("EffectDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "effs" -> generate(effs),
      "doc" -> generate(doc),
    ))

    case Def.ExternType(id, tparams, doc) => obj(HashMap(
      "kind" -> str("ExternType"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "doc" -> generate(doc),
    ))

    case Def.ExternDef(capture, id, tparams, vparams, bparams, ret, bodies, doc, span) => obj(HashMap(
      "kind" -> str("ExternDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams.unspan),
      "vparams" -> generateVparams(vparams.unspan),
      "bparams" -> generateBparams(bparams.unspan),
      "doc" -> generate(doc),
    ))

    case Def.ExternResource(id, tpe, doc) => obj(HashMap(
      "kind" -> str("ExternResource"),
      "id" -> generate(id),
      "tpe" -> generate(tpe),
      "doc" -> generate(doc),
    ))

    case Def.ExternInterface(id, tparams, doc) => obj(HashMap(
      "kind" -> str("ExternInterface"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "doc" -> generate(doc),
    ))

    case Def.ExternInclude(featureFlag, path, contents, id, doc) => obj(HashMap(
      "kind" -> str("ExternInclude"),
      "featureFlag" -> str(featureFlag.toString),
      "path" -> str(path),
      "id" -> generate(id),
      "doc" -> generate(doc),
    ))
  }

  def generate(module: ModuleDecl): DocValue = module match {
    case ModuleDecl(path, includes, defs, doc, span) => obj(HashMap(
      "kind" -> str("ModuleDecl"),
      "path" -> str(path),
      "includes" -> arr(includes.map(generate)),
      "defs" -> arr(defs.map(generate)),
      "doc" -> generate(doc),
    ))
  }
}

case class JSONDocumentationGenerator(ast: ModuleDecl, name: String = "") extends Source, DocumentationGenerator {
  def toJSON(values: List[DocValue]): String = s"[${values.map(toJSON).mkString(",")}]"

  def toJSON(docValue: DocValue): String = docValue match {
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
