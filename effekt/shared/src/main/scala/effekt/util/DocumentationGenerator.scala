package effekt.util

import effekt.source.*
import kiama.util.Source
import scala.collection.immutable.HashMap

// TODO: positions
case class DocumentationGenerator(ast: ModuleDecl, name: String = "") extends Source {
  type Documentation = HashMap[String, DocValue]

  // A recursive structure that resembles JSON
  sealed trait DocValue
  case class DocString(value: String) extends DocValue
  case class DocArray(value: Vector[DocValue]) extends DocValue
  case class DocObject(value: Documentation) extends DocValue

  def empty = DocObject(HashMap.empty)
  def str(s: String) = DocString(s)
  def arr(docs: Vector[DocValue]) = DocArray(docs)
  def obj(doc: Documentation) = DocObject(doc)

  def generate(doc: Doc): DocString = str(s"${doc.getOrElse("").replace("\"", "\\\"")}")

  def generateTparams(list: List[Id]): DocString = str(s"[${list.map(_.name).mkString(", ")}]")

  def generate(effects: Effects): DocValue = arr(effects.effs.map(generate).toVector)

  def generate(effectful: Effectful): DocValue = effectful match {
    case Effectful(tpe, eff) => obj(HashMap(
      "kind" -> str("operation"),
      "tpe" -> generate(tpe),
      "eff" -> generate(eff),
    ))
  }

  def generate(tpe: BlockType): DocValue = tpe match {
    case BlockTypeTree(eff) => ??? // ignore?
    case FunctionType(tparams, vparams, bparams, result, effects) => obj(HashMap(
      "kind" -> str("FunctionType"),
      "tparams" -> generateTparams(tparams),
      // "vparams" -> generateVparams(vparams),
      // "bparams" -> generateBparams(bparams),
      "result" -> generate(result),
      "effects" -> generate(effects),
    ))
    case BlockTypeRef(id, args) => obj(HashMap(
      "kind" -> str("BlockTypeRef"),
      "id" -> generate(id),
      "args" -> arr(args.map(generate).toVector)
    ))
  }

  // TODO: should we provide more information here?
  def generate(id: IdDef): DocValue = str(id.name)
  def generate(id: IdRef): DocValue = str(id.name)

  def generate(capt: CaptureSet): DocValue = arr(capt.captures.map(generate).toVector)

  def generate(tpe: ValueType): DocValue = tpe match {
    case ValueTypeTree(tpe) => ??? // ignore?
    case BoxedType(tpe, capt) => obj(HashMap(
      "kind" -> str("BoxedType"),
      "tpe" -> generate(tpe),
      "capt" -> generate(capt),
    ))
    case ValueTypeRef(id, args) => obj(HashMap(
      "kind" -> str("BoxedType"),
      "id" -> generate(id),
      "args" -> arr(args.map(generate).toVector)
    ))
  }

  def generateVparams(list: List[ValueParam]): DocValue = arr(list.map {
    case ValueParam(id, tpe) =>
      obj(HashMap(
        "kind" -> str("ValueParam"),
        "id" -> generate(id),
        "tpe" -> tpe.map(generate).getOrElse(empty),
      ))
  }.toVector)

  def generateBparams(list: List[BlockParam]): DocValue = arr(list.map {
    case BlockParam(id, tpe) =>
      obj(HashMap(
        "kind" -> str("BlockParam"),
        "id" -> generate(id),
        "tpe" -> tpe.map(generate).getOrElse(empty),
      ))
  }.toVector)

  def generate(definition: Def): Documentation = definition match {
    case Def.FunDef(id, tparams, vparams, bparams, ret, body, doc) => HashMap(
      "kind" -> str("FunDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "vparams" -> generateVparams(vparams),
      "bparams" -> generateBparams(bparams),
      "doc" -> generate(doc),
    )

    case Def.ValDef(id, annot, binding, doc) => HashMap(
      "kind" -> str("ValDef"),
      "id" -> generate(id),
      "doc" -> generate(doc),
    )

    case Def.RegDef(id, annot, region, binding, doc) => HashMap(
      "kind" -> str("RegDef"),
      "id" -> generate(id),
      "doc" -> generate(doc),
    )

    case Def.VarDef(id, annot, binding, doc) => HashMap(
      "kind" -> str("VarDef"),
      "id" -> generate(id),
      "doc" -> generate(doc),
    )

    case Def.DefDef(id, annot, block, doc) => HashMap(
      "kind" -> str("DefDef"),
      "id" -> generate(id),
      "doc" -> generate(doc),
    )

    case Def.NamespaceDef(id, definitions, doc) => HashMap(
      "kind" -> str("NamespaceDef"),
      "id" -> generate(id),
      "doc" -> generate(doc),
    )

    case Def.InterfaceDef(id, tparams, ops, doc) => HashMap(
      "kind" -> str("NamespaceDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "ops" -> arr(generate(ops.toVector).map(obj)),
      "doc" -> generate(doc),
    )

    case Def.DataDef(id, tparams, ctors, doc) => HashMap(
      "kind" -> str("DataDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "doc" -> generate(doc),
    )

    case Def.RecordDef(id, tparams, fields, doc) => HashMap(
      "kind" -> str("RecordDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "doc" -> generate(doc),
    )

    case Def.TypeDef(id, tparams, tpe, doc) => HashMap(
      "kind" -> str("TypeDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "doc" -> generate(doc),
    )

    case Def.EffectDef(id, tparams, effs, doc) => HashMap(
      "kind" -> str("EffectDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "doc" -> generate(doc),
    )

    case Def.ExternType(id, tparams, doc) => HashMap(
      "kind" -> str("ExternType"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "doc" -> generate(doc),
    )

    case Def.ExternDef(capture, id, tparams, vparams, bparams, ret, bodies, doc) => HashMap(
      "kind" -> str("ExternDef"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "vparams" -> generateVparams(vparams),
      "bparams" -> generateBparams(bparams),
      "doc" -> generate(doc),
    )

    case Def.ExternResource(id, tpe, doc) => HashMap(
      "kind" -> str("ExternResource"),
      "id" -> generate(id),
      "doc" -> generate(doc),
    )

    case Def.ExternInterface(id, tparams, doc) => HashMap(
      "kind" -> str("ExternInterface"),
      "id" -> generate(id),
      "tparams" -> generateTparams(tparams),
      "doc" -> generate(doc),
    )

    case Def.ExternInclude(featureFlag, path, contents, id, doc) => HashMap(
      "kind" -> str("ExternInclude"),
      "id" -> generate(id),
      "doc" -> generate(doc),
    )
  }

  def generate(operation: Operation): Documentation = operation match {
    case Operation(id, tparams, vparams, bparams, ret, doc) => HashMap(
      "kind" -> str("operation"),
      "tparams" -> generateTparams(tparams),
      "vparams" -> generateVparams(vparams),
      "bparams" -> generateBparams(bparams),
      "ret" -> generate(ret),
      "doc" -> generate(doc)
    )
  }

  def generate(nodes: Vector[Tree]): Vector[Documentation] = nodes.map { node =>
    node match {
      case n: Def => generate(n)
      case n: Operation => generate(n)
      case _ => HashMap.empty
    }
  }

  def toJSON(values: Vector[DocValue]): String = s"[${values.map(toJSON).mkString(",")}]"

  def toJSON(docValue: DocValue): String = docValue match {
    case DocString(str) => s"\"${str}\""
    case DocObject(obj) => toJSON(obj)
    case DocArray(arr) => toJSON(arr)
  }

  def toJSON(doc: Documentation): String = {
    val jsonPairs = doc.map { (key, docValue) =>
      val value = toJSON(docValue)
      s"\"$key\":$value"
    }
    s"{${jsonPairs.mkString(",")}}"
  }

  def accumulateJSON(docs: Vector[Documentation]) = docs.foldLeft("") { (acc, doc) =>
    s"${acc}, ${toJSON(doc)}"
  }.tail

  lazy val content = {
    val tree = new kiama.relation.Tree[AnyRef & Product, ModuleDecl](ast)
    val documentedNodes = tree.nodes.collect { case t: Def if t.doc.isDefined => t }
    val docs = generate(documentedNodes)

    s"{\"source\": \"${name}\", \"documentation\": [${accumulateJSON(docs)}]}"
  }
}
