package effekt.util

import effekt.source.*
import kiama.util.Source
import scala.collection.immutable.HashMap

// TODO: types, source?
// TODO: position!
// TODO: some are duplicates!
case class DocumentationGenerator(ast: ModuleDecl, name: String = "") extends Source {
  type Documentation = HashMap[String, DocValue]

  // A recursive structure that resembles JSON
  sealed trait DocValue
  case class DocString(value: String) extends DocValue
  case class DocArray(value: Vector[Documentation]) extends DocValue
  case class DocObject(value: Documentation) extends DocValue

  def str(s: String) = DocString(s)
  def arr(docs: Vector[Documentation]) = DocArray(docs)
  def obj(doc: Documentation) = DocObject(doc)

  def showDoc(doc: Doc): String = s"${doc.getOrElse("").replace("\"", "\\\"")}"

  def generate(definition: Def): Documentation = definition match {
    case Def.FunDef(IdDef(id), tparams, vparams, bparams, ret, body, doc) => HashMap(
      "kind" -> str("FunDef"),
      "id" -> str(id),
      "type" -> str("TODO"),
      "doc" -> str(showDoc(doc)),
    )

    case Def.ValDef(IdDef(id), annot, binding, doc) => HashMap(
      "kind" -> str("ValDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.RegDef(IdDef(id), annot, region, binding, doc) => HashMap(
      "kind" -> str("RegDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.VarDef(IdDef(id), annot, binding, doc) => HashMap(
      "kind" -> str("VarDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.DefDef(IdDef(id), annot, block, doc) => HashMap(
      "kind" -> str("DefDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.NamespaceDef(IdDef(id), definitions, doc) => HashMap(
      "kind" -> str("NamespaceDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.InterfaceDef(IdDef(id), tparams, ops, doc) => HashMap(
      "kind" -> str("NamespaceDef"),
      "id" -> str(id),
      "ops" -> arr(generate(ops.toVector)),
      "doc" -> str(showDoc(doc)),
    )

    case Def.DataDef(IdDef(id), tparams, ctors, doc) => HashMap(
      "kind" -> str("DataDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.RecordDef(IdDef(id), tparams, fields, doc) => HashMap(
      "kind" -> str("RecordDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.TypeDef(IdDef(id), tparams, tpe, doc) => HashMap(
      "kind" -> str("TypeDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.EffectDef(IdDef(id), tparams, effs, doc) => HashMap(
      "kind" -> str("EffectDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.ExternType(IdDef(id), tparams, doc) => HashMap(
      "kind" -> str("ExternType"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.ExternDef(capture, IdDef(id), tparams, vparams, bparams, ret, bodies, doc) => HashMap(
      "kind" -> str("ExternDef"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.ExternResource(IdDef(id), tpe, doc) => HashMap(
      "kind" -> str("ExternResource"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.ExternInterface(IdDef(id), tparams, doc) => HashMap(
      "kind" -> str("ExternInterface"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )

    case Def.ExternInclude(featureFlag, path, contents, IdDef(id), doc) => HashMap(
      "kind" -> str("ExternInclude"),
      "id" -> str(id),
      "doc" -> str(showDoc(doc)),
    )
  }

  def generate(operation: Operation): Documentation = operation match {
    case _ => HashMap(
      "kind" -> str("unknown"),
      "debug" -> str(operation.toString)
    )
  }

  def generate[T <: Tree](nodes: Vector[T]): Vector[Documentation] = nodes.map { node =>
    node match {
      case n: Def => generate(n)
      case n: Operation => generate(n)
      case _ => HashMap.empty
    }
  }

  def toJSON(obj: DocObject): String = toJSON(obj.value)

  def toJSON(arr: Vector[Documentation]): String = s"[${arr.map(toJSON).mkString(",")}]"

  def toJSON(doc: Documentation): String = {
    val jsonPairs = doc.map { (key, docValue) =>
      val value = docValue match {
        case DocString(str) => str.replace("\"", "\\\"")
        case DocObject(obj) => toJSON(obj)
        case DocArray(arr) => toJSON(arr)
      }
      s""""$key":"$value""""
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
