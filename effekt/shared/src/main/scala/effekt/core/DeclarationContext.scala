package effekt.core

import effekt.util.messages.ErrorReporter
import PrettyPrinter.*

/**
 * Context for transformations of a [[core.ModuleDecl]] that provides the declarations for this module.
 *
 * The `find*` methods return `Option[*]`, the `get*` call `Context.panic` if no matching declaration can be found.
 * Also provides some extension methods for convenience.
 */
class DeclarationContext(
  val declarations: List[Declaration],
  val externs: List[Extern]
) {

  lazy val externDefs: Map[Id, Extern.Def] = externs.collect {
     case d: Extern.Def => d.id -> d
   }.toMap

  // Maps to speed-up lookup. Assumes that, if we ever lookup a Constructor/Field/Interface/...,
  // we will eventually lookup most of them (and caches all in a respective map).

  lazy val datas: Map[Id, Declaration.Data] = declarations.collect {
    case data: Declaration.Data => data.id -> data
  }.toMap

  lazy val interfaces: Map[Id, Declaration.Interface] = declarations.collect {
    case ifce: Declaration.Interface => ifce.id -> ifce
  }.toMap

  case class ConstructorRef(data: Declaration.Data, constructor: Constructor)
  lazy val constructors: Map[Id, ConstructorRef] = datas.values.flatMap{ data =>
    data.constructors.map { cns => cns.id -> ConstructorRef(data, cns) }
  }.toMap

  case class FieldRef(constructorRef: ConstructorRef, field: Field) { export constructorRef.* }
  lazy val fields: Map[Id, FieldRef] = constructors.values.flatMap { c =>
    c.constructor.fields.map { f => f.id -> FieldRef(c, f) }
  }.toMap

  case class PropertyRef(interface: Declaration.Interface, property: Property)
  lazy val properties: Map[Id, PropertyRef] = interfaces.values.flatMap { i =>
    i.properties.map { p => p.id -> PropertyRef(i, p) }
  }.toMap

  def findDeclaration(id: Id): Option[Declaration] = declarations.find(_.id == id)
  def findData(id: Id): Option[Declaration.Data] = datas.get(id)
  def findData(constructor: Constructor): Option[Declaration.Data] =
    constructors.get(constructor.id).map(_.data)

  def findData(field: Field): Option[Declaration.Data] = fields.get(field.id).map(_.data)
  def findInterface(id: Id): Option[Declaration.Interface] = interfaces.get(id)
  def findInterface(property: Property): Option[Declaration.Interface] = properties.get(property.id).map(_.interface)

  def findConstructor(id: Id): Option[Constructor] = constructors.get(id).map(_.constructor)
  def findConstructor(field: Field): Option[Constructor] = fields.get(field.id).map(_.constructor)
  def findConstructorTag(id: Id): Option[Int] = {
    constructors.get(id).flatMap{ case ConstructorRef(data, constructor) =>
      val tag = data.constructors.indexOf(constructor)
      if tag == -1 then None else Some(tag)
    }
  }
  def findField(id: Id): Option[Field] = fields.get(id).map(_.field)
  def findProperty(id: Id): Option[Property] = properties.get(id).map(_.property)
  def findPropertyTag(id: Id): Option[Int] = {
    properties.get(id).flatMap{ case PropertyRef(interface, property) =>
      val tag = interface.properties.indexOf(property)
      if tag == -1 then None else Some(tag)
    }
  }

  def findExternDef(id: Id): Option[Extern.Def] = externDefs.get(id)

  def getDeclaration(id: Id)(using context: ErrorReporter): Declaration = findDeclaration(id).getOrElse {
    context.panic(s"No declaration found for ${id}.")
  }
  def getData(id: Id)(using context: ErrorReporter): Declaration.Data = findData(id).getOrElse {
    context.panic(s"No declaration found for data type ${id}.")
  }
  def getData(constructor: Constructor)(using context: ErrorReporter): Declaration.Data = findData(constructor).getOrElse {
    context.panic(s"No declaration found for data type with constructor ${pretty(toDoc(constructor)).layout}")
  }
  def getData(field: Field)(using context: ErrorReporter): Declaration.Data = findData(field).getOrElse {
    context.panic(s"No declaration found for data type with field ${pretty(toDoc(field)).layout}")
  }
  def getInterface(id: Id)(using context: ErrorReporter): Declaration.Interface = findInterface(id).getOrElse {
    context.panic(s"No declaration found for interface ${id}")
  }
  def getInterface(property: Property)(using context: ErrorReporter): Declaration.Interface = findInterface(property).getOrElse {
    context.panic(s"No declaration found for interface with property ${pretty(toDoc(property)).layout}")
  }
  def getConstructor(id: Id)(using context: ErrorReporter): Constructor = findConstructor(id).getOrElse {
    context.panic(s"No declaration found for constructor ${id}")
  }
  def getConstructor(field: Field)(using context: ErrorReporter): Constructor = findConstructor(field).getOrElse {
    context.panic(s"No declaration found for constructor with field ${pretty(toDoc(field)).layout}")
  }
  def getConstructorTag(id: Id)(using context: ErrorReporter): Int = findConstructorTag(id).getOrElse {
    context.panic(s"No declaration found for constructor ${id}")
  }
  def getField(id: Id)(using context: ErrorReporter): Field = findField(id).getOrElse {
    context.panic(s"No declaration found for field ${id}")
  }
  def getProperty(id: Id)(using context: ErrorReporter): Property = findProperty(id).getOrElse {
    context.panic(s"No declaration found for property ${id}")
  }
  def getPropertyTag(id: Id)(using context: ErrorReporter): Int = findPropertyTag(id).getOrElse{
    context.panic(s"No declaration found for property ${id}")
  }
  def getExternDef(id: Id)(using context: ErrorReporter): Extern.Def = findExternDef(id).getOrElse{
    context.panic(s"No extern definition found for ${id}")
  }

  extension(field: Field) {
    def constructor(using context: ErrorReporter): Constructor = getConstructor(field)
    def data(using context: ErrorReporter): Declaration.Data = getData(field)
  }
  extension(constructor: Constructor) {
    def data(using context: ErrorReporter): Declaration.Data = getData(constructor)
  }
  extension(property: Property) {
    def interface(using context: ErrorReporter): Declaration.Interface = getInterface(property)
  }
}
