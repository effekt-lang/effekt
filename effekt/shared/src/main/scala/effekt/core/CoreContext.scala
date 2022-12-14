package effekt.core

import effekt.context.Context

/**
 * Context for transformations of a [[core.ModuleDecl]] that provides the declarations for this module.
 * Also wraps the [[effekt.context.Context]] for convenience.
 */
class CoreContext(val declarations: List[Declaration])(using val context: Context) {
  // TODO This might be quite inperformant (searching through everything for each operation)?

  lazy val datas = declarations.collect {
    case data: Declaration.Data => data
  }
  lazy val interfaces = declarations.collect{
    case ifce: Declaration.Interface => ifce
  }
  lazy val constructors = datas.flatMap(_.constructors)
  lazy val fields = constructors.flatMap(_.fields)
  lazy val properties = interfaces.flatMap(_.properties)

  def findDeclaration(id: Id): Option[Declaration] = declarations.find(_.id == id)
  def findData(id: Id): Option[Declaration.Data] = datas.find(_.id == id)
  def findData(constructor: Constructor): Option[Declaration.Data] =
    datas.find(_.constructors contains constructor)
  def findData(field: Field): Option[Declaration.Data] =
    datas.find(_.constructors.exists(_.fields contains field))
  def findInterface(id: Id): Option[Declaration.Interface] = interfaces.find(_.id == id)
  def findInterface(property: Property): Option[Declaration.Interface] =
    interfaces.find(_.properties contains property)
  def findConstructor(id: Id): Option[Constructor] = constructors.find(_.id == id)
  def findConstructor(field: Field): Option[Constructor] = constructors.find(_.fields contains field)
  def findField(id: Id): Option[Field] = fields.find(_.id == id)
  def findProperty(id: Id): Option[Property] = properties.find(_.id == id)

  def getDeclaration(id: Id): Declaration = findDeclaration(id).getOrElse{
    context.panic(s"No declaration found for ${id}.")
  }
  def getData(id: Id): Declaration.Data = findData(id).getOrElse{
    context.panic(s"No declaration found for data type ${id}.")
  }
  def getData(constructor: Constructor): Declaration.Data = findData(constructor).getOrElse{
    import PrettyPrinter.*
    context.panic(s"No declaration found for data type with constructor ${pretty(toDoc(constructor)).layout}")
  }
  def getData(field: Field): Declaration.Data = findData(field).getOrElse{
    import PrettyPrinter.*
    context.panic(s"No declaration found for data type with field ${pretty(toDoc(field)).layout}")
  }
  def getInterface(id: Id): Declaration.Interface = findInterface(id).getOrElse{
    context.panic(s"No declaration found for interface ${id}")
  }
  def getInterface(property: Property): Declaration.Interface = findInterface(property).getOrElse{
    import PrettyPrinter.*
    context.panic(s"No declaration found for interface with property ${pretty(toDoc(property)).layout}")
  }
  def getConstructor(id: Id): Constructor = findConstructor(id).getOrElse{
    context.panic(s"No declaration found for constructor ${id}")
  }
  def getConstructor(field: Field): Constructor = findConstructor(field).getOrElse{
    import PrettyPrinter.*
    context.panic(s"No declaration found for constructor with field ${pretty(toDoc(field)).layout}")
  }
  def getField(id: Id): Field = findField(id).getOrElse{
    context.panic(s"No declaration found for field ${id}")
  }
  def getProperty(id: Id): Property = findProperty(id).getOrElse{
    context.panic(s"No declaration found for property ${id}")
  }

  extension(field: Field) {
    def constructor: Constructor = getConstructor(field)
    def data: Declaration.Data = getData(field)
  }
  extension(constructor: Constructor) {
    def data: Declaration.Data = getData(constructor)
  }
  extension(property: Property) {
    def interface: Declaration.Interface = getInterface(property)
  }
}
given (using C: CoreContext): Context = C.context