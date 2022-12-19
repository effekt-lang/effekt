package effekt.lifted

import effekt.core.Id
import effekt.util.messages.ErrorReporter

class ExternContext(val externs: List[Extern]) {

  lazy val types = externs.collect{
    case t: Extern.Type => (t.id -> t)
  }.toMap

  def findExternType(id: Id): Option[Extern.Type] = types.get(id)
  def getExternType(id: Id)(using E: ErrorReporter): Extern.Type = findExternType(id).getOrElse{
    E.panic(s"Cannot find extern type declaration for ${id}")
  }

}