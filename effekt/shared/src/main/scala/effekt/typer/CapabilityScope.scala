package effekt
package typer

import effekt.context.Context
import effekt.context.assertions.*
import effekt.{ source, symbols }
import effekt.util.messages.ErrorMessageReifier

/**
 * Capabilities are found by the interface of the effect: the closest capability for an interface shadows all others.
 */
sealed trait CapabilityScope {
  def copy: CapabilityScope
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam
  def parent: CapabilityScope
}
case object GlobalCapabilityScope extends CapabilityScope {
  def copy: CapabilityScope = this
  def parent: CapabilityScope = sys error "No parent"
  // If we try to find a capability for an effect that is known to be unhandled (that is no outer scope could
  // potentially handle it, then we raise an error.
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    C.abort(pretty"Effect ${tpe} is not allowed in this context.")
}

def unifyCapability(tpe: symbols.InterfaceType, capability: symbols.BlockParam)(using C: Context): Unit =
  val bound = capability.tpe.getOrElse { effekt.util.messages.INTERNAL_ERROR("Capability type needs to be know.") }.asInterfaceType
  C.requireSubtype(tpe, bound, ErrorContext.Expected(C.unification(tpe), C.unification(bound), C.focus, None, Some(ErrorContext.Because.ClosestCapability)))

class BindSome(binder: source.Tree, capabilities: Map[symbols.BlockTypeConstructor, symbols.BlockParam], val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindSome(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.get(tpe.typeConstructor) match {
      case Some(capability) =>
        unifyCapability(tpe, capability)
        capability
      case None => parent.capabilityFor(tpe)
    }
  override def toString: String = s"BindSome(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}
class BindAll(binder: source.Tree, var capabilities: Map[symbols.BlockTypeConstructor, symbols.BlockParam], val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindAll(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.get(tpe.typeConstructor) match {
      case Some(capability) =>
        unifyCapability(tpe, capability)
        capability
      case None =>
        val freshCapability = C.freshCapabilityFor(tpe)
        capabilities = capabilities.updated(tpe.typeConstructor, freshCapability)
        freshCapability
    }
  override def toString: String = s"BindAll(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}
