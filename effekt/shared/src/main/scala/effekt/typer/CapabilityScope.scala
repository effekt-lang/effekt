package effekt
package typer

import effekt.context.Context
import effekt.symbols.EffectWildcard
import effekt.{source, symbols}
import effekt.util.messages.ErrorMessageReifier

/**
 * Invariant: Like the result effects of Typer, all types of bound capabilities need to be concrete!
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
    C.abort(pretty"Unhandled effect ${tpe}")
}
class BindSome(binder: source.Tree, capabilities: Map[symbols.InterfaceType, symbols.BlockParam],val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindSome(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.getOrElse(tpe, parent.capabilityFor(tpe))
  override def toString: String = s"BindSome(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}
class BindAll(binder: source.Tree, var capabilities: Map[symbols.InterfaceType, symbols.BlockParam], val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindAll(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.getOrElse(tpe, {
      val freshCapability = C.freshCapabilityFor(tpe)
      capabilities = capabilities.updated(tpe, freshCapability)
      freshCapability
    })
  override def toString: String = s"BindAll(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}

class BindAllToWildcard(binder: source.Tree, var capabilities: Map[symbols.InterfaceType, symbols.BlockParam], val parent: CapabilityScope, wildcard: EffectWildcard) extends CapabilityScope {
  def copy: CapabilityScope = BindAllToWildcard(binder, capabilities, parent.copy, wildcard)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.getOrElse(tpe, {
      Context.unification.substitution.updateWith(Substitutions.effects(List(wildcard), List()))

      val freshCapability = C.freshCapabilityFor(tpe)
      capabilities = capabilities.updated(tpe, freshCapability)
      freshCapability
    })
  override def toString: String = s"BindAllToWildcard(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}