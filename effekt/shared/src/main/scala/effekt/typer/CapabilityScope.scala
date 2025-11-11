package effekt
package typer

import effekt.context.Context
import effekt.symbols.{Captures, Type, TypeVar, ValueType}
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
    C.abort(pretty"Effect ${tpe} is not allowed in this context.")
}

object DummyUnifier extends TypeUnifier {
  var hasError = false

  def checkSubtype(t1: effekt.symbols.ValueType, t2: effekt.symbols.ValueType)(using C: Context): Boolean = {
    hasError = false
    unifyValueTypes(
      C.substitution.substitute(t1),
      C.substitution.substitute(t2),
      ErrorContext.MergeTypes(t1, t2))
    val hadError = hasError
    hasError = false
    hadError
  }

  override def requireLowerBound(x: TypeVar.UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()
  override def requireUpperBound(x: TypeVar.UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()
  override def requireEqual(x: TypeVar.UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()
  override def requireSubregion(lower: Captures, upper: Captures, ctx: ErrorContext): Unit = ()

  override def abort(msg: String, ctx: ErrorContext): Nothing = ??? // shouldn't ever be triggered [?]

  override def error(msg: String, ctx: ErrorContext): Unit = { hasError = true }
  override def error(left: Type, right: Type, ctx: ErrorContext): Unit = { hasError = true }
}

class BindSome(binder: source.Tree, capabilities: Map[symbols.InterfaceType, symbols.BlockParam],val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindSome(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.get(tpe).orElse {
      // If the requested type has unbound type parameters, try lexical resolution
      unknowns(tpe).toList match {
        case Nil => None
        case _ =>
          def isValidFit(handlerTpe: symbols.InterfaceType): Boolean =
            handlerTpe.typeConstructor == tpe.typeConstructor &&
              (tpe.args zip handlerTpe.args).forall { case (inferred, concrete) => DummyUnifier.checkSubtype(inferred, concrete) }

          capabilities.collect { case (handlerTpe, cap) if isValidFit(handlerTpe) => (handlerTpe, cap) }.toList match {
            case Nil => None
            case List((handlerTpe, cap)) =>
              (tpe.args zip handlerTpe.args).foreach { case (inferred, concrete) => C.requireSubtype(inferred, concrete, ErrorContext.MergeTypes(inferred, concrete)) }
              Some(cap)
            case _ => None // TODO: Could we have a better error message here?
          }
      }
    }.getOrElse(parent.capabilityFor(tpe))
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
