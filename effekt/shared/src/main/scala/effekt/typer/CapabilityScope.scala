package effekt
package typer

import effekt.context.Context
import effekt.symbols.{BlockType, Captures, Type, TypeVar, ValueType}
import effekt.{source, symbols}
import effekt.util.messages.ErrorMessageReifier

/**
 * Invariant: Like the result effects of Typer, all types of bound capabilities need to be concrete!
 */
sealed trait CapabilityScope {
  def copy: CapabilityScope
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam
  def parent: CapabilityScope
  def relevantInScopeFor(tpe: symbols.InterfaceType)(using C: Context): Set[symbols.InterfaceType]
}
case object GlobalCapabilityScope extends CapabilityScope {
  def copy: CapabilityScope = this
  def parent: CapabilityScope = sys error "No parent"
  // If we try to find a capability for an effect that is known to be unhandled (that is no outer scope could
  // potentially handle it, then we raise an error.
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    C.abort(pretty"Effect ${tpe} is not allowed in this context.")
  def relevantInScopeFor(tpe: BlockType.InterfaceType)(using C: Context): Set[BlockType.InterfaceType] = Set.empty
}

object DummyUnifier extends TypeUnifier {
  private case object UnificationFailed extends Exception with scala.util.control.NoStackTrace

  def checkSubtype(t1: effekt.symbols.ValueType, t2: effekt.symbols.ValueType)(using C: Context): Boolean = try {
    unifyValueTypes(
      C.substitution.substitute(t1),
      C.substitution.substitute(t2),
      ErrorContext.MergeTypes(t1, t2))
    true
  } catch {
    case UnificationFailed => false
  }

  override def requireLowerBound(x: TypeVar.UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()
  override def requireUpperBound(x: TypeVar.UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()
  override def requireEqual(x: TypeVar.UnificationVar, tpe: ValueType, ctx: ErrorContext): Unit = ()
  override def requireSubregion(lower: Captures, upper: Captures, ctx: ErrorContext): Unit = ()

  // NOTE(jiribenes, 2025-11-12): I think the 'abort' should never be triggered here.
  override def abort(msg: String, ctx: ErrorContext): Nothing = sys error "Unexpected abort in DummyUnifier!"

  override def error(msg: String, ctx: ErrorContext): Unit = throw UnificationFailed
  override def error(left: Type, right: Type, ctx: ErrorContext): Unit = throw UnificationFailed
}

class BindSome(binder: source.Tree, capabilities: Map[symbols.InterfaceType, symbols.BlockParam],val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindSome(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.get(tpe).orElse {
      tryLexicalCapabilityFor(tpe)
    }.getOrElse(parent.capabilityFor(tpe))

  // If the requested type has unbound type parameters, try lexical resolution (see PR #1194)
  private def tryLexicalCapabilityFor(tpe: symbols.InterfaceType)(using C: Context): Option[symbols.BlockParam] = {
    if (unknowns(tpe).isEmpty) return None

    // Is `handlerTpe` a valid concrete interface for the `tpe` at hand?
    def isCompatible(handlerTpe: symbols.InterfaceType): Boolean =
      tpe.typeConstructor == handlerTpe.typeConstructor &&
        (tpe.args zip handlerTpe.args).forall { case (inferred, concrete) =>
          DummyUnifier.checkSubtype(inferred, concrete)
        }

    val compatible = capabilities.filter { case (handlerTpe, cap) => isCompatible(handlerTpe) }.toList
    compatible match {
      case List((handlerTpe, cap)) =>
        (tpe.args zip handlerTpe.args).foreach { case (inferred, concrete) =>
          // Enforce subtype constraints for the type params of the chosen handler
          C.requireSubtype(inferred, concrete, ErrorContext.MergeTypes(inferred, concrete))
        }
        Some(cap)
      case _ => None // no compatible match or multiple (ambiguous)
    }
  }


  def relevantInScopeFor(tpe: BlockType.InterfaceType)(using C: Context): Set[BlockType.InterfaceType] =
    capabilities.collect { case (handlerTpe, _) if handlerTpe.typeConstructor == tpe.typeConstructor && unknowns(handlerTpe).isEmpty => handlerTpe }.toSet ++ parent.relevantInScopeFor(tpe)
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
  def relevantInScopeFor(tpe: BlockType.InterfaceType)(using C: Context): Set[BlockType.InterfaceType] =
    capabilities.collect { case (handlerTpe, _) if handlerTpe.typeConstructor == tpe.typeConstructor && unknowns(handlerTpe).isEmpty => handlerTpe }.toSet ++ parent.relevantInScopeFor(tpe)
  override def toString: String = s"BindAll(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}
