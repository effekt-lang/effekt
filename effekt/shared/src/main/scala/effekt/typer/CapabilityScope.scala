package effekt
package typer

import effekt.context.Context
import effekt.{ source, symbols }
import effekt.util.messages.{Candidate, CapabilityResolution, ErrorMessageReifier, Failure, Piece}

/**
 * Capabilities are found like overloads: from the lexically innermost scope outwards, the first scope with capabilities
 * of the effect's interface that fit wins, and exactly one may fit there.
 */
sealed trait CapabilityScope {
  def copy: CapabilityScope
  def parent: CapabilityScope
  // `passed`: the capabilities of the interface in inner scopes that did not fit, with why
  def capabilityFor(tpe: symbols.InterfaceType, passed: List[(symbols.BlockParam, Candidate)])(using C: Context): symbols.BlockParam

  // how a capability bound here is shown, and where
  protected def candidate(capability: symbols.BlockParam, failure: Option[Failure])(using C: Context): Candidate =
    Candidate(pretty"${C.capabilityType(capability)}", None, failure)

  // among the capabilities bound here, the one for `tpe` that fits; if none fits, `orElse` the ones that did not, with a reason
  protected def choose(bound: List[symbols.BlockParam], tpe: symbols.InterfaceType)(orElse: List[(symbols.BlockParam, Candidate)] => symbols.BlockParam)(using C: Context): symbols.BlockParam = {
    val tried = bound.filter { c => C.capabilityType(c).typeConstructor == tpe.typeConstructor }.map { c => (c, C.tryCapability(tpe, c)) }
    tried.collect { case (c, None) => c } match {
      case List(capability) => C.unifyCapability(tpe, capability); capability
      case Nil => orElse(tried.collect { case (c, Some(failure)) => (c, candidate(c, Some(failure))) })
      case several => C.abortWith(CapabilityResolution(tpe, several.map { c => candidate(c, None) }))
    }
  }

  // no capability for `tpe` fits anywhere
  protected def notAllowed(tpe: symbols.InterfaceType, passed: List[(symbols.BlockParam, Candidate)])(using C: Context): symbols.BlockParam = passed match {
    case Nil => C.abort(pretty"Effect ${tpe} is not allowed in this context.")
    // the only one: why it does not fit
    case List((capability, _)) => C.unifyCapability(tpe, capability); capability
    case several => C.abortWith(CapabilityResolution(tpe, several.map(_._2)))
  }
}
case object GlobalCapabilityScope extends CapabilityScope {
  def copy: CapabilityScope = this
  def parent: CapabilityScope = sys error "No parent"
  def capabilityFor(tpe: symbols.InterfaceType, passed: List[(symbols.BlockParam, Candidate)])(using C: Context): symbols.BlockParam =
    notAllowed(tpe, passed)
}

class BindSome(binder: source.Tree, capabilities: List[symbols.BlockParam], val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindSome(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType, passed: List[(symbols.BlockParam, Candidate)])(using C: Context): symbols.BlockParam =
    choose(capabilities, tpe) { notFitting => parent.capabilityFor(tpe, passed ++ notFitting) }

  // a handler of a `try` (its capabilities are in the order of its handlers), or the effects a definition declares
  override protected def candidate(capability: symbols.BlockParam, failure: Option[Failure])(using C: Context): Candidate = binder match {
    case source.TryHandle(_, handlers, _) =>
      // the bound name and type if named, else the effect's type
      val place = handlers.lift(capabilities.indexOf(capability)).map { h => h.capability.map(_.span).getOrElse(h.impl.interface.span) }
      Candidate(pretty"${C.capabilityType(capability)}", place, failure)
    case f: source.FunDef =>
      Candidate(pretty"the effect ${C.capabilityType(capability)} of ${Piece.Code(f.id.name)}", Some(f.id.span), failure)
    case _ =>
      Candidate(pretty"the effect ${C.capabilityType(capability)}", Some(binder.span), failure)
  }
  override def toString: String = s"BindSome(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}

// the requirements of a function without annotated effects: a capability that fits is reused, else a fresh one
class BindAll(binder: source.Tree, var capabilities: List[symbols.BlockParam], val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindAll(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType, passed: List[(symbols.BlockParam, Candidate)])(using C: Context): symbols.BlockParam =
    choose(capabilities, tpe) { _ =>
      val fresh = C.freshCapabilityFor(tpe)
      capabilities = capabilities :+ fresh
      fresh
    }
  override def toString: String = s"BindAll(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}
