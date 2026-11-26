package effekt
package typer

import effekt.context.Context
import effekt.{ source, symbols }
import effekt.util.messages.{Candidate, CapabilityResolution, ErrorMessageReifier, Failure, INTERNAL_ERROR, Piece, Subdiagnostics, note}

/**
 * Capabilities are found like overloads: from the lexically innermost scope outwards, the first scope with capabilities
 * of the effect's interface that fit wins, and exactly one may fit there.
 *
 * A capability's type may contain unknowns: an unannotated function's fresh capability has the request's type as far as it is known. 
 * Trying a capability is undone ([[Context.tryCapability]]), so only committing solves unknowns:
 * unifying with the capability that fits, or with the only one that does not, to report why.
 * Type arguments must be known only where an interface occurs more than once in a set ([[assertConcreteEffect]]).
 */
sealed trait CapabilityScope {
  def copy: CapabilityScope
  def parent: CapabilityScope
  // the capabilities bound here
  def bound: List[symbols.BlockParam]

  // the scopes a lookup from here asks, innermost first
  // an unannotated function's scope is the last one
  private def reached: List[CapabilityScope] = this match {
    case GlobalCapabilityScope => Nil
    case s: BindAll => List(s)
    case s: BindSome => s :: s.parent.reached
  }

  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam = {
    val scopes = reached
    def ofInterface(s: CapabilityScope) =
      s.bound.filter { c => C.capabilityType(c).typeConstructor == tpe.typeConstructor }.map { c => (s, c) }
    def show(candidate: (CapabilityScope, symbols.BlockParam), failure: Option[Failure]) = candidate match {
      case (scope, capability) => scope.candidate(capability, failure)
    }

    resolveOverloaded(scopes.iterator.map(ofInterface)) { case (_, c) => C.tryCapability(tpe, c).toLeft(()) } match {
      case Outcome.Resolved((_, capability), _) => C.unifyCapability(tpe, capability); capability
      case Outcome.Ambiguous(fits) => C.abortWith(CapabilityResolution(tpe, fits.map { c => show(c, None) }))
      case Outcome.Undecided(_) => INTERNAL_ERROR("A capability is tried by one unification, which is never cut off.")
      case Outcome.Failed(tried) => scopes.lastOption match {
        case Some(s: BindAll) => s.fresh(tpe)
        case last =>
          // the effects a top-level definition declares are the context
          val context: Subdiagnostics ?=> Unit = last match { case Some(s: BindSome) => s.declaration; case _ => () }
          tried match {
            case Nil => C.abort(pretty"Effect ${tpe} is not allowed in this context.")(context)
            // the only one: unified with for real, so its mismatch is reported and checking goes on
            case List(((_, capability), _)) => C.unifyCapability(tpe, capability); capability
            case several => C.abortWith(CapabilityResolution(tpe, several.map { (c, failure) => show(c, Some(failure)) }))(context)
          }
      }
    }
  }

  // how a capability bound here is shown, and where
  protected def candidate(capability: symbols.BlockParam, failure: Option[Failure])(using C: Context): Candidate =
    Candidate(pretty"${C.capabilityType(capability)}", None, failure)
}
case object GlobalCapabilityScope extends CapabilityScope {
  def copy: CapabilityScope = this
  def parent: CapabilityScope = sys error "No parent"
  def bound: List[symbols.BlockParam] = Nil
}

class BindSome(binder: source.Tree, capabilities: List[symbols.BlockParam], val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindSome(binder, capabilities, parent.copy)
  def bound: List[symbols.BlockParam] = capabilities

  private[typer] def declaration(using C: Context, S: Subdiagnostics): Unit = binder match {
    case f: source.FunDef if C.isMain(f.symbol) =>
      note(pretty"${Piece.Code("main")} cannot have effects.", Some(f.id.span))
    case f: source.FunDef =>
      val declared = f.symbol.asInstanceOf[symbols.Callable].annotatedEffects.getOrElse(symbols.Effects(Nil))
      // where the effects are written, else the name (a result type alone declares no effects)
      val at = f.ret.unspan.map(_.eff.span).filter(_.origin == source.Origin.Real).getOrElse(f.id.span)
      if (declared.effects.isEmpty) note(pretty"${Piece.Code(f.id.name)} declares no effects.", Some(at))
      else note(pretty"${Piece.Code(f.id.name)} declares only the effects ${declared}.", Some(at))
    case _ => ()
  }

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
  def bound: List[symbols.BlockParam] = capabilities

  private[typer] def fresh(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam = {
    val capability = C.freshCapabilityFor(tpe)
    capabilities = capabilities :+ capability
    capability
  }
  override def toString: String = s"BindAll(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}
