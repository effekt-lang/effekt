package effekt
package symbols

import effekt.context.Context
import scala.collection.mutable

/**
 * Something modeling a scope
 *
 * Scopes ares non-empty immutable linked lists storing mutable maps from symbols to T
 */
object scopes {

  sealed trait Scope {

    val terms: mutable.HashMap[String, Set[TermSymbol]] = mutable.HashMap.empty
    val types: mutable.HashMap[String, TypeSymbol] = mutable.HashMap.empty

    // for now, just get the first one
    def lookupTerm(key: String): Option[TermSymbol] = lookupTermHere(key)
    def lookupType(key: String): Option[TypeSymbol] = lookupTypeHere(key)

    def lookupTermHere(key: String): Option[TermSymbol] = terms.getOrElse(key, Set()).headOption
    def lookupTypeHere(key: String): Option[TypeSymbol] = types.get(key)

    // TODO add appropriate checks
    def define(key: String, sym: TermSymbol)(implicit C: Context): Unit = {
      val bindings = terms.getOrElse(key, Set())
      terms.update(key, bindings + sym)
    }

    def define(key: String, sym: TypeSymbol)(implicit C: Context): Unit =
      types.update(key, sym)

    def enter: Scope = BlockScope(this)

    def enterWith(tms: Map[String, Set[TermSymbol]], tps: Map[String, TypeSymbol]) = {
      val scope = BlockScope(this)
      scope.terms.addAll(tms)
      scope.types.addAll(tps)
      scope
    }

    def leave: Scope = sys error "Leaving top level scope"
  }
  //
  case class Toplevel() extends Scope

  case class BlockScope(parent: Scope) extends Scope {

    override def lookupTerm(key: String): Option[TermSymbol] = super.lookupTerm(key).orElse(parent.lookupTerm(key))
    override def lookupType(key: String): Option[TypeSymbol] = super.lookupType(key).orElse(parent.lookupType(key))

    override def leave: Scope = parent
  }

  def toplevel(types: Map[String, TypeSymbol]): Scope = {
    val scope = Toplevel()
    scope.types.addAll(types)
    scope
  }
}
