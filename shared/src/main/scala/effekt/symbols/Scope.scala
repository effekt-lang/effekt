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

    /**
     * Searches the nested scopes to find the first term.
     * Fails if:
     *   - there are multiple matching terms in the same scope
     *   - there a no matching terms at all
     */
    def lookupFirstTerm(key: String)(implicit C: Context): TermSymbol

    def lookupType(key: String)(implicit C: Context): TypeSymbol

    def lookupTerms(key: String)(implicit C: Context): Set[TermSymbol]

    def currentTermsFor(key: String): Set[TermSymbol] = terms.getOrElse(key, Set.empty)

    // TODO add appropriate checks
    def define(key: String, sym: TermSymbol)(implicit C: Context): Unit = {
      val bindings = terms.getOrElse(key, Set())
      terms.update(key, bindings + sym)
    }

    def define(key: String, sym: TypeSymbol)(implicit C: Context): Unit =
      types.update(key, sym)

    def enter: Scope = BlockScope(this)

    def defineAll(tms: Map[String, Set[TermSymbol]], tps: Map[String, TypeSymbol])(implicit C: Context) = {
      tms.foreach { case (n, syms) => syms.foreach { sym => define(n, sym) } }
      tps.foreach { case (n, sym) => define(n, sym) }
    }

    def enterWith(tms: Map[String, Set[TermSymbol]], tps: Map[String, TypeSymbol])(implicit C: Context) = {
      val scope = BlockScope(this)
      scope.defineAll(tms, tps)
      scope
    }

    def leave(implicit C: Context): Scope
  }

  case class EmptyScope() extends Scope {
    def lookupFirstTerm(key: String)(implicit C: Context): TermSymbol =
      C.abort(s"Could not resolve term ${key}")

    def lookupType(key: String)(implicit C: Context): TypeSymbol =
      C.abort(s"Could not resolve type ${key}")

    def lookupTerms(key: String)(implicit C: Context): Set[TermSymbol] =
      Set.empty

    def leave(implicit C: Context): Scope =
      C.abort("Internal Compiler Error: Leaving top level scope")
  }

  case class BlockScope(parent: Scope) extends Scope {

    def lookupFirstTerm(key: String)(implicit C: Context): TermSymbol =
      terms.get(key).map { bindings =>
        if (bindings.size > 1)
          C.abort(s"Ambiguous reference to ${key}")
        else
          bindings.head
      }.getOrElse { parent.lookupFirstTerm(key) }

    def lookupType(key: String)(implicit C: Context): TypeSymbol =
      types.getOrElse(key, parent.lookupType(key))

    def lookupTerms(key: String)(implicit C: Context): Set[TermSymbol] = {
      val currentTerms = terms.getOrElse(key, Set.empty)
      if (currentTerms.isEmpty) { parent.lookupTerms(key) } else { currentTerms }
    }

    def leave(implicit C: Context): Scope =
      parent
  }

  def toplevel(types: Map[String, TypeSymbol])(implicit C: Context): Scope =
    EmptyScope().enterWith(Map.empty, types)
}
