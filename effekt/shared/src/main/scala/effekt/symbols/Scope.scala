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

    def lookupOverloaded(key: String)(implicit C: Context): List[Set[TermSymbol]]

    // can be a term OR a type symbol
    def lookupFirst(key: String)(implicit C: Context): Symbol

    def currentTermsFor(key: String): Set[TermSymbol] =
      terms.getOrElse(key, Set.empty)

    // TODO add appropriate checks
    def define(key: String, sym: TermSymbol)(implicit C: Context): Unit = {
      val bindings = terms.getOrElse(key, Set())
      terms.update(key, bindings + sym)
    }

    def define(key: String, sym: TypeSymbol)(implicit C: Context): Unit =
      types.update(key, sym)

    def enterLocal: Scope = LocalScope(this)
    // TODO rename global to "static" scope
    def enterGlobal(implicit C: Context): Scope = GlobalScope(this)

    def defineAll(tms: Map[String, Set[TermSymbol]], tps: Map[String, TypeSymbol])(implicit C: Context) = {
      tms.foreach { case (n, syms) => syms.foreach { sym => define(n, sym) } }
      tps.foreach { case (n, sym) => define(n, sym) }
    }

    def enterGlobalWith(tms: Map[String, Set[TermSymbol]], tps: Map[String, TypeSymbol])(implicit C: Context) = {
      val scope = GlobalScope(this)
      scope.defineAll(tms, tps)
      scope
    }

    def leave(implicit C: Context): Scope

    def isGlobal: Boolean = true
  }

  case class EmptyScope() extends Scope {
    def lookupFirstTerm(key: String)(implicit C: Context): TermSymbol =
      C.abort(s"Could not resolve term ${key}")

    def lookupType(key: String)(implicit C: Context): TypeSymbol =
      C.abort(s"Could not resolve type ${key}")

    def lookupFirst(key: String)(implicit C: Context): Symbol =
      C.abort(s"Could not resolve ${key}")

    // returns a list of sets to model the scopes. This way we can decide in Typer how to deal with
    // the ambiguity. If it is nested, the first one that type checks should be chosen.
    def lookupOverloaded(key: String)(implicit C: Context): List[Set[TermSymbol]] =
      Nil

    def leave(implicit C: Context): Scope =
      C.abort("Internal Compiler Error: Leaving top level scope")
  }

  trait BlockScope extends Scope {

    def parent: Scope

    def lookupFirstTerm(key: String)(implicit C: Context): TermSymbol =
      terms.get(key).map { bindings =>
        if (bindings.size > 1)
          C.abort(s"Ambiguous reference to ${key}")
        else
          bindings.head
      }.getOrElse { parent.lookupFirstTerm(key) }

    def lookupFirst(key: String)(implicit C: Context): Symbol =
      (terms.get(key).map(_.toList), types.get(key)) match {
        case (Some(List(t)), None) => t
        case (None, Some(t)) => t
        // give precendence to the type level effect, if an equally named effect op is in scope
        case (Some(List(t1: EffectOp)), Some(t2: UserEffect)) => t2
        case (Some(t1), Some(t2)) =>
          C.abort(s"Ambiguous reference to ${key}. Can refer to a term or a type.")
        case (None, None) => parent.lookupFirst(key)
        case _            => C.abort(s"Ambiguous reference to ${key}.")
      }

    def lookupType(key: String)(implicit C: Context): TypeSymbol =
      types.getOrElse(key, parent.lookupType(key))

    def lookupOverloaded(key: String)(implicit C: Context): List[Set[TermSymbol]] =
      terms.get(key).map { _ :: parent.lookupOverloaded(key) }.getOrElse {
        parent.lookupOverloaded(key)
      }

    def leave(implicit C: Context): Scope =
      parent

    override def toString = s"BlockScope(${terms.keySet.mkString(", ")}) :: $parent"
  }

  case class LocalScope(parent: Scope) extends BlockScope {
    override def enterGlobal(implicit C: Context): Scope =
      C.abort("Cannot open a global scope inside a local scope")

    override def isGlobal: Boolean = false
  }

  // A global namespace
  case class GlobalScope(parent: Scope) extends BlockScope

  def toplevel(types: Map[String, TypeSymbol])(implicit C: Context): Scope =
    EmptyScope().enterGlobalWith(Map.empty, types)
}
