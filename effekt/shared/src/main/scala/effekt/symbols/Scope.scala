package effekt
package symbols

import effekt.source.IdRef
import effekt.util.messages.ErrorReporter

import scala.collection.mutable

/**
 * An immutable container of bindings
 */
case class Bindings(
  terms: Map[String, Set[TermSymbol]], // terms can be overloaded
  types: Map[String, TypeSymbol],
  captures: Map[String, Capture],
  namespaces: Map[String, Bindings]
)

object Bindings {
  def empty: Bindings = Bindings(Map.empty, Map.empty, Map.empty, Map.empty)
}

/**
 * A mutable container of bindings
 */
class Namespace(
  var terms: mutable.Map[String, Set[TermSymbol]],
  var types: mutable.Map[String, TypeSymbol],
  var captures: mutable.Map[String, Capture],
  var namespaces: mutable.Map[String, Namespace]
) {
  def importAll(other: Bindings): Unit = ()

  def addTerm(name: String, sym: TermSymbol): Unit = ()
  def setType(name: String, sym: TypeSymbol): Unit = ()
  def setCapture(name: String, sym: Capture): Unit = ()

  def getNamespace(name: String): Namespace = ???

  /**
   * Convert to immutable bindings
   */
  def toBindings: Bindings = ???
}
object Namespace {
  def empty: Namespace = ???
}

object scopes {

  enum Scope { self =>

    /**
     * The toplevel global scope ("project scope")
     */
    case Global(imports: Namespace, bindings: Namespace)

    /**
     * A scope introducing a new namespace
     */
    case Named(name: String, bindings: Namespace, outer: Scope)

    /**
     * A local scope introduced by functions, blocks, etc.
     */
    case Local(imports: Namespace, bindings: Namespace, outer: Scope)

    /**
     * All scopes introduce (mutable) namespaces for their bindings
     */
    def bindings: Namespace

    def imports(using E: ErrorReporter): Namespace = ???
  }

  case class Scoping(modulePath: List[String], var scope: Scope) {
    def importAs(imports: Bindings, path: List[String])(using E: ErrorReporter): Unit = ()

    // TODO check shadowing etc. Also here concrete functions will *always* shadow imports,
    //   regardless of the order of importing / defining.
    def importAll(imports: Bindings)(using E: ErrorReporter): Unit =  ???

    /**
     * Defines the scoping rules by searching with [[ search ]] in an
     * inside-out manner through all nested scopes.
     */
    private def first[T](path: List[String], scope: Scope)(select: Namespace => Option[T]): Option[T] = None

    private def all[T](path: List[String], scope: Scope)(select: Namespace => T): List[T] = Nil

    /**
     * Searches the nested scopes to find the first term.
     * Fails if:
     *   - there are multiple matching terms in the same scope
     *   - there a no matching terms at all
     */
    def lookupFirstTerm(id: IdRef)(using E: ErrorReporter): TermSymbol = ???

    def lookupFirstTermOption(id: IdRef)(using E: ErrorReporter): Option[TermSymbol] = ???

    def lookupType(id: IdRef)(using E: ErrorReporter): TypeSymbol = ???

    def lookupTypeOption(path: List[String], name: String)(using E: ErrorReporter): Option[TypeSymbol] = ???

    def lookupCapture(id: IdRef)(using E: ErrorReporter): Capture = ???

    def lookupOverloaded(id: IdRef, filter: TermSymbol => Boolean)(using ErrorReporter): List[Set[TermSymbol]] = Nil

    def lookupOperation(path: List[String], name: String)(using ErrorReporter): List[Set[Operation]] = ???

    // can be a term OR a type symbol
    def lookupFirst(path: List[String], name: String)(using E: ErrorReporter): Symbol = ???

    def lookupFirstOption(path: List[String], name: String)(using E: ErrorReporter): Option[Symbol] = ???

    def currentTermsFor(name: String): Set[TermSymbol] = ???

    def allTermsFor(path: List[String], name: String): Set[TermSymbol] = ???

    def define(name: String, sym: TermSymbol)(using E: ErrorReporter): Unit = ()

    def define(name: String, sym: TypeSymbol)(using E: ErrorReporter): Unit = ()

    def define(name: String, capt: Capture)(using ErrorReporter): Unit = ()

    def exports: Bindings = scope.bindings.toBindings

    def scoped[R](block: => R): R = ???

    // (re-)enter the namespace
    def namespace[R](name: String)(block: => R): R = ???

    // returns the current path
    def path: Option[List[String]] = ???
  }

  def toplevel(modulePath: List[String], prelude: Bindings): Scoping = ???
}
