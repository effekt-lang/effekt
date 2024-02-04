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
  def importAll(other: Bindings): Unit = other match {
    case Bindings(terms2, types2, captures2, namespaces2) =>
      terms2.foreach { case (k, syms) => syms.foreach(addTerm(k, _)) }
      types2.foreach { case (k, v) => setType(k, v) }
      captures2.foreach { case (k, v) => setCapture(k, v) }
      namespaces2.foreach { case (k, v) => getNamespace(k).importAll(v) }
  }

  def addTerm(name: String, sym: TermSymbol): Unit =
    val before = terms.getOrElse(name, Set.empty)
    terms.update(name, before + sym)
  def setType(name: String, sym: TypeSymbol): Unit =
    types.update(name, sym)
  def setCapture(name: String, sym: Capture): Unit =
    captures.update(name, sym)

  def getNamespace(name: String): Namespace =
    namespaces.getOrElseUpdate(name, Namespace.empty)

  /**
   * Convert to immutable bindings
   */
  def toBindings: Bindings = Bindings(
    terms.toMap,
    types.toMap,
    captures.toMap,
    namespaces.map { case (k, v) => k -> v.toBindings }.toMap)
}
object Namespace {
  def empty: Namespace = Namespace(mutable.Map.empty, mutable.Map.empty, mutable.Map.empty, mutable.Map.empty)
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

    def imports(using E: ErrorReporter): Namespace = this match {
      case s: Scope.Named => E.abort("Can only import at the top of a file or function definition.")
      case s @ Scope.Global(imports, bindings) => imports
      case s @ Scope.Local(imports, bindings, outer) => imports
    }
  }

  case class Scoping(modulePath: List[String], var scope: Scope) {
    def importAs(imports: Bindings, path: List[String])(using E: ErrorReporter): Unit =
      def go(path: List[String], in: Namespace): Unit = path match {
        case pathSeg :: rest => go(rest, in.getNamespace(pathSeg))
        case Nil => in.importAll(imports)
      }
      go(path, scope.imports)

    // TODO check shadowing etc. Also here concrete functions will *always* shadow imports,
    //   regardless of the order of importing / defining.
    def importAll(imports: Bindings)(using E: ErrorReporter): Unit = scope.imports.importAll(imports)

    /**
     * Defines the scoping rules by searching with [[ search ]] in an
     * inside-out manner through all nested scopes.
     */
    private def first[T](path: List[String], scope: Scope)(select: Namespace => Option[T]): Option[T] =

      def qualified(path: List[String], bindings: Namespace): Option[T] = path match {
        case Nil => select(bindings)
        case pathSegment :: rest => bindings.namespaces.get(pathSegment).flatMap {
          namespace => qualified(rest, namespace)
        }
      }

      scope match {
        case Scope.Global(imports, bindings) =>
          qualified(path, bindings) orElse qualified(path, imports)
        case Scope.Named(name, bindings, outer) =>
          qualified(path, bindings) orElse first(path, outer)(select)
        case Scope.Local(imports, bindings, outer) =>
          qualified(path, bindings) orElse qualified(path, imports) orElse first(path, outer)(select)
      }

    private def all[T](path: List[String], scope: Scope)(select: Namespace => T): List[T] =

      def qualified(path: List[String], bindings: Namespace): List[T] = path match {
        case Nil => select(bindings) :: Nil
        case pathSegment :: rest => bindings.namespaces.get(pathSegment).toList.flatMap {
          namespace => qualified(rest, namespace)
        }
      }

      scope match {
        case Scope.Global(imports, bindings) =>
          qualified(path, bindings) ++ qualified(path, imports)
        case Scope.Named(name, bindings, outer) =>
          qualified(path, bindings) ++ all(path, outer)(select)
        case Scope.Local(imports, bindings, outer) =>
          qualified(path, bindings) ++ qualified(path, imports) ++ all(path, outer)(select)
      }

    /**
     * Searches the nested scopes to find the first term.
     * Fails if:
     *   - there are multiple matching terms in the same scope
     *   - there a no matching terms at all
     */
    def lookupFirstTerm(id: IdRef)(using E: ErrorReporter): TermSymbol =
      lookupFirstTermOption(id) getOrElse { E.abort(pp"Could not resolve term ${id}") }

    def lookupFirstTermOption(id: IdRef)(using E: ErrorReporter): Option[TermSymbol] =
      first(id.path, scope) { _.terms.get(id.name).map { syms =>
        if (syms.size > 1) E.abort(pp"Ambiguous reference to ${id}")
        else syms.head
      }}

    def lookupType(id: IdRef)(using E: ErrorReporter): TypeSymbol =
      lookupTypeOption(id.path, id.name) getOrElse { E.abort(pp"Could not resolve type ${id}") }

    def lookupTypeOption(path: List[String], name: String)(using E: ErrorReporter): Option[TypeSymbol] =
      first(path, scope) { _.types.get(name) }

    def lookupCapture(id: IdRef)(using E: ErrorReporter): Capture =
      first(id.path, scope) { _.captures.get(id.name) } getOrElse E.abort(pp"Could not resolve capture ${id}")

    def lookupOverloaded(id: IdRef, filter: TermSymbol => Boolean)(using ErrorReporter): List[Set[TermSymbol]] =
      all(id.path, scope) { _.terms.getOrElse(id.name, Set.empty).filter(filter) }

    def lookupOperation(path: List[String], name: String)(using ErrorReporter): List[Set[Operation]] =
      all(path, scope) { _.terms.getOrElse(name, Set.empty).collect {
        case o: Operation => o
      }}

    // can be a term OR a type symbol
    def lookupFirst(path: List[String], name: String)(using E: ErrorReporter): Symbol =
      lookupFirstOption(path, name) getOrElse { E.abort(s"Could not resolve ${name}") }

    def lookupFirstOption(path: List[String], name: String)(using E: ErrorReporter): Option[Symbol] =
      first(path, scope) { bindings =>
          (bindings.terms.get(name).map(_.toList), bindings.types.get(name)) match {
            case (Some(List(t)), None) => Some(t)
            case (None, Some(t)) => Some(t)
            // give precedence to the type level effect, if an equally named effect op is in scope
            case (Some(List(t1: Operation)), Some(t2: Interface)) => Some(t2)
            case (Some(t1), Some(t2)) =>
              E.abort(s"Ambiguous reference to ${name}. Can refer to a term or a type.")
            case (None, None) => None
            case _            => E.abort(s"Ambiguous reference to ${name}.")
          }
      }

    def currentTermsFor(name: String): Set[TermSymbol] =
      scope.bindings.terms.getOrElse(name, Set.empty)

    def allTermsFor(path: List[String], name: String): Set[TermSymbol] =
      all(path, scope) { _.terms.getOrElse(name, Set.empty) }.flatten.toSet

    def define(name: String, sym: TermSymbol)(using E: ErrorReporter): Unit = {
      val bindings = scope.bindings
      val termsInScope = currentTermsFor(name)
      sym match {
        case v: ValueSymbol =>
          if (termsInScope.exists(_.isInstanceOf[BlockSymbol])) {
            E.abort(s"Value ${name} has the same name as a block definition in the same scope, which is not allowed.")
          }
        case b: BlockSymbol =>
          if (termsInScope.exists(_.isInstanceOf[ValueSymbol])) {
            E.abort(s"Block ${name} has the same name as a value definition in the same scope, which is not allowed.")
          }
      }
      bindings.addTerm(name, sym)
    }

    def define(name: String, sym: TypeSymbol)(using E: ErrorReporter): Unit =
      val bindings = scope.bindings
      lookupTypeOption(Nil, name).foreach { shadowed =>
        if sym.isInstanceOf[TypeVar] && !shadowed.isInstanceOf[TypeVar] then
          E.warning(pp"Type parameter ${name} shadows outer definition of ${sym}")
      }
      bindings.setType(name, sym)

    def define(name: String, capt: Capture)(using ErrorReporter): Unit =
      scope.bindings.setCapture(name, capt)

    def exports: Bindings = scope.bindings.toBindings

    def scoped[R](block: => R): R =
      val before = scope
      scope = Scope.Local(Namespace.empty, Namespace.empty, before)
      try { block } finally { scope = before }

    // (re-)enter the namespace
    def namespace[R](name: String)(block: => R): R =
      val before = scope
      val namespace = before.bindings.getNamespace(name)
      scope = Scope.Named(name, namespace, before)
      try { block } finally { scope = before }

    // returns the current path
    def path: Option[List[String]] =
      def collect(scope: Scope): Option[List[String]] = scope match {
        case Scope.Global(imports, bindings) => Some(modulePath)
        case Scope.Named(name, bindings, outer) => collect(outer) match {
          case Some(path) => Some(path :+ name)
          // name spaces also START a new path, if there hasn't been one, already
          case None => Some(List(name))
        }
        case Scope.Local(imports, bindings, outer) => None
      }
      collect(scope)
  }

  def toplevel(modulePath: List[String], prelude: Bindings): Scoping =
    val imports = Namespace.empty
    imports.importAll(prelude)
    Scoping(modulePath, Scope.Global(imports, Namespace.empty))
}
