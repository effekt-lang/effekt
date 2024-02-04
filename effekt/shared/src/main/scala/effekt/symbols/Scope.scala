package effekt
package symbols

import effekt.util.messages.ErrorReporter

case class Bindings(
  terms: Map[String, Set[TermSymbol]], // terms can be overloaded
  types: Map[String, TypeSymbol],
  captures: Map[String, Capture],
  namespaces: Map[String, Bindings]
)
object Bindings {
  def empty: Bindings = Bindings(Map.empty, Map.empty, Map.empty, Map.empty)
}

object scopes {

  enum Scope { self =>

    /**
     * The toplevel global scope ("project scope")
     */
    case Global(var imports: Bindings, var bindings: Bindings)

    /**
     * A scope introducing a new namespace
     */
    case Named(name: String, var bindings: Bindings, outer: Scope)

    /**
     * A local scope introduced by functions, blocks, etc.
     */
    case Local(var imports: Bindings, var bindings: Bindings, outer: Scope)

    /**
     * All scopes introduce bindings
     */
    def bindings: Bindings
    def bindings_=(b: Bindings): Unit
  }

  case class Scoping(modulePath: List[String], var scope: Scope) {
    def importAs(imports: Bindings, path: List[String])(using E: ErrorReporter): Unit =
      def go(path: List[String]): Bindings = path match {
        case pathSeg :: rest => Bindings(Map.empty, Map.empty, Map.empty, Map(pathSeg -> go(rest)))
        case Nil => imports
      }
      importAll(go(path))


    def importAll(imports: Bindings)(using E: ErrorReporter): Unit = scope match {
      case s: Scope.Named => E.abort("Can only import at the top of a file or function definition.")

      // TODO check shadowing etc. Also here concrete functions will *always* shadow imports,
      //   regardless of the order of importing / defining.
      case s @ Scope.Global(oldImports, bindings) =>
         s.imports = merge(oldImports, imports)
      case s @ Scope.Local(oldImports, bindings, outer) =>
         s.imports = merge(oldImports, imports)
    }


    /**
     * Defines the scoping rules by searching with [[ search ]] in an
     * inside-out manner through all nested scopes.
     */
    private def first[T](path: List[String], scope: Scope)(select: Bindings => Option[T]): Option[T] =

      def qualified(path: List[String], bindings: Bindings): Option[T] = path match {
        case Nil => select(bindings)
        case pathSegment :: rest => bindings.namespaces.get(pathSegment).flatMap(qualified(rest, _))
      }

      scope match {
        case Scope.Global(imports, bindings) =>
          qualified(path, bindings) orElse qualified(path, imports)
        case Scope.Named(name, bindings, outer) =>
          qualified(path, bindings) orElse first(path, outer)(select)
        case Scope.Local(imports, bindings, outer) =>
          qualified(path, bindings) orElse qualified(path, imports) orElse first(path, outer)(select)
      }

    private def all[T](path: List[String], scope: Scope)(select: Bindings => T): List[T] =

      def qualified(path: List[String], bindings: Bindings): List[T] = path match {
        case Nil => select(bindings) :: Nil
        case pathSegment :: rest =>
          bindings.namespaces.get(pathSegment).toList.flatMap(qualified(rest, _))
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
    def lookupFirstTerm(path: List[String], name: String)(using E: ErrorReporter): TermSymbol =
      lookupFirstTermOption(path, name) getOrElse { E.abort(s"Could not resolve term ${name}") }

    def lookupFirstTermOption(path: List[String], name: String)(using E: ErrorReporter): Option[TermSymbol] =
      first(path, scope) { _.terms.get(name).map { syms =>
        if (syms.size > 1) E.abort(s"Ambiguous reference to ${name}")
        else syms.head
      }}

    def lookupType(path: List[String], name: String)(using E: ErrorReporter): TypeSymbol =
      lookupTypeOption(path, name) getOrElse { E.abort(s"Could not resolve type ${name}") }

    def lookupTypeOption(path: List[String], name: String)(using E: ErrorReporter): Option[TypeSymbol] =
      first(path, scope) { _.types.get(name) }

    def lookupCapture(path: List[String], name: String)(using E: ErrorReporter): Capture =
      first(path, scope) { _.captures.get(name) } getOrElse E.abort(s"Could not resolve capture ${name}")

    def lookupOverloaded(path: List[String], name: String, filter: TermSymbol => Boolean)(using ErrorReporter): List[Set[TermSymbol]] =
      all(path, scope) { _.terms.getOrElse(name, Set.empty).filter(filter) }

    def lookupOperation(path: List[String], name: String)(using ErrorReporter): List[Set[Operation]] =
      all(path, scope) { _.terms.getOrElse(name, Set.empty).collect {
        case o: Operation => o
      }}

    // can be a term OR a type symbol
    def lookupFirst(path: List[String], name: String)(using E: ErrorReporter): Symbol =
      lookupFirstOption(path, name) getOrElse { E.abort(s"Could not resolve ${name}") }

    def lookupFirstOption(path: List[String], name: String)(using E: ErrorReporter): Option[Symbol] =
      first(path, scope) {
        case Bindings(terms, types, captures, namespaces) =>
          (terms.get(name).map(_.toList), types.get(name)) match {
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
      scope.bindings = bindings.copy(terms = bindings.terms.updated(name, termsInScope + sym))
    }

    def define(name: String, sym: TypeSymbol)(using E: ErrorReporter): Unit =
      val bindings = scope.bindings
      lookupTypeOption(Nil, name).foreach { shadowed =>
        if sym.isInstanceOf[TypeVar] && !shadowed.isInstanceOf[TypeVar] then
          E.warning(pp"Type parameter ${name} shadows outer definition of ${sym}")
      }
      scope.bindings = bindings.copy(types = bindings.types.updated(name, sym))

    def define(name: String, capt: Capture)(using ErrorReporter): Unit =
      scope.bindings = scope.bindings.copy(captures = scope.bindings.captures.updated(name, capt))

    // TODO implement proper checks and merging behavior
    def merge(oldNamespaces: Map[String, Bindings], newNewspaces: Map[String, Bindings]): Map[String, Bindings] =
      var bindings = oldNamespaces
      newNewspaces.foreach { case (k, v) => bindings = bindings.updated(k, merge(bindings.getOrElse(k, Bindings.empty), v)) }
      bindings

    // TODO implement proper checks and merging behavior
    def merge(oldBindings: Bindings, newBindings: Bindings): Bindings = (oldBindings, newBindings) match {
      case (Bindings(terms1, types1, captures1, namespaces1), Bindings(terms2, types2, captures2, namespaces2)) =>
        var terms = terms1
        terms2.foreach { case (k, v) => terms = terms.updated(k, terms.getOrElse(k, Set()) ++ v) }
        Bindings(terms, types1 ++ types2, captures1 ++ captures2, merge(namespaces1, namespaces2))
    }

    def exports: Bindings = scope.bindings

    def scoped[R](block: => R): R =
      val before = scope
      scope = Scope.Local(Bindings.empty, Bindings.empty, before)
      try { block } finally { scope = before }


    // (re-)enter the namespace
    def namespace[R](name: String)(block: => R): R =
      val before = scope
      val bindings = before.bindings.namespaces.getOrElse(name, Bindings.empty)
      val namespace = Scope.Named(name, bindings, before)
      scope = namespace
      try { block } finally {
        before.bindings = before.bindings.copy(namespaces = before.bindings.namespaces.updated(name, namespace.bindings))
        scope = before
      }

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

  def toplevel(modulePath: List[String], prelude: Bindings): Scoping = Scoping(modulePath, Scope.Global(prelude, Bindings.empty))
}
