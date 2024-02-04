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

// TODO add visibily like the following
//
//case class Bindings[F[_]](
//  terms: Map[String, Set[F[TermSymbol]]], // terms can be overloaded
//  types: Map[String, F[TypeSymbol]],
//  captures: Map[String, F[Capture]],
//  namespaces: Map[String, Bindings[F]]
//)
//
//enum Visibility[T] {
//  case Public(binding: T)
//  case Private(binding: T)
//
//  def binding: T
//}

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

    // The following would be more flexible, but is difficult to implement with a
    // two-pass (preresolve, resolve) process.
    //
    //    /**
    //     * A scope introduced by importing bindings
    //     */
    //    case Imports(imports: Bindings, var bindings: Bindings, outer: Scope)

    /**
     * A local scope introduced by functions, blocks, etc.
     */
    case Local(var imports: Bindings, var bindings: Bindings, outer: Scope)

    /**
     * All scopes introduce bindings
     */
    def bindings: Bindings
    def bindings_=(b: Bindings): Unit

    //    def iterator: Iterator[Scope] = new Iterator[Scope] {
    //      var curr: Option[Scope] = Some(self)
    //      override def hasNext: Boolean = curr.isDefined
    //      override def next(): Scope =
    //        curr.get match {
    //          case s @ Scope.Global(bindings, exports) =>
    //            curr = None; s
    //          case s @ Scope.Named(name, bindings, exports, outer) =>
    //            curr = Some(outer); s
    //          case s @ Scope.Imports(imports, bindings, outer) =>
    //            curr = Some(outer); s
    //          case s @ Scope.Local(bindings, outer) =>
    //            curr = Some(outer); s
    //        }
    //    }
  }

  case class Scoping(modulePath: List[String], var scope: Scope) {
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
    private def first[T](scope: Scope)(select: Bindings => Option[T]): Option[T] = scope match {
      case Scope.Global(imports, bindings) =>
        select(bindings) orElse select(imports)
      case Scope.Named(name, bindings, outer) =>
        select(bindings) orElse first(outer)(select)
      case Scope.Local(imports, bindings, outer) =>
        select(bindings) orElse select(imports) orElse first(outer)(select)
    }

    private def all[T](scope: Scope)(select: Bindings => T): List[T] = scope match {
      case Scope.Global(imports, bindings) =>
        select(bindings) :: select(imports) :: Nil
      case Scope.Named(name, bindings, outer) =>
        select(bindings) :: all(outer)(select)
      case Scope.Local(imports, bindings, outer) =>
        select(bindings) :: select(imports) :: all(outer)(select)
    }

    /**
     * Searches the nested scopes to find the first term.
     * Fails if:
     *   - there are multiple matching terms in the same scope
     *   - there a no matching terms at all
     */
    def lookupFirstTerm(key: String)(using E: ErrorReporter): TermSymbol =
      lookupFirstTermOption(key) getOrElse { E.abort(s"Could not resolve term ${key}") }

    def lookupFirstTermOption(key: String)(using E: ErrorReporter): Option[TermSymbol] =
      first(scope) { _.terms.get(key).map { syms =>
        if (syms.size > 1) E.abort(s"Ambiguous reference to ${key}")
        else syms.head
      }}

    def lookupType(key: String)(using E: ErrorReporter): TypeSymbol =
      lookupTypeOption(key) getOrElse { E.abort(s"Could not resolve type ${key}") }

    def lookupTypeOption(key: String)(using E: ErrorReporter): Option[TypeSymbol] =
      first(scope) { _.types.get(key) }

    def lookupCapture(key: String)(using E: ErrorReporter): Capture =
      first(scope) { _.captures.get(key) } getOrElse E.abort(s"Could not resolve capture ${key}")

    def lookupOverloaded(key: String, filter: TermSymbol => Boolean)(using ErrorReporter): List[Set[TermSymbol]] =
      all(scope) { _.terms.getOrElse(key, Set.empty).filter(filter) }

    def lookupOperation(key: String)(using ErrorReporter): List[Set[Operation]] =
      all(scope) { _.terms.getOrElse(key, Set.empty).collect {
        case o: Operation => o
      }}

    // can be a term OR a type symbol
    def lookupFirst(key: String)(using E: ErrorReporter): Symbol =
      lookupFirstOption(key) getOrElse { E.abort(s"Could not resolve ${key}") }

    def lookupFirstOption(key: String)(using E: ErrorReporter): Option[Symbol] =
      first(scope) {
        case Bindings(terms, types, captures, namespaces) =>
          (terms.get(key).map(_.toList), types.get(key)) match {
            case (Some(List(t)), None) => Some(t)
            case (None, Some(t)) => Some(t)
            // give precedence to the type level effect, if an equally named effect op is in scope
            case (Some(List(t1: Operation)), Some(t2: Interface)) => Some(t2)
            case (Some(t1), Some(t2)) =>
              E.abort(s"Ambiguous reference to ${key}. Can refer to a term or a type.")
            case (None, None) => None
            case _            => E.abort(s"Ambiguous reference to ${key}.")
          }
      }

    def currentTermsFor(key: String): Set[TermSymbol] =
      scope.bindings.terms.getOrElse(key, Set.empty)

    def allTermsFor(key: String): Set[TermSymbol] =
      all(scope) { _.terms.getOrElse(key, Set.empty) }.flatten.toSet

    def define(key: String, sym: TermSymbol)(using E: ErrorReporter): Unit = {
      val bindings = scope.bindings
      val termsInScope = currentTermsFor(key)
      sym match {
        case v: ValueSymbol =>
          if (termsInScope.exists(_.isInstanceOf[BlockSymbol])) {
            E.abort(s"Value ${key} has the same name as a block definition in the same scope, which is not allowed.")
          }
        case b: BlockSymbol =>
          if (termsInScope.exists(_.isInstanceOf[ValueSymbol])) {
            E.abort(s"Block ${key} has the same name as a value definition in the same scope, which is not allowed.")
          }
      }
      scope.bindings = bindings.copy(terms = bindings.terms.updated(key, termsInScope + sym))
    }

    def define(key: String, sym: TypeSymbol)(using E: ErrorReporter): Unit =
      val bindings = scope.bindings
      lookupTypeOption(key).foreach { shadowed =>
        if sym.isInstanceOf[TypeVar] && !shadowed.isInstanceOf[TypeVar] then
          E.warning(pp"Type parameter ${key} shadows outer definition of ${sym}")
      }
      scope.bindings = bindings.copy(types = bindings.types.updated(key, sym))

    def define(key: String, capt: Capture)(using ErrorReporter): Unit =
      scope.bindings = scope.bindings.copy(captures = scope.bindings.captures.updated(key, capt))

    // TODO implement proper checks and merging behavior
    def merge(oldNamespaces: Map[String, Bindings], newNewspaces: Map[String, Bindings]): Map[String, Bindings] =
      oldNamespaces ++ newNewspaces

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
