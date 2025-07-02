package effekt
package symbols

import effekt.source.IdRef
import effekt.util.messages.ErrorReporter

import scala.annotation.tailrec

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
 * An immutable container of bindings.
 */
case class Namespace(
  terms: Map[String, Set[TermSymbol]] = Map.empty,
  types: Map[String, TypeSymbol] = Map.empty,
  captures: Map[String, Capture] = Map.empty,
  namespaces: Map[String, Namespace] = Map.empty
) {

  def importAll(other: Bindings): Namespace = other match {
    case Bindings(terms2, types2, captures2, namespaces2) =>
      val withTerms = terms2.foldLeft(this) {
        case (ns, (name, syms)) =>
          syms.foldLeft(ns) { (ns2, sym) =>
            ns2.addTerm(name, sym)
          }
      }
      val withTypes = types2.foldLeft(withTerms) {
        case (ns, (name, tpe)) =>
          ns.setType(name, tpe)
      }
      val withCaptures = captures2.foldLeft(withTypes) {
        case (ns, (name, capt)) =>
          ns.setCapture(name, capt)
      }
      namespaces2.foldLeft(withCaptures) {
        case (ns, (name, otherNs)) =>
          ns.updateNamespace(name, _.importAll(otherNs))
      }
  }

  def addTerm(name: String, sym: TermSymbol): Namespace =
    copy(terms = terms.updated(name, terms.getOrElse(name, Set.empty) + sym))

  def setType(name: String, sym: TypeSymbol): Namespace =
    copy(types = types.updated(name, sym))

  def setCapture(name: String, capt: Capture): Namespace =
    copy(captures = captures.updated(name, capt))

  def updateNamespace(name: String, f: Namespace => Namespace): Namespace =
    copy(namespaces = namespaces.updated(name, f(namespaces.getOrElse(name, Namespace.empty))))

  def getNamespace(name: String): Option[Namespace] =
    namespaces.get(name)

  def operations: Map[String, Set[Operation]] =
    types.values.toSet.flatMap {
      case BlockTypeConstructor.Interface(_, _, operations, _) => operations.toSet
      case _ => Set.empty
    }.groupMap(_.name.name)(op => op)

  def toBindings: Bindings =
    Bindings(
      terms,
      types,
      captures,
      namespaces.view.mapValues(_.toBindings).toMap
    )
}

object Namespace {
  val empty: Namespace = Namespace()
}

object scopes {

  enum Scope {
    self =>

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
    case Local(name: Option[String], imports: Namespace, bindings: Namespace, outer: Scope)

    /**
     * All scopes introduce (mutable) namespaces for their bindings
     */
    def bindings: Namespace

    def imports(using E: ErrorReporter): Namespace = this match {
      case s: Scope.Named => E.abort("Can only import at the top of a file or function definition.")
      case s@Scope.Global(imports, bindings) => imports
      case s@Scope.Local(_, imports, bindings, outer) => imports
    }

    /**
     * Return a new Scope with `newBindings`.
     */
    def withBindings(newBindings: Namespace): Scope = this match {
      case g@Global(_, _) => g.copy(bindings = newBindings)
      case n@Named(_, _, _) => n.copy(bindings = newBindings)
      case l@Local(_, _, _, _) => l.copy(bindings = newBindings)
    }

    /**
     * Return a new Scope with `newImports`.
     */
    def withImports(newImports: Namespace): Scope = this match {
      case g@Global(_, _) => g.copy(imports = newImports)
      case n@Named(_, _, _) => throw new IllegalStateException("Cannot change imports in a named scope")
      case l@Local(_, _, _, _) => l.copy(imports = newImports)
    }

    /**
     * Returns a new scope with the `newNamespaces`
     */
    def withNamespaces(newNamespaces: Map[String, Namespace]): Scope = this match {
      case g@Global(imports, bindings) => g.copy(bindings = bindings.copy(namespaces = newNamespaces))
      case n@Named(name, bindings, outer) => n.copy(bindings = bindings.copy(namespaces = newNamespaces))
      case l@Local(name, imports, bindings, outer) => l.copy(bindings = bindings.copy(namespaces = newNamespaces))
    }
  }

  case class Scoping(modulePath: List[String], var scope: Scope) {
    def importAs(bindings: Bindings, path: List[String])(using E: ErrorReporter): Unit = {
      @tailrec
      def go(ns: Namespace, path: List[String]): Namespace = path match {
        case pathSeg :: rest =>
          ns.updateNamespace(pathSeg, child => go(child, rest))
        case Nil =>
          ns.importAll(bindings)
      }

      scope = scope.withImports(go(scope.imports, path))
    }

    // TODO check shadowing etc. Also here concrete functions will *always* shadow imports,
    //   regardless of the order of importing / defining.
    def importAll(imports: Bindings)(using E: ErrorReporter): Unit = {
      scope = scope.withImports(scope.imports.importAll(imports))
    }

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
        case Scope.Local(_, imports, bindings, outer) =>
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
        case Scope.Local(_, imports, bindings, outer) =>
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

    def lookupOverloadedMethod(id: IdRef, filter: TermSymbol => Boolean)(using ErrorReporter): List[Set[Operation]] =
      all(id.path, scope) { namespace =>
        namespace.operations.getOrElse(id.name, Set.empty).filter(filter)
      }

    def lookupOperation(path: List[String], name: String)(using ErrorReporter): List[Set[Operation]] =
      all(path, scope) { namespace =>
        namespace.operations.getOrElse(name, Set.empty)
      }.filter { namespace => namespace.nonEmpty }

    def lookupFunction(path: List[String], name: String)(using ErrorReporter): List[Set[Callable]] =
      all(path, scope) { namespace =>
        namespace.terms.getOrElse(name, Set.empty).collect { case c: Callable if !c.isInstanceOf[Operation] => c }
      }.filter { namespace => namespace.nonEmpty }

    def lookupFirstBlockParam(path: List[String], name: String)(using ErrorReporter): Set[BlockParam] =
       first(path, scope) { namespace =>
        namespace.terms.get(name).map(set =>
          set.collect { case bp: BlockParam => bp }
        )
      }.getOrElse(Set.empty)

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
      scope = scope.withBindings(scope.bindings.addTerm(name, sym))
    }

    def define(name: String, sym: TypeSymbol)(using E: ErrorReporter): Unit =
      val bindings = scope.bindings
      if bindings.types.isDefinedAt(name) then
        E.error(pp"Type ${name} already defined in the current scope")

      lookupTypeOption(Nil, name).foreach { shadowed =>
        if sym.isInstanceOf[TypeVar] && !shadowed.isInstanceOf[TypeVar] then
          E.warning(pp"Type parameter ${name} shadows outer definition of ${sym}")
      }
      scope = scope.withBindings(bindings.setType(name, sym))

    def define(name: String, capt: Capture)(using ErrorReporter): Unit =
      scope = scope.withBindings(scope.bindings.setCapture(name, capt))

    def exports: Bindings = scope.bindings.toBindings

    def scoped[R](name: String, block: => R): R =
      val before = scope
      scope = Scope.Local(Some(name), Namespace.empty, Namespace.empty, before)
      try { block } finally { scope = before }

    def scoped[R](block: => R): R =
      val before = scope
      scope = Scope.Local(None, Namespace.empty, Namespace.empty, before)
      try { block } finally { scope = before }

    // (re-)enter the namespace
    def namespace[R](name: String)(block: => R): R =
      val before = scope
      val childNamespace = before.bindings.getNamespace(name).getOrElse(Namespace.empty)
      val newBefore = before.withNamespaces(before.bindings.namespaces.updated(name, childNamespace))
      scope = Scope.Named(name, childNamespace, newBefore)
      try { block } finally { scope = before.withNamespaces(before.bindings.namespaces.updated(name, scope.bindings)) }

    // returns the current path
    def path: Option[List[String]] =
      def collect(scope: Scope): Option[List[String]] = scope match {
        case Scope.Global(imports, bindings) => Some(modulePath)
        case Scope.Named(name, bindings, outer) => collect(outer) match {
          case Some(path) => Some(path :+ name)
          // name spaces also START a new path, if there hasn't been one, already
          case None => Some(List(name))
        }
        case Scope.Local(_, imports, bindings, outer) => None
      }
      collect(scope)
  }

  def toplevel(modulePath: List[String], prelude: Bindings): Scoping =
    val imports = Namespace.empty.importAll(prelude)
    Scoping(modulePath, Scope.Global(imports, Namespace.empty))
}
