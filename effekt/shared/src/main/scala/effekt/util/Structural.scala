package effekt
package util

import scala.quoted.*

/**
 * Automatically generates traversal code.
 *
 * Use macro [[structural]] to generate traversal code.
 * See [[effekt.core.Tree.Rewrite]] for an example how to use it.
 * To traverse the tree and rewrite it, he generated traversal automatically uses all
 * (and only those) methods that are called the same as the method the macro is used in.
 * The traversal stops when there is no such method to be called. It also automatically
 * traverses lists and options if the payload can be traversed.
 *
 * ==Usage Example==
 *
 * The following example will increment all integer literals in a tree:
 * {{{
 *   enum Exp { case Lit(n: Int); case Add(l: Exp, r: Exp) }
 *   object Test extends Structural {
 *     def increment(e: Exp): Exp = structural(e)
 *     def increment(n: Int): Int = n + 1
 *   }
 * }}}
 *
 * The call to [[structural]] above generates the following code (output of [[structuralDebug]]))
 * {{{
 *   e match {
 *     case Lit(n)    => Lit.apply(increment(n))
 *     case Add(l, r) => Add.apply(increment(l), increment(r))
 *     case _ => sys.error("Should never happen!")
 *   }
 * }}}
 *
 * As can be seen, `increment` is also called on the integer `n` since the function
 * `increment(n: Int)` is present in `Test`.
 *
 * The recursive functions (`increment` in the example above) can take arbitrary
 * (term-level) arguments that are passed along to recursive calls.
 *
 * ==Usage Example: Query==
 * {{{
 *   enum Exp { case Lit(n: Int); case Add(exp: Exp, exps: List[Exp]) }
 *   object AllLiterals extends util.Structural {
 *     def allLits(e: Exp): Set[Int] = structuralQuery(e, Set.empty, _ ++ _)
 *     def allLits(n: Int): Set[Int] = Set(n)
 *   }
 *   def demo() = AllLiterals.allLits(Add(Lit(0), List(Lit(1), Lit(2), Lit(3))))
 * }}}
 *
 * Running `demo` will result in `Set(0, 1, 2, 3)`.
 *
 * The generated traversal is:
 * {{{
 *   e match {
 *     case Lit(n) => allLits(n)
 *     case Add(exp, exps) => combine(allLits(exp), combine(exps.map(t => allLits(t))))
 *     case _ => sys.error("Should never happen!")
 *  }
 * }}}
 *
 * Note: In the generated code, it looks like `combine` is called with multiple arguments. However, it taking a `Seq`
 * we are calling it as a var-arg.
 *
 * ==Debugging==
 *
 * Tipp: replace [[structural]] by [[structuralDebug]] to have the generated
 *   code be printed at compile time.
 *
 *   If there is a bug in the macro, try uncommenting `-Xcheck-macros` in [[build.sbt]].
 *
 * Here are some useful resources explaining details of the Scala 3 macro system:
 *
 * @see https://eed3si9n.com/intro-to-scala-3-macros/
 * @see https://docs.scala-lang.org/scala3/guides/macros/best-practices.html
 */
trait Structural {

  /**
   * Performs structural recursion on [[sc]] and reconstructs a value of [[T]]
   * while doing so.
   */
  inline def structural[T](sc: T): T =
    ${structuralImpl[this.type, T]('{sc}, false)}

  /**
   * Like structural without [[p]], but first consults the partial function before
   * destructing [[sc]]
   */
  inline def structural[T](sc: T, inline p: PartialFunction[T, T]): T =
    if (p.isDefinedAt(sc)) { p(sc) } else { structural(sc) }

  /**
   * Same as structural, but prints the generated code.
   */
  inline def structuralDebug[T](sc: T): T =
    ${structuralImpl[this.type, T]('{sc}, true)}

  /**
   * Performs structural recursion on [[sc]] and collects the result in a monoid [[R]].
   */
  inline def structuralQuery[T, R](sc: T, empty: R, combine: (R, R) => R): R =
    structuralQuery[T, R](sc, empty, (all: Seq[R]) => all.fold(empty)(combine))

  inline def structuralQuery[T, R](sc: T, empty: R, combine: Seq[R] => R): R =
    ${queryImpl[this.type, T, R]('{sc}, '{empty}, '{combine}, false)}

  inline def structuralQueryDebug[T, R](sc: T, empty: R, combine: Seq[R] => R): R =
    ${queryImpl[this.type, T, R]('{sc}, '{empty}, '{combine}, true)}
}


class StructuralMacro[Self: Type, Q <: Quotes](debug: Boolean)(using val q: Q) {
  import q.reflect.*

  case class RewriteMethod(tpe: TypeRepr, method: Symbol)

  // Info
  // ----

  val owner = Symbol.spliceOwner
  val self = TypeRepr.of[Self].typeSymbol

  val rewriteMethod =
    // TODO find method that has this.type == Self
    def findMethod(sym: Symbol): Symbol =
      if (sym.isDefDef && !sym.isAnonymousFunction) sym else findMethod(sym.owner)

    findMethod(Symbol.spliceOwner)

  val rewriteName = rewriteMethod.name

  val rewriteParams = rewriteMethod.paramSymss match {
    case (rec :: rest) :: sections =>
      if (rec.isTypeParam) report.errorAndAbort(s"For now, structurally recursive functions cannot be polymorphic, but got ${rec.name}.")
      (rest, sections)
    case _ => report.errorAndAbort(s"Parameters of structurally recursive function ${rewriteName} must not be empty.")
  }

  val rewrites: List[RewriteMethod] = self.methodMember(rewriteName).map { m =>
    TypeRepr.of[Self].memberType(m) match {
      case tpe: MethodType =>
        def findResult(tpe: TypeRepr): TypeRepr = tpe match {
          case tpe: LambdaType => findResult(tpe.resType)
          case _ => tpe
        }
        val argType = tpe.paramTypes.head
        val resType = findResult(tpe)

        // For a valid rewrite, the result needs to be a subtype of the argument
        //  if (!(resType <:< argType)) {
        //    val msg = s"To be a valid rewrite, the return type of method '${m.name}' should be a subtype of its argument type."
        //    val pos= m.pos.getOrElse(Position.ofMacroExpansion)
        //    report.errorAndAbort(msg, pos)
        //  }

        // For a valid query, the result needs to have the query domain type.
        // ...

        RewriteMethod(argType, m)
      case _ => report.errorAndAbort("Needs to be a method")
    }
  }

  /**
   * Returns all compatible rewrite methods sorted from more-specific to less-specific
   */
  def rewritesFor(tpt: TypeRepr): List[RewriteMethod] = rewrites.filter {
    case RewriteMethod(tpe, m) => tpt <:< tpe
  }.sortWith {
    case (m1, m2) => m1.tpe <:< m2.tpe
  }

  def rewriteFor(tpt: TypeRepr): Option[RewriteMethod] = rewritesFor(tpt).headOption

  def hasRewriteFor(tpt: TypeRepr): Boolean = rewritesFor(tpt).nonEmpty


  // Structural rewrites
  // -------------------

  /**
   * The main entry point of this macro
   */
  def structural[T: Type](sc: quoted.Expr[T]): quoted.Expr[T] = {

    val tpt = TypeRepr.of[T]
    val typeSym: Symbol = tpt.typeSymbol

    val variants: List[Symbol] =
      if (typeSym.isSumType) typeSym.children
      else List(typeSym)

    val scrutinee = sc.asTerm

    // x match { ... }
    val rewritten = Match(scrutinee, variants.map(v => rewriteCase(v, owner)) :+ catchAll)
    if (debug) { report.info(Printer.TreeShortCode.show(rewritten)) }
    rewritten.asExprOf[T]
  }


  // rewrite(arg: tpe, OTHERARGS, ...)(OTHERARGS, ...)
  def rewrite(tpe: TypeRepr, arg: Term): Term = {
    val m = rewriteFor(tpe).getOrElse { report.errorAndAbort(s"No rewrite method called '${rewriteName}' for type ${tpe}!") }
    val (sameSection, otherSections) = rewriteParams
    val baseCall = Apply(Ref(m.method), arg :: sameSection.map(Ref.apply))
    otherSections.foldLeft(baseCall) {
      case (call, args) => Apply(call, args.map(Ref.apply))
    }
  }

  def tryRewriteExpression(term: Term, tpe: TypeRepr, owner: Symbol): Option[Term] = {
    // base case: we can directly rewrite this type: rewrite(e)(context)
    if (hasRewriteFor(tpe)) {
      Some(rewrite(tpe, term))

    // it is a list or option: term.map({ t => rewrite(t) })
    } else if (tpe <:< TypeRepr.of[List[Any]] || tpe <:< TypeRepr.of[Option[Any]]) tpe.typeArgs match {
      case List(elTpe) if hasRewriteFor(elTpe) =>
        Some(Select.overloaded(term, "map", List(elTpe),
          List(makeClosure(elTpe, elTpe, owner, (owner, t) => rewriteExpression(t, elTpe, owner))), tpe))
      case _ =>
        None
    } else {
      None
    }
  }

  def rewriteExpression(term: Term, tpe: TypeRepr, owner: Symbol): Term =
    tryRewriteExpression(term, tpe, owner).getOrElse(term)

  def rewriteCase(sym: Symbol, owner: Symbol): CaseDef =
    if (sym.isCaseClass) rewriteCaseCaseClass(sym, owner)
    else rewriteCaseTrait(sym, owner)

  // case Return.unapply(e) => Return.apply(rewrite(e))
  def rewriteCaseCaseClass(typeSym: Symbol, owner: Symbol): CaseDef =
    val tpt = typeSym.typeRef

    val companion = typeSym.companionModule
    val constructor = Select.unique(Ref(companion), "apply")
    val destructor = Select.unique(Ref(companion), "unapply")

    // val expr: Expr
    val fields = typeSym.caseFields
    // Expr
    val fieldTypes: List[TypeRepr] = fields.map { f => tpt.memberType(f) }
    // e: Expr
    val fieldSym = (fields zip fieldTypes).map { case (f, tpt) => Symbol.newBind(owner, f.name, Flags.Local, tpt) }
    // e @ _
    val bindFields = (fieldSym zip fieldTypes).map { case (f, tpt) => Bind(f, Wildcard()) }

    var didRewrite = false
    val rewrittenFields = (fieldSym zip fieldTypes).map {
      case (f, tpt) =>
        val res = tryRewriteExpression(Ref(f), tpt, owner)
        if (res.isDefined) { didRewrite = true }
        res.getOrElse(Ref(f))
    }

    if (didRewrite) {
      // case Return.unapply(e @ _) => Return.apply(REWRITE[[e]])
      CaseDef(TypedOrTest(Unapply(destructor, Nil, bindFields), TypeIdent(typeSym)), None,
        Block(Nil, Apply(constructor, rewrittenFields)))
    } else {
      // case e @ Return.unapply(...) => e
      val e = Symbol.newBind(owner, "e", Flags.Local, tpt)
      CaseDef(Bind(e, TypedOrTest(Unapply(destructor, Nil, bindFields), TypeIdent(typeSym))), None,
        Block(Nil, Ref(e)))
    }

  // case e: Expr => rewrite(e)
  def rewriteCaseTrait(typeSym: Symbol, owner: Symbol): CaseDef = {
    val tpt = typeSym.typeRef
    val e = Symbol.newBind(owner, "e", Flags.Local, tpt)

    // case e @ (_: Expr) => rewrite(e)
    CaseDef(Bind(e, Typed(Wildcard(), TypeIdent(typeSym))), None,
      Block(Nil, rewrite(tpt, Ref(e))))
  }

  // Structural queries
  // ------------------
  def query[T: Type, R: Type](sc: quoted.Expr[T], empty: quoted.Expr[R], combine: quoted.Expr[Seq[R] => R]): quoted.Expr[R] = {
    val tpt = TypeRepr.of[T]
    val domainType = TypeRepr.of[R]
    val typeSym: Symbol = tpt.typeSymbol

    val variants: List[Symbol] =
      if (typeSym.isSumType) typeSym.children
      else List(typeSym)

    val scrutinee = sc.asTerm

    // x match { ... }
    val result = Match(scrutinee, variants.map(v => queryCase(v, domainType, empty.asTerm, combine.asTerm, owner)) :+ catchAll)
    if (debug) { report.info(Printer.TreeShortCode.show(result)) }
    result.asExprOf[R]
  }

  def queryCase(sym: Symbol, domainType: TypeRepr, empty: Term, combine: Term, owner: Symbol): CaseDef =
    if (sym.isCaseClass) queryCaseCaseClass(sym, domainType, empty, combine, owner)
    else queryCaseTrait(sym, empty, combine, owner)

  // case Foo.unapply(a, b, c) => combine(List())
  def queryCaseCaseClass(typeSym: Symbol, domainType: TypeRepr, empty: Term, combine: Term, owner: Symbol): CaseDef =
    val tpt = typeSym.typeRef

    val companion = typeSym.companionModule
    val destructor = Select.unique(Ref(companion), "unapply")

    // val expr: Expr
    val fields = typeSym.caseFields
    // Expr
    val fieldTypes: List[TypeRepr] = fields.map { f => tpt.memberType(f) }
    // e: Expr
    val fieldSym = (fields zip fieldTypes).map { case (f, tpt) => Symbol.newBind(owner, f.name, Flags.Local, tpt) }
    // e @ _
    val bindFields = (fieldSym zip fieldTypes).map { case (f, tpt) => Bind(f, Wildcard()) }

    val queriedFields = (fieldSym zip fieldTypes).toList.flatMap {
      case (f, tpt) => tryQueryExpression(Ref(f), tpt, domainType, empty, combine, owner)
    } match {
      case Nil => empty
      case el :: Nil => el
      case other => Apply(Select.unique(combine, "apply"), List(reifyList(other, domainType)))
    }

    // case Return.unapply(e @ _) => combine(REWRITE[[e]])
    CaseDef(TypedOrTest(Unapply(destructor, Nil, bindFields), TypeIdent(typeSym)), None,
      Block(Nil, queriedFields))


  def queryCaseTrait(sym: Symbol, empty: Term, combine: Term, owner: Symbol): CaseDef = {
    val tpt = sym.typeRef
    val e = Symbol.newBind(owner, "e", Flags.Local, tpt)

    // case e @ (_: Expr) => query(e)
    CaseDef(Bind(e, Typed(Wildcard(), TypeIdent(sym))), None,
      Block(Nil, rewrite(tpt, Ref(e))))
  }

  def queryExpression(term: Term, tpe: TypeRepr, domainType: TypeRepr, empty: Term, combine: Term, owner: Symbol): Term =
    tryQueryExpression(term, tpe, domainType, empty, combine, owner).getOrElse(empty)

  def tryQueryExpression(term: Term, tpe: TypeRepr, domainType: TypeRepr, empty: Term, combine: Term, owner: Symbol): Option[Term] = {
    // base case: we can directly rewrite this type: query(e)(context)
    if (hasRewriteFor(tpe)) {
      Some(rewrite(tpe, term))

    // it is a list or option: combine(term.toList.map({ t => query(t) }))
    } else if (tpe <:< TypeRepr.of[List[Any]] || tpe <:< TypeRepr.of[Option[Any]]) tpe.typeArgs match {
      case List(elTpe) if hasRewriteFor(elTpe) =>
        def toList(term: Term): Term = if (tpe <:< TypeRepr.of[List[Any]]) term else Select.unique(term, "toList")
        Some(Apply(Select.unique(combine, "apply"), List(
          Select.overloaded(toList(term), "map", List(domainType),
            List(makeClosure(elTpe, domainType, owner, (owner, t) => queryExpression(t, elTpe, domainType, empty, combine, owner))), tpe))))
      case _ =>
        None
    } else {
      None
    }
  }

  // Util
  // ----

  extension (sym: Symbol) {
    def isTrait: Boolean = sym.flags.is(Flags.Trait)
    def isEnum: Boolean = sym.flags.is(Flags.Enum) && !sym.isEnumCase
    def isEnumCase: Boolean = sym.flags.is(Flags.Case)
    def isSumType: Boolean = sym.isTrait || sym.isEnum
    def isCaseClass: Boolean = !isSumType && sym.isClassDef
  }

  // List.apply(l: _*)
  def reifyList(l: List[Term], elementType: TypeRepr): Term =
    Repeated(l, Inferred(elementType))

  // We add a case that should never occur, only because exhaustivity checks seem not
  // to work for generated matches.
  // case _ => sys.error("Should never happen!")
  def catchAll = CaseDef(Wildcard(), None, Block(Nil, '{sys.error("Should never happen!")}.asTerm))

  // { (x: [[from]]) => [[body]](x) : [[to]] }
  def makeClosure(from: TypeRepr, to: TypeRepr, owner: Symbol, body: (Symbol, Term) => Term): Term = {
    val methodSym = Symbol.newMethod(owner, rewriteName,
      MethodType(List("t"))(m => List(from), m => to))
    Block(List(
      DefDef(methodSym, {
        case List(List(t: Term)) => Some(body(methodSym, t))
        case _ => ???
      })
    ), Closure(Ref(methodSym), None))
  }
}

def structuralImpl[Self: Type, T: Type](
  sc: quoted.Expr[T], debug: Boolean
)(using q: Quotes): quoted.Expr[T] = new StructuralMacro[Self, q.type](debug).structural[T](sc)

def queryImpl[Self: Type, T: Type, R: Type](
  sc: quoted.Expr[T], empty: quoted.Expr[R], combine: quoted.Expr[Seq[R] => R], debug: Boolean
)(using q: Quotes): quoted.Expr[R] = new StructuralMacro[Self, q.type](debug).query[T, R](sc, empty, combine)

/**
 * For debugging and development.
 *
 * From https://eed3si9n.com/intro-to-scala-3-macros/
 */
inline def showTree[A](inline a: A): A = ${showTreeImpl[A]('{ a })}

def showTreeImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] = {
  import quotes.reflect.*
  report.info(Printer.TreeStructure.show(a.asTerm))
  a
}
