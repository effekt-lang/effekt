package effekt
package util

import scala.quoted.*

final class NoContext
given NoContext = new NoContext

/**
 * Automatically generates traversal code.
 *
 * Use macro [[structural]] to generate traversal code.
 * See [[effekt.core.Tree.Rewrite]] for an example how to use it. The generated
 * traversal automatically uses all (and only those) methods that are called [[rewrite]]
 * to traverse the tree and rewrite it. The traversal stops when there is no [[rewrite]]
 * method to be called. It also automatically traverses lists and options if the
 * payload can be traversed.
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
 *
 * TODO also support single product types, like [[effekt.core.Constructor]].
 *
 * TODO if nothing is transformed, as in
 *     case ValueVar(id, annotatedType) => ValueVar.apply(id, annotatedType)
 *   it should result in
 *     case v @ ValueVar(id, annotatedType) => v
 *
 * TODO also support context parameters, like in [[generator.chez.Tree.Rewrite]]
 *
 * TODO add a variant without partial function (like in source.Rewrite.rewrite(Handler))
 *
 *
 * TODO maybe do not specialize to "rewrite", but make work in general for:
 *
 *   def <name>(<structuralarg>, <otherargs>, ...)(<otherargs>...) = structural
 */
trait Visitor[C] {

  /**
   * Hook that can be overridden to perform an action at every node in the tree
   */
  def visit[T](source: T)(visitor: T => T)(using C): T = visitor(source)

  inline def structural[T](sc: T, p: PartialFunction[T, T])(using ctx: C): T =
    if (p.isDefinedAt(sc)) { p(sc) } else { structural(sc) }

  inline def structural[T](sc: T)(using ctx: C): T =
    ${structuralImpl[this.type, T, C]('{sc}, '{ctx}, false)}

  inline def structuralVisit[T](sc: T, p: PartialFunction[T, T])(using ctx: C): T =
    visit(sc) { t => structural(t, p) }

  /**
   * Same as structural, but prints the generated code.
   */
  inline def structuralDebug[T](sc: T)(using ctx: C): T =
    ${structuralImpl[this.type, T, C]('{sc}, '{ctx}, true)}
}

class StructuralVisitor[Self: Type, Q <: Quotes, C: Type](ctx: quoted.Expr[C], debug: Boolean)(using val q: Q) {
  import q.reflect.*

  case class RewriteMethod(tpe: TypeRepr, method: Symbol)

  val rewriteName = {
    // TODO find method that has this.type == Self
    def findMethod(sym: Symbol): Symbol =
      if (sym.isDefDef && !sym.isAnonymousFunction) sym else findMethod(sym.owner)

    findMethod(Symbol.spliceOwner).name
  }

  val owner = Symbol.spliceOwner
  val self = TypeRepr.of[Self].typeSymbol

  // the context we pass around to all recursive function calls
  val context = ctx.asTerm

  val rewrites: List[RewriteMethod] = self.methodMember(rewriteName).map { m =>
    // TODO using .tree is discouraged. Find type differently!
    m.tree match {
      case DefDef(name, params, tpe, rhs) =>
        RewriteMethod(tpe.tpe, m)
      case _ => report.errorAndAbort("Not supported.")
    }
  }

  // For now, we can only traverse types that have a rewrite.
  // Later, we will add support for lists, option, and tuples of traversables.
  def canTraverse(tpt: TypeRepr): Boolean = hasRewriteFor(tpt)

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

  extension (sym: Symbol) {
    def isTrait: Boolean = sym.flags.is(Flags.Trait)
    def isEnum: Boolean = sym.flags.is(Flags.Enum) && !sym.isEnumCase
    def isEnumCase: Boolean = sym.flags.is(Flags.Case)
    def isSumType: Boolean = sym.isTrait || sym.isEnum
    def isCaseClass: Boolean = !isSumType && sym.isClassDef
  }

  // { (x: [[tpe]]) => [[body]](x) : [[tpe]] }
  def makeClosure(tpe: TypeRepr, owner: Symbol, body: (Symbol, Term) => Term): Term = {
    val methodSym = Symbol.newMethod(owner, rewriteName,
      MethodType(List("t"))(m => List(tpe), m => tpe))
    Block(List(
      DefDef(methodSym, {
        case List(List(t: Term)) => Some(body(methodSym, t))
        case _ => ???
      })
    ), Closure(Ref(methodSym), None))
  }

  // rewrite(arg: tpe)(context)
  def rewrite(tpe: TypeRepr, arg: Term): Term = {
    val m = rewriteFor(tpe).getOrElse { report.errorAndAbort(s"No rewrite method for type ${tpe}!") }
    Apply(Apply(Ref(m.method), List(arg)), List(context))
  }

  def rewriteExpression(term: Term, tpe: TypeRepr, owner: Symbol): Term = {
    // base case: we can directly rewrite this type: rewrite(e)(context)
    if (hasRewriteFor(tpe))
      return rewrite(tpe, term)

    // it is a list or option: term.map({ t => rewrite(t) })
    if (tpe <:< TypeRepr.of[List[Any]] || tpe <:< TypeRepr.of[Option[Any]]) tpe.typeArgs match {
      case List(elTpe) if hasRewriteFor(elTpe) =>
        return Select.overloaded(term, "map", List(elTpe),
          List(makeClosure(elTpe, owner, (owner, t) => rewriteExpression(t, elTpe, owner))), tpe)
      case _ =>
        ()
    }

    // fall back: identity
    term
  }

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
    // TODO not sure this is needed:
    //   Typed(Wildcard(), Inferred(tpt))
    val bindFields = (fieldSym zip fieldTypes).map { case (f, tpt) => Bind(f, Wildcard()) }

    // case Return.unapply(e @ _) => Return.apply(REWRITE[[e]])
    CaseDef(TypedOrTest(Unapply(destructor, Nil, bindFields), TypeIdent(typeSym)), None,
      Block(Nil, Apply(constructor, (fieldSym zip fieldTypes).map {
        case (f, tpt) => rewriteExpression(Ref(f), tpt, owner)
      })))

  // case e: Expr => rewrite(e)
  def rewriteCaseTrait(sym: Symbol, owner: Symbol): CaseDef = {
    val tpt = sym.typeRef
    val e = Symbol.newBind(owner, "e", Flags.Local, tpt)

    // case e @ (_: Expr) => rewrite(e)
    CaseDef(Bind(e, Typed(Wildcard(), TypeIdent(sym))), None,
      Block(Nil, rewrite(tpt, Ref(e))))
  }

  // the main entry point of this macro
  def structural[T: Type](sc: quoted.Expr[T]): quoted.Expr[T] = {

    val tpt = TypeRepr.of[T]
    val typeSym: Symbol = tpt.typeSymbol

    val variants: List[Symbol] =
      if (typeSym.isSumType) typeSym.children
      else List(typeSym)

    // TODO check probably not necessary. Drop?
    variants.foreach { v =>
      val tpt = v.typeRef
      if (!canTraverse(tpt)) { report.errorAndAbort(s"Don't know how to generate traversal for case ${ v }") }
    }
    val scrutinee = sc.asTerm

    // We add a case that should never occur, only because exhaustivity checks seem not
    // to work for generated matches.
    // case _ => sys.error("Should never happen!")
    val catchall = CaseDef(Wildcard(), None,
      Block(Nil, '{ sys.error("Should never happen!") }.asTerm))

    // x match { ... }
    val rewritten = Match(scrutinee, variants.map(v => rewriteCase(v, owner)) :+ catchall)
    if (debug) { report.info(Printer.TreeShortCode.show(rewritten)) }
    rewritten.asExprOf[T]
  }
}

def structuralImpl[Self: Type, T: Type, C: Type](
  sc: quoted.Expr[T], c: quoted.Expr[C], debug: Boolean
)(using q: Quotes): quoted.Expr[T] = new StructuralVisitor[Self, q.type, C](c, debug).structural[T](sc)

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
