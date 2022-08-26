package effekt
package generator
package chez

// TODO choose appropriate representation and apply conversions
case class ChezName(name: String)

case class Binding(name: ChezName, expr: Expr)

case class Block(definitions: List[Def], expressions: List[Expr], result: Expr)

case class Handler(constructorName: ChezName, operations: List[Operation])
case class Operation(name: ChezName, params: List[ChezName], k: ChezName, body: Expr)

/**
 * This file defines the syntax of Chez Scheme as it is the image of our translation.
 */
enum Expr {

  // e.g. (<EXPR> <EXPR>)
  case Call(callee: Expr, arguments: List[Expr])

  // e.g. (display "foo")
  case RawExpr(raw: String)

  // e.g. 42 (represented as Scala string "42") and inserted verbatim
  case RawValue(raw: String)

  // e.g. (let ([x 42]) (+ x x))
  case Let(bindings: List[Binding], body: Block)

  // e.g. (let* ([x 42] [y x]) (+ x y))
  case Let_*(bindings: List[Binding], body: Block)

  // e.g. (lambda (x y) body)
  case Lambda(params: List[ChezName], body: Block)

  // e.g. (if COND THEN ELSE)
  case If(cond: Expr, thn: Expr, els: Expr)

  // e.g x
  case Variable(name: ChezName)

  // match and handle are both macros, stable across Chez variants, so we add them to the language.
  case Match(scrutinee: Expr, clauses: List[(Expr, Expr)])

  case Handle(handlers: List[Handler], body: Expr)
}
export Expr.*

enum Def {
  // e.g. (define x 42)
  case Constant(name: ChezName, value: Expr)

  // e.g. (define (f x y) ...)
  case Function(name: ChezName, params: List[ChezName], body: Block)

  case RawDef(raw: String)

  case Record(typeName: ChezName, constructorName: ChezName, predicateName: ChezName, uid: ChezName, fields: List[ChezName])
}
export Def.*

// smart constructors
def Call(name: ChezName, args: Expr*): Expr = Call(Variable(name), args.toList)

def Lambda(params: List[ChezName], body: Expr): Lambda = Lambda(params, Block(Nil, Nil, body))

def Function(name: ChezName, params: List[ChezName], body: Expr): Function = Function(name, params, Block(Nil, Nil, body))

def ChezString(chezString: String): Expr = RawExpr(s"\"${chezString}\"")

def Builtin(name: String, args: Expr*): Expr = Call(Variable(ChezName(name)), args.toList)

def curry(lam: chez.Lambda): chez.Lambda = lam.params.foldRight[chez.Lambda](chez.Lambda(Nil, lam.body)) {
  case (p, body) => chez.Lambda(List(p), body)
}

def cleanup(expr: Expr): Expr = DeadCodeElimination.rewrite(LetFusion.rewrite(expr)(using ()))(using ())

object LetFusion extends Tree.Rewrite[Unit] {
  override def expr(using Unit) = {
    case Let(bindings, body) => rewrite(body) match {
      case Block(Nil, Nil, Let(otherBindings, body)) => Let_*((bindings map rewrite) ++ otherBindings, body)
      case Block(Nil, Nil, Let_*(otherBindings, body)) => Let_*((bindings map rewrite) ++ otherBindings, body)
      case b => Let(bindings map rewrite, b)
    }
    case Let_*(bindings, body) => rewrite(body) match {
      case Block(Nil, Nil, Let(otherBindings, body)) => Let_*((bindings map rewrite) ++ otherBindings, body)
      case Block(Nil, Nil, Let_*(otherBindings, body)) => Let_*((bindings map rewrite) ++ otherBindings, body)
      case b => Let(bindings map rewrite, b)
    }
  }
}

object DeadCodeElimination extends Tree.Rewrite[Unit] {
  override def expr(using Unit) = {
    case Let(bindings, body) =>
      val transformedBody = rewrite(body)
      val fv = FreeVariables.query(transformedBody)
      val bs = bindings.filter {
        case Binding(name, binding) => (fv contains name) || !isInlinable(binding)
      }
      Let(bs, transformedBody)
  }
}

def isInlinable(e: Expr): Boolean = e match {
  case _: Variable => true
  case _: RawValue => true
  case _: Lambda => true
  case _ => false
}

object FreeVariables extends Tree.Query[Unit, Set[ChezName]] {

  given Unit = ()

  def empty = Set.empty
  def combine = _ ++ _

  def bound(bindings: List[Binding]): Set[ChezName] = bindings.map { b => b.name }.toSet
  def free(bindings: List[Binding]): Set[ChezName] = bindings.flatMap { b => query(b.expr) }.toSet

  override def expr(using Unit) = {
    case Variable(name) => Set(name)

    case Let(bindings, body) =>
      free(bindings) ++ (query(body) -- bound(bindings))

    case Let_*(bindings, body) =>
      val freeInBindings = bindings.foldRight(Set.empty[ChezName]) {
        case (Binding(name, b), free) => (free - name) ++ query(b)
      }
      freeInBindings ++ (query(body) -- bound(bindings))

    case Lambda(params, body) => query(body) -- params.toSet

    case Handle(handlers, body) =>
      query(body) ++ handlers.flatMap {
        case Handler(name, ops) => ops flatMap {
          case Operation(name, params, k, body) => query(body) -- params.toSet - k
        }
      }
  }

  override def defn(using Unit) = {
    case chez.Function(name, params, body) => query(body) -- params.toSet - name // recursive functions
    case chez.Constant(name, expr) => query(expr)
  }

  override def query(b: Block)(using Unit): Set[ChezName] = b match {
    // defs are potentially recursive!
    case Block(defs, exprs, result) =>
      val mutualFunctions = defs.collect { case f: chez.Function => f.name }.toSet

      val freeInDefs = defs.foldRight(Set.empty[ChezName]) {
        case (f : chez.Function, free) => (free - f.name) ++ (query(f) -- mutualFunctions)
        case (c : chez.Constant, free) => (free - c.name) ++ query(c)
        // TODO Records!
        case (_, free) => free
      }

      val boundByDefs = defs.collect {
        case f: chez.Function => f.name
        case c: chez.Constant => c.name
        // TODO Records!
      }.toSet

      freeInDefs ++ ((query(result) ++ exprs.flatMap(query)) -- boundByDefs)
  }
}


object Tree {

  // This solution is between a fine-grained visitor and a untyped and unsafe traversal.
  trait Rewrite[Ctx] {

    // Hooks to override
    def expr(using C: Ctx): PartialFunction[Expr, Expr] = PartialFunction.empty
    def defn(using C: Ctx): PartialFunction[Def, Def] = PartialFunction.empty

    /**
     * Hook that can be overridden to perform an action at every node in the tree
     */
    def visit[T](source: T)(visitor: T => T)(using Ctx): T = visitor(source)

    def rewrite(block: Block)(using Ctx): Block = visit(block) {
      case Block(defs, exprs, result) => Block(defs map rewrite, exprs map rewrite, rewrite(result))
    }

    def rewrite(binding: Binding)(using Ctx): Binding = visit(binding) {
      case Binding(name, expr) => Binding(name, rewrite(expr))
    }

    def rewrite(h: Handler)(using Ctx): Handler = visit(h) {
      case Handler(constructorName, operations) => Handler(constructorName, operations map rewrite)
    }
    def rewrite(op: Operation)(using Ctx): Operation = visit(op) {
      case Operation(name, params, k, body) => Operation(name, params, k, rewrite(body))
    }

    def rewrite(e: Expr)(using Ctx): Expr = visit(e) {
      case e if expr.isDefinedAt(e) => expr(e)
      case e: Variable => e
      case e: RawExpr => e
      case e: RawValue => e

      case Call(callee, arguments) => Call(rewrite(callee), arguments map rewrite)
      case Let(bindings, body) => Let(bindings map rewrite, rewrite(body))
      case Let_*(bindings, body) => Let_*(bindings map rewrite, rewrite(body))
      case Lambda(params, body) => Lambda(params, rewrite(body))
      case If(cond, thn, els) => If(rewrite(cond), rewrite(thn), rewrite(els))
      case Match(scrutinee, clauses) => Match(rewrite(scrutinee), clauses map {
        case (p, e) => (rewrite(p), rewrite(e))
      })
      case Handle(handlers, body) => Handle(handlers map rewrite, rewrite(body))
    }

    def rewrite(t: Def)(using C: Ctx): Def = visit(t) {
      case t if defn.isDefinedAt(t) => defn(t)
      case r : RawDef => r
      case r : Record => r
      case Constant(name, value) => Constant(name, rewrite(value))
      case Function(name, params, body) => Function(name, params, rewrite(body))
    }
  }

  trait Visit[Ctx] extends Query[Ctx, Unit] {
    override def empty = ()
    override def combine = (_, _) => ()
    override def combineAll(rs: List[Unit]): Unit = ()
  }

  trait Query[Ctx, Res] {

    def empty: Res
    def combine: (Res, Res) => Res
    def combineAll(rs: List[Res]): Res = rs.foldLeft(empty)(combine)

    // Hooks to override
    def expr(using Ctx): PartialFunction[Expr, Res] = PartialFunction.empty
    def defn(using Ctx): PartialFunction[Def, Res] = PartialFunction.empty

    /**
     * Hook that can be overridden to perform an action at every node in the tree
     */
    def visit[T](t: T)(visitor: T => Res)(using Ctx): Res = visitor(t)

    /**
     * Hook that can be overridden to perform something for each new lexical scope
     */
    def scoped(action: => Res)(using Ctx): Res = action

    def query(e: Expr)(using Ctx): Res = visit(e) {

      case e if expr.isDefinedAt(e) => expr.apply(e)

      case e: RawExpr => empty
      case e: RawValue => empty
      case e: Variable => empty

      case Let(bindings, body) => combineAll(query(body) :: bindings.map(query))
      case Let_*(bindings, body) => combineAll(query(body) :: bindings.map(query))
      case Lambda(params, body) => query(body)

      case Call(callee, arguments) => combineAll(query(callee) :: arguments.map(query))
      case If(cond, thn, els) => combineAll(List(query(cond), query(thn), query(els)))

      case Match(scrutinee, clauses) => combineAll(query(scrutinee) :: clauses.map { case (p, e) => combine(query(p), query(e)) })
      case Handle(handlers, body) =>
        combineAll(query(body) :: handlers.map {
          case Handler(name, ops) => combineAll(ops map {
            case Operation(name, params, k, body) => query(body)
          })
        })
    }

    def query(d: Def)(using Ctx): Res = visit(d) {

      case d if defn.isDefinedAt(d) => defn.apply(d)

      case Constant(name, value) => query(value)
      case Function(name, params, body) => query(body)
      case RawDef(raw) => empty
      case Record(typeName, constructorName, predicateName, uid, fields) => empty
    }

    def query(b: Block)(using Ctx): Res = visit(b) {
      case Block(defs, exprs, result) => scoped { combineAll(query(result) :: defs.map(query) ++ exprs.map(query)) }
    }

    def query(b: Binding)(using Ctx): Res = visit(b) {
      case Binding(name, expr) => query(expr)
    }
  }
}