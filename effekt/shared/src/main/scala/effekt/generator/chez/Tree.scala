package effekt
package generator
package chez

import scala.language.implicitConversions

// TODO choose appropriate representation and apply conversions
case class ChezName(name: String)

case class Binding(name: ChezName, expr: Expr)

case class Block(definitions: List[Def], expressions: List[Expr], result: Expr)

case class Operation(name: ChezName, params: List[ChezName], k: ChezName, body: Expr)

/**
 * This file defines the syntax of Chez Scheme as it is the image of our translation.
 */
enum Expr {

  // e.g. (<EXPR> <EXPR>)
  case Call(callee: Expr, arguments: List[Expr])

  // e.g. "" <EXPR> " + " <EXPR>
  //   raw scheme splices, always start with a prefix string, then interleaved with arguments
  case RawExpr(raw: List[String], args: List[Expr])

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

  // e.g. (cond ([COND1 BRANCH1]... [else DEFAULT]))
  case Cond(clauses: List[(Expr, Expr)], default: Option[Expr])

  // e.g x
  case Variable(name: ChezName)

  // handle is a macro, stable across Chez variants, so we add it to the language.
  case Handle(handlers: List[Expr], body: Expr)

  // handler is part of a macro used by chez-callcc and chez-monadic (not chez-lifted)
  case Handler(constructorName: ChezName, operations: List[Operation])
}
export Expr.*

def RawExpr(str: String): chez.Expr = Expr.RawExpr(List(str), Nil)

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
def Call(callee: Expr, args: Expr*): Expr = Call(callee, args.toList)

def Lambda(params: List[ChezName], body: Expr): Lambda = Lambda(params, Block(Nil, Nil, body))

def Function(name: ChezName, params: List[ChezName], body: Expr): Function = Function(name, params, Block(Nil, Nil, body))

def Let(bindings: List[Binding], body: Expr): Expr = Let(bindings, Block(Nil, Nil, body))

def ChezString(chezString: String): Expr = RawExpr(s"\"${chezString}\"")

def Builtin(name: String, args: Expr*): Expr = Call(Variable(ChezName(name)), args.toList)

def curry(lam: chez.Lambda): chez.Lambda = lam.params.foldRight[chez.Lambda](chez.Lambda(Nil, lam.body)) {
  case (p, body) => chez.Lambda(List(p), body)
}

def unit = chez.Expr.RawValue("'()")

implicit def autoVar(n: ChezName): Expr = Variable(n)

def cleanup(expr: Expr): Expr = LetFusion.rewrite(DeadCodeElimination.rewrite(expr)(using ()))(using ())

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
      val bs = bindings.collect {
        case b @ Binding(name, binding) if (fv contains name) || !isInlinable(binding) => rewrite(b)
      }
      Let(bs, transformedBody)
  }

  override def rewrite(block: Block)(using Unit): Block = block match {

    case Block(defs, exprs, result) =>
      val transformedResult = rewrite(result)
      val transformedExprs = exprs.map(rewrite)
      val transformedDefs = defs.map(rewrite)

      val fv = transformedDefs.flatMap(FreeVariables.query) ++ transformedExprs.flatMap(FreeVariables.query) ++ FreeVariables.query(transformedResult)

      val filteredDefs = transformedDefs.filter {
        case Constant(name, expr) => (fv contains name) || !isInlinable(expr)
        case Function(name, params, body) => fv contains name
        case _ => true
      }

      Block(filteredDefs, transformedExprs, transformedResult)
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
  }

  override def query(operation: Operation)(using Unit): Set[ChezName] =
    query(operation.body) -- operation.params.toSet - operation.k

  override def defn(using Unit) = {
    case chez.Function(name, params, body) => query(body) -- params.toSet - name // recursive functions
    case chez.Constant(name, expr) => query(expr)
  }

  override def query(b: Block)(using Unit): Set[ChezName] = b match {
    // defs are potentially recursive!
    case Block(defs, exprs, result) =>

      val boundByDefs = defs.collect {
        case f: chez.Function => f.name
        case f: chez.Constant => f.name
      }.toSet

      val freeInDefs = defs.flatMap(query).toSet

      (freeInDefs ++ query(result) ++ exprs.flatMap(query)) -- boundByDefs
  }
}


object Tree {

  // This solution is between a fine-grained visitor and a untyped and unsafe traversal.
  trait Rewrite[Ctx] extends util.Structural {

    // Hooks to override
    def expr(using C: Ctx): PartialFunction[Expr, Expr] = PartialFunction.empty
    def defn(using C: Ctx): PartialFunction[Def, Def] = PartialFunction.empty

    def rewrite(block: Block)(using Ctx): Block = rewriteStructurally(block)
    def rewrite(binding: Binding)(using Ctx): Binding = rewriteStructurally(binding)
    def rewrite(op: Operation)(using Ctx): Operation = rewriteStructurally(op)

    def rewrite(e: Expr)(using Ctx): Expr = rewriteStructurally(e, expr)
    def rewrite(t: Def)(using C: Ctx): Def = rewriteStructurally(t, defn)

    def rewrite(clause: (Expr, Expr))(using Ctx): (Expr, Expr) = clause match {
      case (c, t) => (rewrite(c), rewrite(t))
    }
  }

  trait Visit[Ctx] extends Query[Ctx, Unit] {
    override def empty = ()
    override def combine = (_, _) => ()
  }

  trait Query[Ctx, Res] extends util.Structural {

    def empty: Res
    def combine: (Res, Res) => Res

    // Hooks to override
    def expr(using Ctx): PartialFunction[Expr, Res] = PartialFunction.empty
    def defn(using Ctx): PartialFunction[Def, Res] = PartialFunction.empty

    /**
     * Hook that can be overridden to perform an action at every node in the tree
     */
    def visit[T](t: T)(visitor: T => Res)(using Ctx): Res = visitor(t)

    inline def structuralQuery[T](el: T, pf: PartialFunction[T, Res] = PartialFunction.empty)(using Ctx): Res = visit(el) { t =>
      if pf.isDefinedAt(el) then pf.apply(el) else queryStructurally(t, empty, combine)
    }

    def query(e: Expr)(using Ctx): Res = structuralQuery(e, expr)
    def query(d: Def)(using Ctx): Res = structuralQuery(d, defn)
    def query(b: Block)(using Ctx): Res = structuralQuery(b)
    def query(b: Binding)(using Ctx): Res = structuralQuery(b)
    def query(e: Operation)(using Ctx): Res = structuralQuery(e)

    def query(clause: (Expr, Expr))(using Ctx): Res = clause match {
      case (p, e) => combine(query(p), query(e))
    }
  }
}
