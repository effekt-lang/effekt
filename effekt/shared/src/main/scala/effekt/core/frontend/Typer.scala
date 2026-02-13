package effekt
package core.frontend

import core.frontend
import core.Type.*
import effekt.source.Span


object typer {

  case class Context(
    terms: Map[frontend.Id, core.Id],
    types: Map[frontend.Id, core.Id],
    values: Map[core.Id, core.ValueType],
    blocks: Map[core.Id, (core.BlockType, core.Captures)]
  )
  object Context {
    def empty = Context(Map.empty, Map.empty, Map.empty, Map.empty)
  }

  case class Typed[E, T](term: E, tpe: T, captures: core.Captures)

  def typecheck(m: frontend.ModuleDecl): core.ModuleDecl = ???

  def typecheck(stmt: frontend.Stmt)(using C: Context): Typed[core.Stmt, core.ValueType] = stmt match {
    case frontend.Stmt.Let(id, binding, body) =>
      val fresh = core.Id(id)
      val Typed(binding2, bindingTpe, bindingCapt) = typecheck(binding)
      val Typed(body2, bodyTpe, bodyCapt) = typecheck(body)(using C.bindValue(id, fresh, bindingTpe))
      Typed(core.Let(fresh, bindingTpe, binding2, body2), bodyTpe, bodyCapt ++ bindingCapt)

    // TODO what about recursive functions and their captures??? They need to be annotated
    case frontend.Stmt.Def(id, block, body) =>
      val fresh = core.Id(id)
      val Typed(block2, blockTpe, blockCapt) = typecheck(block)
      val Typed(body2, bodyTpe, bodyCapt) = typecheck(body)(using C.bindBlock(id, fresh, blockTpe, blockCapt))
      Typed(core.Def(fresh, block2, body2), bodyTpe, blockCapt ++ bodyCapt) // ??? even if body doesn't use block?

    case frontend.Stmt.Val(id, binding, body) =>
      val fresh = core.Id(id)
      val Typed(binding2, bindingTpe, bindingCapt) = typecheck(binding)
      val Typed(body2, bodyTpe, bodyCapt) = typecheck(body)(using C.bindValue(id, fresh, bindingTpe))
      Typed(core.Val(fresh, bindingTpe, binding2, body2), bodyTpe, bodyCapt ++ bindingCapt)

    case frontend.Stmt.App(callee, targs, vargs, bargs) => ???

    case frontend.Stmt.Invoke(callee, method, targs, vargs, bargs) => ???

    case frontend.Stmt.Return(expr) =>
      val Typed(expr2, exprTpe, exprCapt) = typecheck(expr)
      Typed(core.Return(expr2), exprTpe, exprCapt)

    case frontend.Stmt.Hole() =>
      // TODO: Supply a reasonable span here
      Typed(core.Hole(Span.missing), TBottom, Set.empty)
  }

  def typecheck(block: frontend.Block)(using Context): Typed[core.Block, core.BlockType] = ???

  def typecheck(expr: frontend.Pure)(using Context): Typed[core.Expr, core.ValueType] = expr match {
    case frontend.Pure.ValueVar(id) =>
      val tpe = valueTypeOf(id)
      Typed(core.Expr.ValueVar(resolveTerm(id), tpe), tpe, Set.empty)
    case frontend.Pure.Literal(value, annotatedType) =>
      Typed(core.Expr.Literal(value, annotatedType), annotatedType, Set.empty)
    case frontend.Pure.Make(tag, targs, vargs) => ???
    case frontend.Pure.PureApp(b, targs, vargs) => ???
    case frontend.Pure.Box(block) =>
      val Typed(block2, blockTpe, blockCapt) = typecheck(block)
      Typed(core.Box(block2, blockCapt), core.ValueType.Boxed(blockTpe, blockCapt), Set.empty)
  }

  def typecheck(expr: frontend.Expr)(using Context): Typed[core.Expr, core.ValueType] = expr match {
    case p: Pure => typecheck(p)
    case frontend.DirectApp(b, targs, vargs, bargs) => ???
  }


  // Helpers
  // -------

  def resolveTerm(id: frontend.Id)(using C: Context): core.Id =
    C.terms.getOrElse(id, sys error s"Variable not bound ${id}.")

  def valueTypeOf(id: frontend.Id)(using C: Context): core.ValueType =
    valueTypeOf(resolveTerm(id))

  def valueTypeOf(id: core.Id)(using C: Context): core.ValueType =
    C.values.getOrElse(id, sys error s"Cannot find type for ${id}")

  extension (C: Context) {
    def bindValue(id: frontend.Id, sym: core.Id, tpe: core.ValueType): Context =
      C.copy(terms = C.terms.updated(id, sym), values = C.values.updated(sym, tpe))
    def bindBlock(id: frontend.Id, sym: core.Id, tpe: core.BlockType, capt: core.Captures): Context =
      C.copy(terms = C.terms.updated(id, sym), blocks = C.blocks.updated(sym, (tpe, capt)))
  }
}

