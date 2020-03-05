package effekt
package core

import effekt.typer.Typer
import effekt.util.control
import effekt.util.control._
import effekt.symbols.Symbol

class LiftInference {

  def run(m: ModuleDecl): ModuleDecl = {
    val toplevel = Context(Map.empty)
    ModuleDecl(m.path, m.imports, transform(m.defs)(given toplevel))
  }

  // TODO what's with recursive blocks?

  def transform(b: Block)(given Context): Block = b match {
    case BlockVar(v) => Context.env.getOrElse(v, b)
    case BlockDef(ps, b) =>
      val ctx = ps.filter { p => p.isInstanceOf[BlockParam] }.foldLeft(Context.current) {
        case (ctx, p) => ctx.bind(p.id, BlockVar(p.id))
      }
      BlockDef(ps, transform(b)(given ctx))
  }

  def transform(s: Stmt)(given Context): Stmt = s match {
    case Def(id, block, rest) =>
      Def(id, transform(block), transform(rest)(given Context.bind(id, BlockVar(id))))
    case Val(id, binding, body) =>
      Val(id, transform(binding), transform(body))
    case Data(id, ctors, rest) =>
      Data(id, ctors, transform(rest))
    case App(b, args) => App(transform(b), args map {
      case e: Expr => e
      case b: Block => transform(b)
    })
    case If(cond, thn, els) => If(cond, transform(thn), transform(els))
    case While(cond, body) => While(transform(cond), transform(body))
    case Ret(e) => Ret(e)
    case Exports(p, e) => Exports(p, e)
    case Include(contents, rest) => Include(contents, rest)
    case Handle(body, clauses) =>
      Handle(transform(body)(given Context.lift), clauses.map { (s, c) => (s, transform(c)) })
    case Match(sc, clauses) =>
      Match(sc, clauses.map { (s, c) => (s, transform(c)) })
  }

  case class Context(env: Map[Symbol, Block]) {
    def bind(s: Symbol, b: Block) = copy(env = env + (s -> b))
    def lift = copy(env = env.map { case (s, b) => (s -> Lift(b)) })
    def current = this
  }
  def Context(given ctx: Context) = ctx

}