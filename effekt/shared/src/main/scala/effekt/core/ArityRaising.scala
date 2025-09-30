package effekt
package core
import effekt.context.Context
import effekt.core.optimizer.Deadcode
import effekt.typer.Typer.checkMain

object ArityRaising extends Phase[CoreTransformed, CoreTransformed] {
  override val phaseName: String = "arity raising"


  override def run(input: CoreTransformed)(using C: Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) => {
      implicit val pctx: DeclarationContext = new DeclarationContext(core.declarations, core.externs)
      Context.module = mod
      val main = C.ensureMainExists(mod)
      val res = Deadcode.remove(main, core)
      val transformed = Context.timed(phaseName, source.name) { transform(res) }
      println(PrettyPrinter.format(transformed))
      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using Context, DeclarationContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations, externs, definitions map transform, exports)
  }

  def transform(toplevel: Toplevel)(using Context, DeclarationContext): Toplevel = toplevel match {
    case Toplevel.Def(id, block) => Toplevel.Def(id, transform(block))
    case Toplevel.Val(id, tpe, binding) => Toplevel.Val(id, tpe, transform(binding))
  }

  def transform(block: Block.BlockLit)(using Context, DeclarationContext): Block.BlockLit = block match {
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      Block.BlockLit(tparams, cparams, vparams, bparams, transform(body))
  }
  
  def transform(block: Block)(using Context, DeclarationContext): Block = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) => block
    case b: Block.BlockLit => transform(b)
    case Block.Unbox(pure) => Block.Unbox(transform(pure))
    case Block.New(impl) => block
  }

  def transform(stmt: Stmt)(using Context, DeclarationContext): Stmt = stmt match {
    case Stmt.Def(id, block, rest) =>
      Stmt.Def(id, transform(block), transform(rest))
    case Stmt.Let(id, tpe, binding, rest) =>
      Stmt.Let(id, tpe, transform(binding), transform(rest))
    case Stmt.Return(expr) =>
      Stmt.Return(transform(expr))
    case Stmt.Val(id, tpe, binding, body) =>
      Stmt.Val(id, tpe, transform(binding), transform(body))
    case Stmt.App(callee, targs, vargs, bargs) =>
      Stmt.App(transform(callee), targs, vargs map transform, bargs map transform) // possible args
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      Stmt.Invoke(transform(callee), method, methodTpe, targs, vargs map transform, bargs map transform) // possible args
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(transform(scrutinee), clauses.map { case (id, clause) => (id, transform(clause)) }, default map transform)
    case _ => stmt
  }

  def transform(expr: Expr)(using Context, DeclarationContext): Expr = expr match {
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(b, targs, vargs map transform, bargs map transform) // possible args
    case pure: Pure =>
      transform(pure)
  }

  def transform(pure: Pure)(using Context, DeclarationContext): Pure = pure match {
    case Pure.ValueVar(id, annotatedType) => pure
    case Pure.Literal(value, annotatedType) => pure
    case Pure.Box(b, annotatedCapture) => pure
    case Pure.PureApp(b, targs, vargs) =>
      Pure.PureApp(b, targs, vargs map transform) // possible args
    case Pure.Make(data, tag, targs, vargs) =>
      Pure.Make(data, tag, targs, vargs map transform) // possibe args
      
  }

  def transform(valueType: ValueType.Data)(using Context, DeclarationContext): ValueType.Data = valueType match {
    case ValueType.Data(symbol, targs) => valueType // trainsform
  }

}