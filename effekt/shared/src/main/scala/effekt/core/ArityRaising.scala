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
      println("\n\n\nparts\n\n")
      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using Context, DeclarationContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations, externs, definitions map transform, exports)
  }

  def transform(toplevel: Toplevel)(using C: Context, DC: DeclarationContext): Toplevel = toplevel match {
    case Toplevel.Def(id, BlockLit(tparams, cparams, List(ValueParam(param, ValueType.Data(name, targs))), bparams, body)) => 
      println("### id : ")
      println(param)
      println(name)
      println(targs)
      DC.findData(name) match {
        case Some(Data(_, List(), List(Constructor(test, List(Field(x, tpe1)), List(Field(y, tpe2)))))) => 
          println(test)
          val vparams = List(ValueParam(x, tpe1), ValueParam(y, tpe2))
          val transformedBody = Let(param, ValueType.Data(name, targs), 
          Make(ValueType.Data(name, targs), test, List(), List(ValueVar(x, tpe1), ValueVar(y, tpe2))), transform(body))
          Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, transformedBody)) 

        case _ => toplevel
      }
      
    case Toplevel.Def(id, block) => 
      println("\n\nid:")
      println(doIndentation(id.toString))
      println("block:")
      println(doIndentation(block.toString))
      Toplevel.Def(id, transform(block))
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
      // here too
      Stmt.Def(id, transform(block), transform(rest))
    case Stmt.Let(id, tpe, binding, rest) =>
      Stmt.Let(id, tpe, transform(binding), transform(rest))
    case Stmt.Return(expr) =>
      Stmt.Return(transform(expr))
    case Stmt.Val(id, tpe, binding, body) =>
      Stmt.Val(id, tpe, transform(binding), transform(body))
    case Stmt.App(callee, targs, vargs, bargs) =>
      println(doIndentation(stmt.toString()))
      print(doIndentation(vargs.head.tpe.toString()))
      Stmt.App(transform(callee), targs, vargs map transform, bargs map transform) // possible args
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      Stmt.Invoke(transform(callee), method, methodTpe, targs, vargs map transform, bargs map transform) // possible args
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(transform(scrutinee), clauses.map { case (id, clause) => (id, transform(clause)) }, default map transform)
    case _ => stmt
  }

  def transform(pure: Expr)(using Context, DeclarationContext): Expr = pure match {
    case Expr.ValueVar(id, annotatedType) => pure
    case Expr.Literal(value, annotatedType) => pure
    case Expr.Box(b, annotatedCapture) => pure
    case Expr.PureApp(b, targs, vargs) =>
      Expr.PureApp(b, targs, vargs map transform) // possible args
    case Expr.Make(data, tag, targs, vargs) =>
      Expr.Make(data, tag, targs, vargs map transform) // possibe args
  }

  def transform(valueType: ValueType.Data)(using Context, DeclarationContext): ValueType.Data = valueType match {
    case ValueType.Data(symbol, targs) => valueType // trainsform
  }


  def doIndentation(input: String): String = {
    val sb = new StringBuilder
    var indent = 0
    var i = 0

    while (i < input.length) {
      input(i) match {
        case '(' =>
          sb.append("(\n")
          indent += 1
          sb.append("  " * indent)
        case ')' =>
          sb.append("\n")
          indent -= 1
          sb.append("  " * indent)
          sb.append(")")
        case ',' =>
          sb.append(",\n")
          sb.append("  " * indent)
        case c if c.isWhitespace =>
          // skip extra whitespace
        case c =>
          sb.append(c)
      }
      i += 1
    }
    sb.toString
  }

}