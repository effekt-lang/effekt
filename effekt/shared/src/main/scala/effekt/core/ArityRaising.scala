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
      // println("Before")
      // println(PrettyPrinter.format(res))
      val transformed = Context.timed(phaseName, source.name) { transform(res) }
      // println(PrettyPrinter.format(transformed))
      // println("\n\n\nparts\n\n")
      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using Context, DeclarationContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations, externs, definitions map transform, exports)
  }

  def transform(toplevel: Toplevel)(using C: Context, DC: DeclarationContext): Toplevel = toplevel match {
    case Toplevel.Def(id, BlockLit(tparams, cparams, List(ValueParam(param, ValueType.Data(name, targs))), bparams, body)) => 
      DC.findData(name) match {
        case Some(Data(_, List(), List(Constructor(test, List(), List(Field(x, tpe1), Field(y, tpe2)))))) => 
          //println(test)
          //new names
          val p1 = Id(x.name)
          val p2 = Id(y.name)
          val vparams = List(ValueParam(p1, tpe1), ValueParam(p2, tpe2))
          val transformedBody = Let(param, ValueType.Data(name, targs), 
          Make(ValueType.Data(name, targs), test, List(), List(ValueVar(p1, tpe1), ValueVar(p2, tpe2))), transform(body))
          Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, transformedBody)) 

        case _ => 
          toplevel
      }
      
    case Toplevel.Def(id, block) => 
      val res = Toplevel.Def(id, transform(block))
      //println("\n\nres:")
      //println(doIndentation(res.toString))
      res
 
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

  def transform(stmt: Stmt)(using C: Context, DC: DeclarationContext): Stmt = stmt match {
    case Stmt.Def(id, block, rest) =>
     Stmt.Def(id, transform(block), transform(rest))
    case Stmt.Let(id, tpe, binding, rest) =>
      Stmt.Let(id, tpe, transform(binding), transform(rest))
    case Stmt.Return(expr) =>
      Stmt.Return(transform(expr))
    case Stmt.Val(id, tpe, binding, body) =>
      Stmt.Val(id, tpe, transform(binding), transform(body))
    case Stmt.App(callee, targs, vargs, bargs) =>
      callee match {
        case BlockVar(id, BlockType.Function(List(), List(), List(ValueType.Data(name, List())), List(), returnTpe), annotatedCapt) => 
          DC.findData(name) match {
            case Some(Data(_, List(), List(Constructor(test, List(), List(Field(x, tpe1), Field(y, tpe2)))))) => 
              val p1 = Id(x.name)
              val p2 = Id(y.name)
              val transformedVargs = List(ValueVar(p1, tpe1), ValueVar(p2, tpe2))
              val res = Stmt.App(BlockVar(id, BlockType.Function(List(), List(), List(tpe1, tpe2), List(), returnTpe), annotatedCapt), targs, transformedVargs, bargs)
              val latermatch: BlockLit = Block.BlockLit(List(), List(), List(ValueParam(p1,tpe1), ValueParam(p2, tpe2)), List(), res)
              Stmt.Match(vargs.head, List((test,  latermatch)), None)
            
            case _ => 
              stmt
          }

        case _ => stmt
      }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      Stmt.Invoke(transform(callee), method, methodTpe, targs, vargs map transform, bargs map transform)
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
      Expr.PureApp(b, targs, vargs map transform) 
    case Expr.Make(data, tag, targs, vargs) =>
      Expr.Make(data, tag, targs, vargs map transform) 
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
          // Look ahead to see if it's a short List(...) with no commas
          val closing = input.indexOf(')', i)
          val inside = if (closing > i) input.substring(i + 1, closing) else ""
          if (inside.contains(',') || inside.contains('(') || inside.contains(')')) {
            sb.append("(\n")
            indent += 1
            sb.append("  " * indent)
          } else {
            sb.append('(')
          }

        case ')' =>
          val prev = if (i > 0) input(i - 1) else ' '
          if (prev == '(' || prev.isLetterOrDigit) {
            sb.append(')')
          } else {
            sb.append("\n")
            indent -= 1
            sb.append("  " * indent)
            sb.append(")")
          }

        case ',' =>
          sb.append(",\n")
          sb.append("  " * indent)

        case c if c.isWhitespace =>
          // skip

        case c =>
          sb.append(c)
      }
      i += 1
    }

    sb.toString
  }

}