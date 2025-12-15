package effekt
package core
import effekt.context.Context
import effekt.core.optimizer.Deadcode
import effekt.typer.Typer.checkMain
import effekt.symbols.Symbol.fresh
import effekt.lexer.TokenKind
import effekt.core.Type.instantiate

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
      // println("\n\n\n\nhello")
      // println(PrettyPrinter.format(transformed))
      Some(CoreTransformed(source, tree, mod, transformed))
    }
  }

  def transform(decl: ModuleDecl)(using Context, DeclarationContext): ModuleDecl = decl match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations, externs, definitions map transform, exports)
  }

  def transform(toplevel: Toplevel)(using C: Context, DC: DeclarationContext): Toplevel = toplevel match {
    case Toplevel.Def(id, block) => Toplevel.Def(id, transform(block))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, transform(binding))
  }
  
  def transform(block: Block)(using C: Context, DC: DeclarationContext): Block = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) => block
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      def flattenParam(param: ValueParam): (List[ValueParam], List[(Id, Expr)]) = param match {
        case ValueParam(paramId, tpe @ ValueType.Data(name, targs)) =>
          DC.findData(name) match {
            case Some(Data(_, List(), List(Constructor(ctor, List(), fields)))) =>
              val (flatParams, allBindings, fieldVars) = fields.map { case Field(fieldName, fieldType) =>
                val freshId = Id(fieldName.name)
                val (params, bindings) = flattenParam(ValueParam(freshId, fieldType))
                (params, bindings, ValueVar(freshId, fieldType))
              }.unzip3
              
              val binding = (paramId, Make(tpe, ctor, List(), fieldVars))
              (flatParams.flatten, allBindings.flatten :+ binding)
              
            case _ => (List(param), List())
          }
        case _ => (List(param), List())
      }

      val flattened = vparams.map(flattenParam)
      val (allParams, allBindings) = flattened.unzip
      
      val newBody = allBindings.flatten.foldRight(transform(body)) {
        case ((id, expr), body) => Let(id, expr, body)
      }
    
      Block.BlockLit(tparams, cparams, allParams.flatten, bparams, newBody) 
    case Block.Unbox(pure) => Block.Unbox(transform(pure))
    case Block.New(impl) => block
  }

  def transform(stmt: Stmt)(using C: Context, DC: DeclarationContext): Stmt = stmt match {
    case Stmt.App(callee @ BlockVar(id, BlockType.Function(tparams, cparams, vparamsTypes, bparams, returnTpe), annotatedCapt), targs, vargs, bargs) =>
      def flattenArg(arg: Expr, argType: ValueType): (List[Expr], List[ValueType], List[(Expr, Id, List[ValueParam])]) = argType match {
        case ValueType.Data(name, targs) =>
          DC.findData(name) match {
            case Some(Data(_, List(), List(Constructor(ctor, List(), fields)))) =>
              val fieldInfo = fields.map { case Field(fieldName, fieldType) =>
                val freshId = Id(fieldName.name)
                val freshVar = ValueVar(freshId, fieldType)
                val freshParam = ValueParam(freshId, fieldType)
                
                val (nestedVars, nestedTypes, nestedMatches) = flattenArg(freshVar, fieldType)
                (freshVar, freshParam, fieldType, nestedVars, nestedTypes, nestedMatches)
              }
              
              val vars = fieldInfo.flatMap(_._4)
              val types = fieldInfo.flatMap(_._5)
              val params = fieldInfo.map(_._2)
              val nestedMatches = fieldInfo.flatMap(_._6)
              val thisMatch = (arg, ctor, params)
              
              (vars, types, thisMatch :: nestedMatches)
              
            case _ => (List(arg), List(argType), List())
          }
        case _ => (List(arg), List(argType), List())
      }

      val flattened = (vargs zip vparamsTypes).map { case (arg, tpe) => flattenArg(arg, tpe) }
      val (allArgs, allTypes, allMatches) = flattened.unzip3
      
      val newCalleTpe: BlockType.Function = BlockType.Function(tparams, cparams, allTypes.flatten, bparams, returnTpe)
      val newCallee = BlockVar(id, newCalleTpe, annotatedCapt)
      val innerApp = Stmt.App(newCallee, targs, allArgs.flatten, bargs map transform)
      
      allMatches.flatten.foldRight(innerApp) {
        case ((scrutinee, ctor, params), body) =>
          val resultTpe = instantiate(newCalleTpe, targs, bargs.map(_.capt)).result
          Stmt.Match(scrutinee, resultTpe, List((ctor, BlockLit(List(), List(), params, List(), body))), None)
      }

    // Generic case for all other applications: just recurse into arguments / blocks
    case Stmt.App(callee, targs, vargs, bargs) =>
      Stmt.App(callee, targs, vargs map transform, bargs map transform)

    case Stmt.Def(id, block, rest) =>
      Stmt.Def(id, transform(block), transform(rest))
    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, transform(binding), transform(rest))
    case Stmt.Return(expr) =>
      Stmt.Return(transform(expr))
    case Stmt.Val(id, binding, body) =>
      Stmt.Val(id, transform(binding), transform(body))
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      Stmt.Invoke(transform(callee), method, methodTpe, targs, vargs map transform, bargs map transform)
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Match(scrutinee, tpe, clauses, default) =>
      Stmt.Match(transform(scrutinee), tpe, clauses.map { case (id, BlockLit(tparams, cparams, vparams, bparams, body)) => (id, BlockLit(tparams, cparams, vparams, bparams, transform(body))) }, default map transform)
    case Stmt.ImpureApp(callee, ret, targs, vargs, bargs, retCallee) =>
      Stmt.ImpureApp(callee, ret, targs, vargs map transform, bargs map transform, retCallee)
    case Stmt.Region(BlockLit(tparams, cparams, vparams, bparams, body)) =>
      Stmt.Region(BlockLit(tparams, cparams, vparams, bparams, transform(body)))
    case Stmt.Alloc(id, tpe, init, rest) =>
      Stmt.Alloc(id, tpe, init, transform(rest))
    case Stmt.Var(id, tpe, init, rest) =>
      Stmt.Var(id, tpe, init, transform(rest))
    case Stmt.Get(id, tpe, region, slot, rest) =>
      Stmt.Get(id, tpe, region, slot, transform(rest))
    case Stmt.Put(region, slot, value, rest) =>
      Stmt.Put(region, slot, transform(value), transform(rest))
    case Stmt.Reset(BlockLit(tparams, cparams, vparams, bparams, body)) =>
      Stmt.Reset(BlockLit(tparams, cparams, vparams, bparams, transform(body)))
    case Stmt.Shift(id, k, body) => 
      Stmt.Shift(id, k, transform(body)) 
    case Stmt.Resume(k, value) =>
      Stmt.Resume(k, transform(value))
    case Stmt.Hole(tpe, span) =>
      Stmt.Hole(tpe, span)
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