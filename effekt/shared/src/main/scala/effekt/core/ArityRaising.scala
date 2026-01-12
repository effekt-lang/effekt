package effekt
package core
import effekt.context.Context
import effekt.core.optimizer.Deadcode
import effekt.typer.Typer.checkMain
import effekt.symbols.Symbol.fresh
import effekt.lexer.TokenKind
import effekt.core.Type.instantiate
import effekt.generator.llvm.Transformer.BlockContext
import effekt.machine.Transformer.BlocksParamsContext

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
    case Toplevel.Def(id, block) => Toplevel.Def(id, transform(block)(using C, DC, Set.empty))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, transform(binding)(using C, DC, Set.empty))
  }
  
  def transform(block: Block)(using C: Context, DC: DeclarationContext, bargs: Set[Id]): Block = block match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) => block
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      val newBargs = bargs ++ bparams.map(_.id)
      def flattenParam(param: ValueParam): (List[ValueParam], List[(Id, Expr)]) = param match {
        case ValueParam(paramId, tpe @ ValueType.Data(name, targs)) =>
          DC.findData(name) match {
            case Some(Data(_, List(), List(Constructor(ctor, List(), fields)))) =>
              val (flatParams, allBindings, fieldVars) = fields.map { case Field(fieldName, fieldType) =>
                val freshId = Id(fieldName)
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
      
      val newBody = allBindings.flatten.foldRight(transform(body)(using C, DC, newBargs)) {
        case ((id, expr), body) => Let(id, expr, body)
      }
    
      Block.BlockLit(tparams, cparams, allParams.flatten, bparams, newBody) 
    case Block.Unbox(pure) => Block.Unbox(transform(pure))
    case Block.New(Implementation(interface, operations)) => 
      Block.New(Implementation(interface, operations.map {
        case Operation(name, tparams, cparams, vparams, bparams, body) =>
          val opBargs = bargs ++ bparams.map(_.id)
          Operation(name, tparams, cparams, vparams, bparams, transform(body)(using C, DC, opBargs))
       }))
  }
// Helper to check if a type needs flattening
      def needsFlattening(tpe: ValueType)(using DC:DeclarationContext): Boolean = tpe match {
        case ValueType.Data(name, _) =>
          DC.findData(name) match {
            case Some(Data(_, List(), List(Constructor(_, List(), _)))) => true
            case _ => false
          }
        case _ => false
      }

  def transform(stmt: Stmt)(using C: Context, DC: DeclarationContext, bargs: Set[Id]): Stmt = stmt match {
    case Stmt.App(callee @ BlockVar(id, BlockType.Function(tparams, cparams, vparamsTypes, bparamTypes, returnTpe), annotatedCapt), targs, vargs, appBargs)
        if !bargs.contains(id) =>
      def flattenArg(arg: Expr, argType: ValueType): (List[Expr], List[ValueType], List[(Expr, Id, List[ValueParam])]) = argType match {
        case ValueType.Data(name, targs) =>
          DC.findData(name) match {
            case Some(Data(_, List(), List(Constructor(ctor, List(), fields)))) =>
              val fieldInfo = fields.map { case Field(fieldName, fieldType) =>
                val freshId = Id(fieldName)
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

      

      val transformedBargs = appBargs.map { barg =>
        barg match {
          // This handles: 
          // val res = myList.map {myFunc} 
          // by making it:
          // val res = myList.map {t => myFunc(t)}
          // but only if the arity of myFunc changes
          case BlockVar(id, annotatedTpe, annotatedCapt) => 
            annotatedTpe match {
              case BlockType.Function(tparams, cparams, vparams, bparamTpes, result) 
                  if vparams.exists(needsFlattening) =>
                val values = vparams.map { tpe =>
                  val freshId = Id("x")
                  (ValueParam(freshId, tpe), ValueVar(freshId, tpe))
                }
                val blocks = bparamTpes.zip(cparams).map { case (tpe, capt) =>
                  val freshId = Id("f")
                  (BlockParam(freshId, tpe, Set(capt)), BlockVar(freshId, tpe, Set(capt)))
                }
 
                // Don't transform the call - the BlockVar keeps its original signature
                val call = Stmt.App(barg, List(), values.map(_._2), blocks.map(_._2))
                
                val wrapperBargs = bargs ++ blocks.map(_._1.id)
                BlockLit(tparams, cparams, values.map(_._1), blocks.map(_._1), transform(call)(using C, DC, wrapperBargs))

              case _ => transform(barg)
            }

          case BlockLit(btparams, bcparams, bvparams, bbparams, body) =>
            // Keep the signature unchanged
            val litBargs = bargs ++ bbparams.map(_.id)
            val transformedBody = transform(body)(using C, DC, litBargs)
            BlockLit(btparams, bcparams, bvparams, bbparams, transformedBody)
          
          case _ => 
            transform(barg)
        }
      }
      
      val newCalleTpe: BlockType.Function = BlockType.Function(tparams, cparams, allTypes.flatten, bparamTypes, returnTpe)
      val newCallee = BlockVar(id, newCalleTpe, annotatedCapt)
      val innerApp = Stmt.App(newCallee, targs, allArgs.flatten, transformedBargs)
      
      allMatches.flatten.foldRight(innerApp) {
        case ((scrutinee, ctor, params), body) =>
          val resultTpe = instantiate(newCalleTpe, targs, appBargs.map(_.capt)).result
          Stmt.Match(scrutinee, resultTpe, List((ctor, BlockLit(List(), List(), params, List(), body))), None)
      }

    case Stmt.App(callee, targs, vargs, appBargs) =>
      Stmt.App(callee, targs, vargs map transform, appBargs map transform)
    case Stmt.Def(id, block, rest) =>
      Stmt.Def(id, transform(block), transform(rest))
    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, transform(binding), transform(rest))
    case Stmt.Return(expr) =>
      Stmt.Return(transform(expr))
    case Stmt.Val(id, binding, body) =>
      Stmt.Val(id, transform(binding), transform(body))
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, invokeBargs) =>
      Stmt.Invoke(transform(callee), method, methodTpe, targs, vargs map transform, invokeBargs map transform)
    case Stmt.If(cond, thn, els) =>
      Stmt.If(transform(cond), transform(thn), transform(els))
    case Stmt.Match(scrutinee, tpe, clauses, default) =>
      Stmt.Match(transform(scrutinee), tpe, clauses.map { case (id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
        val clauseBargs = bargs ++ bparams.map(_.id)
        (id, BlockLit(tparams, cparams, vparams, bparams, transform(body)(using C, DC, clauseBargs)))
      }, default map transform)
    case Stmt.ImpureApp(id, callee, targs, vargs, impureBargs, body) =>
      Stmt.ImpureApp(id, callee, targs, vargs map transform, impureBargs map transform, transform(body))
    case Stmt.Region(BlockLit(tparams, cparams, vparams, bparams, body)) =>
      val regionBargs = bargs ++ bparams.map(_.id)
      Stmt.Region(BlockLit(tparams, cparams, vparams, bparams, transform(body)(using C, DC, regionBargs)))
    case Stmt.Alloc(id, init, region, body) =>
      Stmt.Alloc(id, transform(init), region, transform(body))
    case Stmt.Var(ref, init, capture, body) =>
      Stmt.Var(ref, transform(init), capture, transform(body))
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) =>
      Stmt.Get(id, annotatedTpe, ref, annotatedCapt, transform(body))
    case Stmt.Put(ref, annotatedCapt, value, body) =>
      Stmt.Put(ref, annotatedCapt, transform(value), transform(body))
    case Stmt.Reset(BlockLit(tparams, cparams, vparams, bparams, body)) =>
      val resetBargs = bargs ++ bparams.map(_.id)
      Stmt.Reset(BlockLit(tparams, cparams, vparams, bparams, transform(body)(using C, DC, resetBargs)))
    case Stmt.Shift(prompt, k, body) =>
      // k is a continuation (block param), so add it to bargs
      val shiftBargs = bargs + k.id
      Stmt.Shift(prompt, k, transform(body)(using C, DC, shiftBargs))
    case Stmt.Resume(k, body) =>
      Stmt.Resume(k, transform(body))
    case Stmt.Hole(tpe, span) =>
      Stmt.Hole(tpe, span)
  }

  def transform(pure: Expr)(using C: Context, DC: DeclarationContext, bargs: Set[Id]): Expr = pure match {
    case Expr.ValueVar(id, annotatedType) => pure
    case Expr.Literal(value, annotatedType) => pure
    case Expr.Box(barg @ BlockVar(id, annotatedTpe, annotatedCapt), annotatedCapture) => 
       annotatedTpe match {
          case BlockType.Function(tparams, cparams, vparams, bparamTpes, result) 
              if vparams.exists(needsFlattening) =>
            val values = vparams.map { tpe =>
              val freshId = Id("x")
              (ValueParam(freshId, tpe), ValueVar(freshId, tpe))
            }
            val blocks = bparamTpes.zip(cparams).map { case (tpe, capt) =>
              val freshId = Id("f")
              (BlockParam(freshId, tpe, Set(capt)), BlockVar(freshId, tpe, Set(capt)))
            }

            // Don't transform the call - the BlockVar keeps its original signature
            val call = Stmt.App(barg, List(), values.map(_._2), blocks.map(_._2))
            
            val wrapperBargs = bargs ++ blocks.map(_._1.id)
            Expr.Box(BlockLit(tparams, cparams, values.map(_._1), blocks.map(_._1), transform(call)(using C, DC, wrapperBargs)), annotatedCapture)

          case _ => Expr.Box(transform(barg), annotatedCapture)
       }

    case Expr.Box(b, annotatedCapture) => Expr.Box(transform(b), annotatedCapture)
    case Expr.PureApp(b, targs, vargs) =>
      Expr.PureApp(b, targs, vargs map transform) 
    case Expr.Make(data, tag, targs, vargs) =>
      Expr.Make(data, tag, targs, vargs map transform) 
  }

  def transform(valueType: ValueType.Data)(using C: Context, DC: DeclarationContext): ValueType.Data = valueType match {
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
