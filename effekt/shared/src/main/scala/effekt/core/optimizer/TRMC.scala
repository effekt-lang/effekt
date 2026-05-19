package effekt.core.optimizer

import effekt.{Phase, PhaseResult, core, symbols}
import effekt.PhaseResult.{CoreTransformed, Typechecked}
import effekt.context.Context
import effekt.core.Stmt.*
import effekt.core.{Toplevel, *}
import effekt.core.Block.*
import effekt.core.BlockType.*
import effekt.core.optimizer.TRMC.TransformContext
import effekt.symbols.{TermSymbol, TypeSymbol, builtins}
import effekt.util.messages.ErrorReporter

object TRMC extends Phase[CoreTransformed, CoreTransformed]{
  val phaseName: String = "trmc"

  case class ImpossibleStateError(message: String) extends RuntimeException(message: String)

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    val CoreTransformed(source, tree, mod, modDec) = input
    println(effekt.util.PrettyPrinter.format(modDec).layout)
    val DC = DeclarationContext(modDec.declarations, modDec.externs)
    
    var transformedFunctions: List[Toplevel] = List()
    
    var functionLinks: Map[Id, Id] = Map.empty
    
    object transform extends Tree.Rewrite {
      override def toplevel: PartialFunction[Toplevel, Toplevel] = {
        case Toplevel.Def(id, block) =>
          println("in Toplevel")
          println(id.name.name)
          val outputFunId = Id(id.name.name + "_trmc")
          if (id.name.name == "simpleTRMC") {
            transformedFunctions = transformedFunctions.appended(trmc(id, block, outputFunId, DC))
            functionLinks = functionLinks+(id -> outputFunId)
          }
          Toplevel.Def(id, block)
      }

//      override def stmt: PartialFunction[Stmt, Stmt] = {
//        case Def(id, block, body) if id.name.name == "simpleTRMC" =>
//          trmc(id, block, body)
//      }
    }
    
    object rewriteOtherCalls extends Tree.Rewrite {
      override def toplevel: PartialFunction[Toplevel, Toplevel] = {
        case Toplevel.Def(id, block) => block match {
          case Block.BlockVar(id, annotatedTpe, annotatedCapt) => ???
          case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
            Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, rewriteCalls(body, id, functionLinks, DC))) 
          case Block.Unbox(pure) => ???
          case Block.New(impl) => ???
        }
      }
    }

    val transformed = Context.timed(phaseName, source.name) {
      transform.rewrite(modDec)
      val m = rewriteOtherCalls.rewrite(modDec)
      m match {
        case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
          ModuleDecl(path, includes, declarations, externs, definitions ++ transformedFunctions, exports)
      }
    }

    Some(CoreTransformed(source, tree, mod, transformed))
  }
  
  def freeInStmt(id:Id, stmt: Stmt): Boolean = stmt.free.freeIds.contains(id)
  def freeInExpr(id:Id, expr: Expr): Boolean = expr.free.freeIds.contains(id)
  def freeInBlock(id:Id, block: Block): Boolean = block.free.freeIds.contains(id)
  
  def rewriteCalls(stmt: Stmt, transformedfun: Id, functionLinks: Map[Id, Id], DC: DeclarationContext)(using Context): Stmt = stmt match {
    case Stmt.Def(id, block, body) => ???
    case Stmt.Let(id, binding, body) => Stmt.Let(id, binding, rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => Stmt.ImpureApp(id, callee, targs, vargs, bargs, rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.Return(expr) => Stmt.Return(expr)
    case Stmt.Val(id, binding, body) => Stmt.Val(id, rewriteCalls(binding, transformedfun, functionLinks, DC), rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.App(callee, targs, vargs, bargs) => callee match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
        val outputfun = functionLinks.get(id)
        if(id != transformedfun && outputfun.isDefined){
//          val ctxDecl: Declaration = DC.declarations.find(_.id.name.name == "HoleContext").getOrElse { //TODO: refactor duplicate code
//            Context.panic(s"No declaration found for HoleContext.")
//          }
          val outerContextTpe = ValueType.Data(builtins.ContextSymbol, List(stmt.tpe, stmt.tpe)) //TODO: parameters, if unequal
          annotatedTpe match {
            case Function(tparams, cparams, vparams, bparams, result) =>
              Stmt.App(
                Block.BlockVar(
                  outputfun.get,
                  Function(tparams, cparams, vparams.appended(outerContextTpe), bparams, result), annotatedCapt),//TODO:capt? 
                targs,
                vargs.appended(PureApp(blockVarFromExternDef("ctx_emptyContext", DC), List(stmt.tpe),Nil)),
                bargs)
            case _ => throw ImpossibleStateError("in an App() Statement a Function should be called")
          }
        }else{
          stmt
        }
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => ???
      case Block.Unbox(pure) => ???
      case Block.New(impl) => ???
    }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???
    case Stmt.If(cond, thn, els) => Stmt.If(cond, rewriteCalls(thn, transformedfun, functionLinks, DC), rewriteCalls(els, transformedfun, functionLinks, DC))
    case Stmt.Match(scrutinee, annotatedTpe, clauses, default) => 
      Stmt.Match(
        scrutinee,
        annotatedTpe,
        clauses.map((id, blockLit) => blockLit match {
          case BlockLit(tparams, cparams, vparams, bparams, body) => (id, BlockLit(tparams, cparams, vparams, bparams, rewriteCalls(body, transformedfun, functionLinks, DC)))
        }),
        default match {
          case Some(value) => Some(rewriteCalls(value, transformedfun, functionLinks, DC))
          case None => None
        }
      )
    case Stmt.Region(body) => ???
    case Stmt.Alloc(id, init, region, body) => ???
    case Stmt.Var(ref, init, capture, body) => ???
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => ???
    case Stmt.Put(ref, annotatedCapt, value, body) => ???
    case Stmt.Reset(body) => ???
    case Stmt.Shift(prompt, k, body) => ???
    case Stmt.Resume(k, body) => ???
    case Stmt.Hole(annotatedTpe, span) => ???
  }


  def trmc(id: Id, block: Block, outputfun: Id, DC: DeclarationContext)(using Context): Toplevel = block match {
    case effekt.core.Block.BlockVar(id, annotatedTpe, annotatedCapt) => ???
    case effekt.core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
//      val ctxDecl: Declaration = DC.declarations.find(_.id.name.name == "HoleContext").getOrElse {
//        Context.panic(s"No declaration found for HoleContext.")
//      }
      val outerContextTpe = ValueType.Data(builtins.ContextSymbol, List(body.tpe, body.tpe)) //TODO: parameters, if unequal
      val ctxId = Id("ctx")
      Toplevel.Def(outputfun, BlockLit(tparams, cparams, vparams.appended(ValueParam(ctxId, outerContextTpe)), bparams,
        trmc(body, id, outputfun, TransformContext.Outer(ctxId), outerContextTpe, DC)))
    case effekt.core.Block.Unbox(pure) => ???
    case effekt.core.Block.New(impl) => ???
  }
  enum TransformContext {
    case Outer(id: Id)
    case Val(id: Id, body: Stmt, next: TransformContext)
  }

  enum TailContext {
    case Empty
    case Outer(id: Id)
    case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], before: List[Expr], after: List[Expr])
    case Compose(first: TailContext, second: TailContext) //order is currently: second into first
  }

  def split(context: TransformContext, DC: DeclarationContext)(using Context): (TailContext, Option[TransformContext]) ={
    val listDecl: Declaration = DC.declarations.find(_.id.name.name == "List").getOrElse {
      Context.panic(s"No declaration found for List.")
    }
    val consId: Id = listDecl match{
      case Data(id, tparams, List(Constructor(nilId, niltparams, nilFields),Constructor(consId, constparams, consfields))) => consId //TODO: could this be less ugly?
      case _ => throw ImpossibleStateError("should have failed above while finding List or the pattern is incorrect")
    }
    context match {
      case TransformContext.Outer(id) => (TailContext.Outer(id), None)
      case TransformContext.Val(id, Stmt.Return(Expr.Make(data, tag, targs, head :: Expr.ValueVar(id2, tpe) :: Nil)), next) //TODO: leads always to a compose TailContext, even if its only one Val
        if tag == consId && id == id2 =>
        val (init, rest) = split(next, DC)
        (TailContext.Compose(init, TailContext.Make(data, tag, targs, List(head), Nil)), rest)
      case TransformContext.Val(id, Stmt.Let(id2,Expr.Make(data, tag, targs, head :: Expr.ValueVar(id3, tpe) :: Nil), 
          Stmt.Return(Expr.ValueVar(id4, tpe2))), next) //TODO: what about longer let-chains?
        if tag == consId && id == id3 && id2 == id4 =>
        val (init, rest) = split(next, DC)
        (TailContext.Compose(init, TailContext.Make(data, tag, targs, List(head), Nil)), rest)
      case _ => (TailContext.Empty, Some(context))
    }
  }
  
  
  def trmc(stmt: Stmt, inputfun: Id, outputfun: Id, context: TransformContext, outerContextTpe: ValueType, DC: DeclarationContext)(using Context): Stmt = stmt match {
    case Stmt.Def(id, block, body) => ???
    case Stmt.Let(id, binding, body) => Stmt.Let(id, binding, trmc(body,inputfun, outputfun, context, outerContextTpe, DC))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???
    case Stmt.Return(expr) => reify(stmt, context, inputfun, outputfun, outerContextTpe, DC) //probably works every time, original function must still exit in case inputfun is free in expr
    case Stmt.Val(id, binding, body) => trmc(binding, inputfun, outputfun, TransformContext.Val(id,body,context), outerContextTpe, DC)
    case Stmt.App(callee, targs, vargs, bargs) => callee match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
        if (id == inputfun) {
          val (init, rest) = split(context, DC)
          annotatedTpe match {
            case Function(tparams, cparams, vparams, bparams, result) =>
              val inner = Stmt.App(
                Block.BlockVar(
                  outputfun,
                  Function(tparams, cparams, vparams.appended(outerContextTpe), bparams, result), annotatedCapt),//TODO:capt? 
                targs,
                vargs.appended(innerReify(init, outerContextTpe, stmt.tpe, DC)), //is input.tpe always the correct resType?
                bargs)
              rest match {
                case Some(rest) => reify(inner, rest, inputfun, outputfun, outerContextTpe, DC)
                case None => inner
              }
            case _ => throw ImpossibleStateError("in an App() Statement a Function should be called")
          }
        }else{
          reify(stmt, context, inputfun, outputfun, outerContextTpe, DC)
        }
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => ???
      case Block.Unbox(pure) => ???
      case Block.New(impl) => ???
    }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???
    case Stmt.If(cond, thn, els) =>
      Stmt.If(cond, //same caveat as Return()
        trmc(thn, inputfun, outputfun, context, outerContextTpe, DC),
        trmc(els, inputfun, outputfun, context, outerContextTpe, DC))
    case Stmt.Match(scrutinee, annotatedTpe, clauses, default) => ???
    case Stmt.Region(body) => ???
    case Stmt.Alloc(id, init, region, body) => ???
    case Stmt.Var(ref, init, capture, body) => ???
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => ???
    case Stmt.Put(ref, annotatedCapt, value, body) => ???
    case Stmt.Reset(body) => ???
    case Stmt.Shift(prompt, k, body) => ???
    case Stmt.Resume(k, body) => ???
    case Stmt.Hole(annotatedTpe, span) => ???
  }


  def reify(stmt: Stmt, context: TransformContext, inputfun: Id, outputfun: Id, outerContextTpe: ValueType, DC: DeclarationContext)(using Context) : Stmt = context match {
    case TransformContext.Outer(id) =>
      val tmpId = Id("tmp")
      Stmt.Val(
        tmpId, 
        stmt, 
        Stmt.Return(
          PureApp(
            blockVarFromExternDef("ctx_applyContext", DC), 
            Nil, 
            List(ValueVar(id, outerContextTpe), ValueVar(tmpId, stmt.tpe))))) //TODO: fix ctxType,tmpType?
    case TransformContext.Val(id, body, next) =>
      Stmt.Val(id, stmt, trmc(body, inputfun, outputfun, next, outerContextTpe, DC))
  }

  def innerReify(context: TailContext, outerContextTpe: ValueType, resType: ValueType, DC: DeclarationContext)(using Context): Expr = context match {
    case TailContext.Empty => PureApp(blockVarFromExternDef("ctx_emptyContext", DC),List(resType),Nil) 
    case TailContext.Outer(id) => Expr.ValueVar(id,outerContextTpe)
    case TailContext.Make(data, tag, targs, before, after) => MakeContext(data, tag, targs, before, after)
    case TailContext.Compose(first, second) => 
      PureApp(
        blockVarFromExternDef("ctx_composeContext", DC), 
        List(resType, resType, resType), 
        List(innerReify(first, outerContextTpe, resType, DC), innerReify(second, outerContextTpe, resType, DC))) //TODO: targs if general
  }
  
  def blockVarFromExternDef(name: String, DC: DeclarationContext)(using Context) : Block.BlockVar = {
    DC.getUniqueExternDef(name) match {
      case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => 
        BlockVar(id, Function(tparams, cparams, vparams.map(getType), bparams.map(getType), ret), annotatedCapture)
    }
  }

  def getType(vparam: ValueParam): ValueType = vparam.tpe
  def getType(bparam: BlockParam): BlockType = bparam.tpe


}
