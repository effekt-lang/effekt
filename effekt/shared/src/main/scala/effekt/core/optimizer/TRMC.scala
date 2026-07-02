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
  
  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    val CoreTransformed(source, tree, mod, modDec) = input
    println(effekt.util.PrettyPrinter.format(modDec).layout)
    val DC = DeclarationContext(modDec.declarations, modDec.externs)
    
    var transformedFunctions: List[Toplevel] = List()
    
    var functionLinks: Map[Id, Id] = modDec.definitions.collect{
      case Toplevel.Def(id, block) =>
        val outputFunId = Id(id.name.name + "_trmc")
        (id -> outputFunId)
    }.toMap //TODO: exclude main?
    
    
    object transform extends Tree.Rewrite {
      override def toplevel: PartialFunction[Toplevel, Toplevel] = {
        case Toplevel.Def(id, block) =>
          println("in Toplevel")
          println(id.name.name)
          //println(effekt.util.PrettyPrinter.format(Toplevel.Def(id, block)).layout)
          val outputFunId = functionLinks(id)
          if (id.name.name != "main") {
            transformedFunctions = transformedFunctions.appended(trmc(id, block, outputFunId, functionLinks, DC))
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
          case Block.BlockVar(id, annotatedTpe, annotatedCapt) => Toplevel.Def(id, block) //TODO: do BlockVar, Unbox and New actually happen?
          case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
            Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, rewriteCalls(body, id, functionLinks, DC))) 
          case Block.Unbox(pure) => Toplevel.Def(id, block)
          case Block.New(impl) => Toplevel.Def(id, block)
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
  
  private def rewriteCalls(stmt: Stmt, transformedfun: Id, functionLinks: Map[Id, Id], DC: DeclarationContext)(using Context): Stmt = stmt match {
    case Stmt.Def(id, block, body) => Stmt.Def(id, block, rewriteCalls(body, transformedfun, functionLinks, DC)) //TODO: rewrite blocks too?
    case Stmt.Let(id, binding, body) => Stmt.Let(id, binding, rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => Stmt.ImpureApp(id, callee, targs, vargs, bargs, rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.Return(expr) => Stmt.Return(expr)
    case Stmt.Val(id, binding, body) => Stmt.Val(id, rewriteCalls(binding, transformedfun, functionLinks, DC), rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.App(callee, targs, vargs, bargs) => callee match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
        val outputfun = functionLinks.get(id)
        if(id != transformedfun && outputfun.isDefined){ //TODO: do recursive calls have to be excluded?
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
            case _ => Context.panic("in an App() Statement a Function should be called")
          }
        }else{
          stmt
        }
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => stmt //TODO: do BLockLit, Unbox and New actually happen?
      case Block.Unbox(pure) => stmt
      case Block.New(impl) => stmt
    }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => stmt
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
    case Stmt.Region(body) => stmt //TODO: rewrite blocks too?
    case Stmt.Alloc(id, init, region, body) => Stmt.Alloc(id, init, region, rewriteCalls(body, transformedfun, functionLinks, DC)) //TODO: body might be smth different?
    case Stmt.Var(ref, init, capture, body) => Stmt.Var(ref, init, capture, rewriteCalls(body, transformedfun, functionLinks, DC)) //TODO: body might be smth different?
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => Stmt.Get(id, annotatedTpe, ref, annotatedCapt, rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.Put(ref, annotatedCapt, value, body) => Stmt.Put(ref, annotatedCapt, value, rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.Reset(body) => stmt //TODO: rewrite blocks too?
    case Stmt.Shift(prompt, k, body) => Stmt.Shift(prompt, k, rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.Resume(k, body) => Stmt.Resume(k, rewriteCalls(body, transformedfun, functionLinks, DC))
    case Stmt.Hole(annotatedTpe, span) => stmt
  }
  
  private def trmc(id: Id, block: Block, outputfun: Id, functionLinks: Map[Id, Id], DC: DeclarationContext)(using Context): Toplevel = block match {
    case effekt.core.Block.BlockVar(id, annotatedTpe, annotatedCapt) => Toplevel.Def(id, block) //fallback to original function correct? //TODO: do BlockVar, Unbox and New actually happen?
    case effekt.core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
//      val ctxDecl: Declaration = DC.declarations.find(_.id.name.name == "HoleContext").getOrElse {
//        Context.panic(s"No declaration found for HoleContext.")
//      }
      val outerContextTpe = ValueType.Data(builtins.ContextSymbol, List(body.tpe, body.tpe)) //TODO: parameters, if unequal
      val ctxId = Id("ctx")
      Toplevel.Def(outputfun, BlockLit(tparams, cparams, vparams.appended(ValueParam(ctxId, outerContextTpe)), bparams,
        trmc(body, TransformContext.Outer(ctxId), outerContextTpe, functionLinks, DC)))
    case effekt.core.Block.Unbox(pure) => Toplevel.Def(id, block)
    case effekt.core.Block.New(impl) => Toplevel.Def(id, block)
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
      case _ => Context.panic("should have failed above while finding List or the pattern is incorrect")
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
  
  private def trmc(block: Block, functionLinks: Map[Id, Id], DC: DeclarationContext)(using Context): Block = block match {
    case effekt.core.Block.BlockVar(id, annotatedTpe, annotatedCapt) => block
    case effekt.core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      //      val ctxDecl: Declaration = DC.declarations.find(_.id.name.name == "HoleContext").getOrElse {
      //        Context.panic(s"No declaration found for HoleContext.")
      //      }
      val outerContextTpe = ValueType.Data(builtins.ContextSymbol, List(body.tpe, body.tpe)) //TODO: parameters, if unequal
      val ctxId = Id("ctx")
      BlockLit(tparams, cparams, vparams.appended(ValueParam(ctxId, outerContextTpe)), bparams,
        trmc(body,  TransformContext.Outer(ctxId), outerContextTpe, functionLinks, DC))
    case effekt.core.Block.Unbox(pure) => block
    case effekt.core.Block.New(impl) => block
  }
  
  private def trmc(stmt: Stmt, context: TransformContext, outerContextTpe: ValueType, functionLinks: Map[Id, Id], DC: DeclarationContext)(using Context): Stmt = stmt match {
    case Stmt.Def(id, block, body) => 
      val outputId = Id(id.name.name + "_trmc")
      val updatedLinks = functionLinks + (id -> outputId)
      Stmt.Def(id, block, 
        Stmt.Def(outputId, 
          trmc(block, updatedLinks, DC), 
          trmc(body,  context, outerContextTpe, updatedLinks, DC))) 
    case Stmt.Let(id, binding, body) => Stmt.Let(id, binding, trmc(body,  context, outerContextTpe, functionLinks, DC))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => Stmt.ImpureApp(id, callee, targs, vargs, bargs, trmc(body,  context, outerContextTpe, functionLinks, DC))
    case Stmt.Return(expr) => reify(stmt, context,  outerContextTpe, functionLinks, DC) //probably works every time, original function must still exist in case inputfun is free in expr
    case Stmt.Val(id, binding, body) => trmc(binding,  TransformContext.Val(id,body,context), outerContextTpe, functionLinks, DC)
    case Stmt.App(callee, targs, vargs, bargs) => callee match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
        functionLinks.get(id) match {
          case Some(transformed) => 
            val (init, rest) = split(context, DC)
            annotatedTpe match {
              case Function(tparams, cparams, vparams, bparams, result) =>
                val inner = Stmt.App(
                  Block.BlockVar(
                    transformed,
                    Function(tparams, cparams, vparams.appended(outerContextTpe), bparams, result), annotatedCapt),//TODO:capt? 
                  targs,
                  vargs.appended(innerReify(init, outerContextTpe, stmt.tpe, DC)), //is input.tpe always the correct resType?
                  bargs)
                rest match {
                  case Some(rest) => reify(inner, rest,  outerContextTpe, functionLinks, DC)
                  case None => inner
                }
              case _ => Context.panic("in an App() Statement a Function should be called")
            }
            // unknown calls, e.g. function arguments of higher order functions
          case None => reify(stmt, context,  outerContextTpe, functionLinks, DC)
        }
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => reify(stmt, context,  outerContextTpe, functionLinks, DC) //TODO: do BLockLit, Unbox and New actually happen?
      case Block.Unbox(pure) => reify(stmt, context,  outerContextTpe, functionLinks, DC)
      case Block.New(impl) => reify(stmt, context,  outerContextTpe, functionLinks, DC)
    }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => reify(stmt, context,  outerContextTpe, functionLinks, DC)
    case Stmt.If(cond, thn, els) =>
      Stmt.If(cond, //same caveat as Return()
        trmc(thn,  context, outerContextTpe, functionLinks, DC),
        trmc(els,  context, outerContextTpe, functionLinks, DC))
    case Stmt.Match(scrutinee, annotatedTpe, clauses, default) =>
      Stmt.Match(
        scrutinee,
        annotatedTpe,
        clauses.map((id, blockLit) => blockLit match {
          case BlockLit(tparams, cparams, vparams, bparams, body) => (id, BlockLit(tparams, cparams, vparams, bparams, trmc(body,  context, outerContextTpe, functionLinks, DC)))
        }),
        default match {
          case Some(value) => Some(trmc(value,  context, outerContextTpe, functionLinks, DC))
          case None => None
        }
      )
    case Stmt.Region(body) => reify(stmt, context,  outerContextTpe, functionLinks, DC)
    case Stmt.Alloc(id, init, region, body) => Stmt.Alloc(id, init, region, trmc(body,  context, outerContextTpe, functionLinks, DC))
    case Stmt.Var(ref, init, capture, body) => Stmt.Var(ref, init, capture, trmc(body,  context, outerContextTpe, functionLinks, DC))
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => Stmt.Get(id, annotatedTpe, ref, annotatedCapt, trmc(body,  context, outerContextTpe, functionLinks, DC))
    case Stmt.Put(ref, annotatedCapt, value, body) => Stmt.Put(ref, annotatedCapt, value, trmc(body,  context, outerContextTpe, functionLinks, DC))
    case Stmt.Reset(body) => reify(stmt, context,  outerContextTpe, functionLinks, DC)
    case Stmt.Shift(prompt, k, body) => reify(stmt, context,  outerContextTpe, functionLinks, DC)
    case Stmt.Resume(k, body) => reify(stmt, context,  outerContextTpe, functionLinks, DC)
    case Stmt.Hole(annotatedTpe, span) => reify(stmt, context,  outerContextTpe, functionLinks, DC)
  }


  def reify(stmt: Stmt, context: TransformContext, outerContextTpe: ValueType, functionLinks: Map[Id, Id], DC: DeclarationContext)(using Context) : Stmt = context match {
    case TransformContext.Outer(id) =>
      val tmpId = Id("tmp")
      Stmt.Val(
        tmpId, 
        stmt, 
        Stmt.Return(
          PureApp(
            blockVarFromExternDef("ctx_applyContext", DC), 
            Nil, 
            List(ValueVar(id, outerContextTpe), ValueVar(tmpId, stmt.tpe)))))
    case TransformContext.Val(id, body, next) =>
      Stmt.Val(id, stmt, trmc(body, next, outerContextTpe, functionLinks, DC))
  }

  def innerReify(context: TailContext, outerContextTpe: ValueType, resType: ValueType, DC: DeclarationContext)(using Context): Expr = context match {
    case TailContext.Empty => PureApp(blockVarFromExternDef("ctx_emptyContext", DC),List(resType),Nil) 
    case TailContext.Outer(id) => Expr.ValueVar(id, outerContextTpe)
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
