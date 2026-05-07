package effekt.core.optimizer

import effekt.{Phase, PhaseResult, core, symbols}
import effekt.PhaseResult.{CoreTransformed, Typechecked}
import effekt.context.Context
import effekt.core.Stmt.*
import effekt.core.*
import effekt.core.Block.*
import effekt.core.BlockType.*
import effekt.symbols.{TermSymbol, TypeSymbol}
import effekt.util.messages.ErrorReporter

object TRMC extends Phase[CoreTransformed, CoreTransformed]{
  val phaseName: String = "trmc"

  case class ImpossibleStateError(message: String) extends RuntimeException(message: String)

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    val CoreTransformed(source, tree, mod, modDec) = input
    println(effekt.util.PrettyPrinter.format(modDec).layout)
    val DC = DeclarationContext(modDec.declarations, modDec.externs)
    
    var transformedFunctions: List[Toplevel] = List()
    
    object transform extends Tree.Rewrite {
      override def toplevel: PartialFunction[Toplevel, Toplevel] = {
        case Toplevel.Def(id, block) =>
          println("in Toplevel")
          println(id.name.name)
          if (id.name.name == "simpleTRMC") {
            transformedFunctions = transformedFunctions.appended(trmc(id, block, DC))
          }
          Toplevel.Def(id, block)//TODO: rewrite calls

      }

//      override def stmt: PartialFunction[Stmt, Stmt] = {
//        case Def(id, block, body) if id.name.name == "simpleTRMC" =>
//          trmc(id, block, body)
//      }
    }

    val transformed = Context.timed(phaseName, source.name) {
      val m = transform.rewrite(modDec)
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

  
  enum TransformContext {
    case Outer(id: Id)
    case Val(id: Id, body: Stmt, next: TransformContext)
  }

  enum TailContext {
    case Empty
    case Outer(id: Id)
    case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], before: List[Expr], after: List[Expr])
    case Compose(first: TailContext, second: TailContext)
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
      case TransformContext.Val(id, Stmt.Return(Expr.Make(data, tag, targs, head :: Expr.ValueVar(id2, tpe) :: Nil)), next) //TODO: leads to always a compose TailContext, even if its only one Val
        if tag == consId && id == id2 =>
        val (init, rest) = split(next, DC)
        (TailContext.Compose(init, TailContext.Make(data, tag, targs, List(head), Nil)), rest)
      case _ => (TailContext.Empty, Some(context))
    }
  }
    


  def trmc(id: Id, block: Block, DC: DeclarationContext)(using Context): Toplevel = block match {
    case effekt.core.Block.BlockVar(id, annotatedTpe, annotatedCapt) => ???
    case effekt.core.Block.BlockLit(tparams, cparams, vparams, bparams, body) => 
      val outputFunId = Id(id.name.name + "_trmc")
      val ctxDecl: Declaration = DC.declarations.find(_.id.name.name == "HoleContext").getOrElse {
        Context.panic(s"No declaration found for HoleContext.")
      }
      val outerContextTpe = ValueType.Data(ctxDecl.id,List(body.tpe, body.tpe))
      val ctxId = Id("ctx")
      Toplevel.Def(outputFunId, BlockLit(tparams, cparams, vparams.appended(ValueParam(ctxId, outerContextTpe)), bparams,
        trmc(body, id, outputFunId, TransformContext.Outer(ctxId), outerContextTpe, DC))) //TODO: fix HoleContext (its in the Declerations, but how to parametrize?)
    case effekt.core.Block.Unbox(pure) => ???
    case effekt.core.Block.New(impl) => ???
  }
  
  def trmc(input: Stmt, inputfun: Id, outputfun: Id, context: TransformContext, outerContextTpe: ValueType, DC: DeclarationContext)(using Context): Stmt = input match {
    case Stmt.Def(id, block, body) => ???
    case Stmt.Let(id, binding, body) => Stmt.Let(id, binding, trmc(body,inputfun, outputfun, context, outerContextTpe, DC))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???
    case Stmt.Return(expr) => reify(input, context, inputfun, outputfun, outerContextTpe, DC) //probably works every time, original function must still exit in case inputfun is free in expr
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
                vargs.appended(innerReify(init, outerContextTpe, input.tpe, DC)), //is input.tpe always the correct resType?
                bargs)
              rest match {
                case Some(rest) => reify(inner, rest, inputfun, outputfun, outerContextTpe, DC)
                case None => inner
              }
            case _ => throw ImpossibleStateError("in an App() Statement a Function should be called")
          }
        }else{
          reify(input, context, inputfun, outputfun, outerContextTpe, DC)
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
      Stmt.Val(tmpId, stmt, Stmt.Return(PureApp(blockVarFromExternDef("ctx_applyContext", DC), Nil, List(ValueVar(id, outerContextTpe), ValueVar(tmpId, stmt.tpe))))) //TODO: fix ctxType,tmpType?
    case TransformContext.Val(id, body, next) =>
      Stmt.Val(id, stmt, trmc(body, inputfun, outputfun, next, outerContextTpe, DC))
  }

  def innerReify(context: TailContext, outerContextTpe: ValueType, resType: ValueType, DC: DeclarationContext)(using Context): Expr = context match {
    case TailContext.Empty => PureApp(blockVarFromExternDef("ctx_emptyContext", DC),List(resType),Nil) 
    case TailContext.Outer(id) => Expr.ValueVar(id,outerContextTpe)
    case TailContext.Make(data, tag, targs, before, after) => MakeContext(data, tag, targs, before, after)
    case TailContext.Compose(first, second) => PureApp(blockVarFromExternDef("ctx_composeContext", DC), Nil, List(innerReify(first, outerContextTpe, resType, DC),innerReify(second, outerContextTpe, resType, DC)))
  }
  
  def blockVarFromExternDef(name: String, DC: DeclarationContext)(using Context) : Block.BlockVar = {
    DC.getUniqueExternDef(name) match {
      case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => BlockVar(id, Function(tparams, cparams, vparams.map(getType), bparams.map(getType), ret), annotatedCapture)
    }
  }

  def getType(vparam: ValueParam): ValueType = vparam.tpe
  def getType(bparam: BlockParam): BlockType = bparam.tpe


}
