package effekt.core.optimizer

import effekt.{Phase, PhaseResult, core, symbols}
import effekt.PhaseResult.{CoreTransformed, Typechecked}
import effekt.context.Context
import effekt.core.Stmt.{App, Def}
import effekt.core.{Block, Expr, Id, ModuleDecl, Stmt, Toplevel, Tree, ValueType}
import effekt.core.Block.*
import effekt.symbols.{TermSymbol, TypeSymbol}

object TRMC extends Phase[CoreTransformed, CoreTransformed]{
  val phaseName: String = "trmc"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    val CoreTransformed(source, tree, mod, modDec) = input
    //println(effekt.util.PrettyPrinter.format(modDec).layout)
    
    object transform extends Tree.Rewrite{
      override def toplevel: PartialFunction[Toplevel, Toplevel] = {
        case Toplevel.Def(id, block) =>
          println("in Toplevel")
          println(id.name.name)
          if (tailRecModCons(id, block)){
            println("Halleluja")
          }
          trmc(id, block)
      }
      override def stmt: PartialFunction[Stmt, Stmt] = {
        case Def(id, block, body) if tailRecModCons(id, block, body) =>
          trmc(id, block,body)
      }
    }

    val transformed = Context.timed(phaseName, source.name) {
      transform.rewrite(modDec)
    }

    Some(CoreTransformed(source, tree, mod, transformed))
    
  def tailRecModCons(id: Id, block: Block): Boolean = { 
    if(id.name.name == "simpleTRMC"){
      println(effekt.util.PrettyPrinter.format(Toplevel.Def(id, block)).layout)
    }
    block match {
      case BlockVar(id, annotatedTpe, annotatedCapt) => ???
      case BlockLit(tparams,cparams,vparams,bparams,body) => tailRecModCons(body, id.name.name, false)
      case Unbox(pure) => ???
      case New(impl) => ???
    }
  }


  def tailRecModCons(id: Id, block: Block, body: Stmt): Boolean = {
    //println(effekt.util.PrettyPrinter.format(Def(id, block, body)).layout)
    id.name.name match {
      case "simpleTRMC" => true
      case _ => false
    }
    
  }

  //non-recursive is also true 
  //mutually recursive functions should be false, we dont want to jump to definitions
  def tailRecModCons(stmt: Stmt, fun: String, callerIsTail: Boolean): Boolean = stmt match {
    case Stmt.Def(id, block, body) => tailRecModCons(body,fun, false)
    case Stmt.Let(id, binding, body) => tailRecModCons(binding,fun,isTail(stmt)) && tailRecModCons(body,fun, false)
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???
    case Stmt.Return(expr) => tailRecModCons(expr, fun, isTail(stmt)) 
    case Stmt.Val(id, binding, body) => tailRecModCons(binding,fun,isTail(stmt)) && tailRecModCons(body,fun,false)
    case Stmt.App(callee, targs, vargs, bargs) => callee match {
      case effekt.core.Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
        if(id.name.name == fun && callerIsTail){
          true
        }else{
          false
        }
      case effekt.core.Block.BlockLit(tparams, cparams, vparams, bparams, body) => ???
      case effekt.core.Block.Unbox(pure) => ???
      case effekt.core.Block.New(impl) => ???
    }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???
    case Stmt.If(cond, thn, els) => tailRecModCons(thn,fun,false) && tailRecModCons(els,fun, false)
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
  
  def tailRecModCons(expr: Expr, fun: String, callerIsTail: Boolean): Boolean = expr match {
    case Expr.ValueVar(id, annotatedType) => true
    case Expr.Literal(value, annotatedType) => true
    case Expr.PureApp(b, targs, vargs) => 
      if((b.id.name.name == fun) == callerIsTail){ //subtle difference to above: ==
        true
      }else{
        false
      }
    case Expr.Make(data, tag, targs, vargs) => tag.name.name match {
      case "Nil" => true
      case "Cons" => true
      case _ => false
    }
    case Expr.Box(b, annotatedCapture) => ???
  }
  
  def isTail(stmt:Stmt): Boolean = stmt match {
    case Stmt.Def(id, block, body) => ???
    case Stmt.Let(id, binding, body) => isReturnOrCons(body)
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???
    case Stmt.Return(expr) => true
    case Stmt.Val(id, binding, body) => isReturnOrCons(body)
    case Stmt.App(callee, targs, vargs, bargs) => ???
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???
    case Stmt.If(cond, thn, els) => ???
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
  
  def isReturnOrCons(stmt: Stmt): Boolean = stmt match {
    case effekt.core.Stmt.Let(id, binding, body) => binding match {
      case effekt.core.Expr.ValueVar(id, annotatedType) => false
      case effekt.core.Expr.Literal(value, annotatedType) => false
      case effekt.core.Expr.PureApp(b, targs, vargs) => false
      case effekt.core.Expr.Make(data, tag, targs, vargs) => tag.name.name match { //vargs could again be anything
        case "Cons" => true
        case _ => false
      }
      case effekt.core.Expr.Box(b, annotatedCapture) => false
    }
    case effekt.core.Stmt.Return(expr) => true
    case effekt.core.Stmt.Val(id, binding, body) => false //?
    case _ => false
  }

  def trmc(id: Id, block: Block): Toplevel = {
    println("in trmc()")
    Toplevel.Def(id: Id, block: Block)
  }
  def trmc(id: Id, block: Block, body: Stmt): Stmt = {
    Def(id, block, body)
  }

}
