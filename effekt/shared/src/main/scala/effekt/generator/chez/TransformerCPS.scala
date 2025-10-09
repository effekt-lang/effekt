package effekt
package generator
package chez

import cps.*
import effekt.symbols.Symbol
import effekt.core

object TransformerCPS {

  // Defined in chez/cps/effekt.ss
  val RUN_TOP_LEVEL = "run-top-level"

  def compile(input: cps.ModuleDecl, mainSymbol: symbols.TermSymbol): chez.Block = {
    val definitions = input.definitions.map(toChez)
    val externs = input.externs.map(toChez)
    val runMain = Builtin(RUN_TOP_LEVEL, nameRef(mainSymbol))
    chez.Block(externs ++ definitions, Nil, runMain)
  }

  def toChez(definition: cps.ToplevelDefinition): chez.Def = definition match {
    case ToplevelDefinition.Def(id, block) => chez.Constant(nameDef(id), toChez(block))
    case ToplevelDefinition.Let(id, expr) => chez.Constant(nameDef(id), toChez(expr))
    case _ => ???
  }

  def toChez(extern: cps.Extern): chez.Def = extern match {
    case Extern.Def(id, vparams, bparams, async, body) if !async =>
      val params = (vparams ++ bparams).map(nameDef)
      chez.Function(nameDef(id), params, toChez(body))
    case Extern.Include(_, contents) =>
      chez.RawDef(contents)
    case _ => ???
  }

  def toChez(externBody: cps.ExternBody): chez.Expr = externBody match {
    case ExternBody.StringExternBody(_, contents) =>
      RawExpr(contents.strings, contents.args.map(toChez))
    case ExternBody.Unsupported(err) => ???
  }

  def toChez(block: cps.Block): chez.Expr = block match {
    case BlockVar(id) => chez.Variable(nameRef(id))
    case BlockLit(vparams, bparams, ks, k, body) =>
      val params = vparams ++ bparams ++ List(ks, k)
      chez.Lambda(params.map(nameDef), toChez(body))
    case _ => ???
  }

  def toChez(stmt: cps.Stmt): chez.Expr = stmt match {
    case App(callee, vargs, bargs, ks, k) =>
      val args = vargs.map(toChez) ++ bargs.map(toChez)
              ++ List(toChez(ks.id), toChez(k))
      chez.Call(toChez(callee), args)
    case Jump(k, vargs, ks) =>
      val args = vargs.map(toChez) :+ toChez(ks.id)
      chez.Call(toChez(k), args)
    case _ => ???
  }

  def toChez(expr: cps.Expr): chez.Expr = expr match {
    case ValueVar(id) => toChez(id)
    case Literal(()) => chez.RawValue("(void)")
    case Literal(value) => chez.RawValue(value.toString())
    case _ => ???
  }

  def toChez(cont: cps.Cont): chez.Expr = cont match {
    case Cont.ContVar(id) => toChez(id)
    case _ => ???
  }

  def toChez(id: Symbol): chez.Variable = Variable(nameRef(id))
}