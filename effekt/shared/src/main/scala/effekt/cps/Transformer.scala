package effekt
package cps

import effekt.PhaseResult
import effekt.Phase
import effekt.context.Context
import effekt.symbols.{Symbol}
import effekt.context.assertions.*
import effekt.util.messages.ErrorReporter
import effekt.core.CoreParsers.statement
import effekt.cps.*




object Transformer extends Phase[PhaseResult.CoreLifted, PhaseResult.CpsTransformed] {

  val phaseName = "cps-transform"


  def run(input: CoreLifted)(using Context): Option[CpsTransformed] =
    val transformed = cps.transform(input.core)
    Some(CpsTransformed(input.source, input.tree, input.mod, input.core, transformed))
}

def transform(core: lifted.ModuleDecl): ModuleDecl = 
  ModuleDecl(core.path, core.includes, core.decls map transform, core.externs map transform, core.definitions map transform, core.exports)

def transform(decl: lifted.Declaration): Declaration = decl match {
  case lifted.Declaration.Data(id, tparams, constructors) => Declaration.Data(id, tparams, constructors map transform)
  case lifted.Declaration.Interface(id, tparams, properties) => Declaration.Interface(id, tparams, properties map transform)
}

def transform(extern: lifted.Extern): Extern = extern match {
  case Extern.Def(id, tparams, params, ret, body) => Extern.Def(id, tparams, params, ret, Template(body.strings, body.args map transform)
  )
}

def transform(definition: lifted.Definition): Definition = ???


def transform(expr: lifted.Expr): Expr = ???

def transform(stmt: lifted.Stmt): Term = stmt match {
  case lifted.Stmt.Return(e)  =>  Term.AppCont(Name("Return"), transform(e))
  case lifted.Stmt.Val(id, binding, body)  =>  ???
  case _ => ???
}

def transform(constructor: lifted.Constructor): Constructor = Constructor(constructor.id, constructor.fields map transform)
def transform(property: lifted.Property): Property = Property(property.id, transform(property.tpe))
def transform(field: lifted.Field): Field = Field(field.id, transform(field.tpe))

def transform(tpe: lifted.ValueType): ValueType = tpe match {
  case lifted.ValueType.Var(id)  =>  ValueType.Var(id)
  case lifted.ValueType.Data(name, targs)  =>  ValueType.Data(name, targs map transform)
  case lifted.ValueType.Boxed(blockTpe)  =>  ValueType.Boxed(transform(blockTpe))
}

def transform(tpe: lifted.BlockType): BlockType = tpe match {
  case lifted.BlockType.Function(tparams, eparams, vparams, bparams, result) 
    => BlockType.Function(tparams, eparams map transform , vparams map transform , bparams map transform, transform(result))
  case lifted.BlockType.Interface(name, targs) => BlockType.Interface(name, targs map transform)
}

def transform(tpe: lifted.EvidenceType): EvidenceType = EvidenceType()
