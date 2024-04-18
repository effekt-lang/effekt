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
import effekt.source.AnnotateCaptures.annotate





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
  case lifted.Extern.Def(id, tparams, params, ret, body) =>
    Extern.Def(id, tparams, params map transform, transform(ret), Template(body.strings, body.args map transform))
  case lifted.Extern.Include(contents) => Extern.Include(contents)
}

def transform(p: lifted.Param): Param = p match {
  case lifted.Param.ValueParam(id, tpe) => Param.ValueParam(id, transform(tpe))
  case lifted.Param.BlockParam(id, tpe) => Param.BlockParam(id, transform(tpe))
  case lifted.Param.EvidenceParam(id) => Param.EvidenceParam(id)
}


def transform(definition: lifted.Definition): Definition = definition match {
  case lifted.Definition.Def(id, lifted.BlockLit(List(), params, body)) =>
    Definition.Function(id, params map transform, Id.apply("k"), transform(body))
  case lifted.Definition.Let(id, binding) => Definition.Let(id, transform(binding))
  case _ => 
    println(definition)
  ???
}


def transform(arg: lifted.Argument): Either[Expr, Block] = arg match {
  case expr: lifted.Expr => Left(transform(expr))
  case block: lifted.Block => Right(transform(block))
  case ev: lifted.Evidence => ???
}

def transform(ev: lifted.Evidence): Var = Var(Id.apply("something"))

def transform(expr: lifted.Expr): Expr = expr match {
  case lifted.Literal(value, _) => Expr.Lit(value.toString().toInt)// any to Int
  case lifted.ValueVar(id, annotatedType) => Expr.Var(id)
  case lifted.PureApp(b, targs, args) => Expr.PureApp(transform(b), targs map transform, args map transform)
  case lifted.Make(data, tag, args) => Expr.Make(transform(data), tag, args map transform)
  case lifted.Select(target, field, annotatedType) => Expr.Select(transform(target), field, transform(annotatedType))
  case lifted.Box(b) => Expr.Box(transform(b))
  case lifted.Run(s) => Expr.Run(transform(s))
}

def transform(b: lifted.Block.BlockLit): BlockLit =
  BlockLit(b.tparams, b.params map transform, transform(b.body))

def transform(b: lifted.Block): Block = b match {
  case lifted.Block.BlockVar(id, annotatedType) => BlockVar(id, transform(annotatedType))
  case lifted.Block.BlockLit(tparams, params, body) => BlockLit(tparams, params map transform, transform(body))
  case lifted.Block.Member(b, field, annotatedType) => Member(transform(b), field, transform(annotatedType))
  case lifted.Block.Unbox(e) => Unbox(transform(e))
  case lifted.Block.New(impl) => New(transform(impl))
}

def transform(impl: lifted.Implementation): Implementation =
  Implementation(transform(impl.interface), impl.operations map transform)

def transform(operation: lifted.Operation): Operation =
  Operation(operation.name, transform(operation.implementation))

def transform(stmt: lifted.Stmt): Term = stmt match {
  case lifted.Stmt.Return(e)  =>  AppCont(Id.apply("k"), transform(e))
  case lifted.Stmt.Val(id, binding, body)  => Val(id, transform(binding), transform(body))
  case lifted.Stmt.Scope(definitions, body) => Scope(definitions map transform, transform(body))
  case lifted.Stmt.App(b, targs, args) => println(stmt); ???
  case _ => println(stmt); ???
}

def transform(constructor: lifted.Constructor): Constructor = Constructor(constructor.id, constructor.fields map transform)
def transform(property: lifted.Property): Property = Property(property.id, transform(property.tpe))
def transform(field: lifted.Field): Field = Field(field.id, transform(field.tpe)) 

def transform(tpe: lifted.ValueType): ValueType = tpe match {
  case lifted.ValueType.Var(id)  =>  ValueType.Var(id)
  case lifted.ValueType.Data(name, targs)  =>  ValueType.Data(name, targs map transform)
  case lifted.ValueType.Boxed(blockTpe)  =>  ValueType.Boxed(transform(blockTpe))
}

def transform(dataTpe: lifted.ValueType.Data): ValueType.Data = ValueType.Data(dataTpe.name, dataTpe.targs map transform)

def transform(tpe: lifted.BlockType.Interface): BlockType.Interface =
  BlockType.Interface(tpe.name, tpe.targs map transform)

def transform(tpe: lifted.BlockType): BlockType = tpe match {
  case lifted.BlockType.Function(tparams, eparams, vparams, bparams, result) 
    => BlockType.Function(tparams, eparams map transform , vparams map transform , bparams map transform, transform(result))
  case lifted.BlockType.Interface(name, targs) => BlockType.Interface(name, targs map transform)
}



def transform(tpe: lifted.EvidenceType): EvidenceType = EvidenceType()
