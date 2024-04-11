package effekt
package generator
package hvm

import effekt.core.Id
import effekt.machine.builtins

import scala.collection.mutable.{Map => MutableMap}
import effekt.generator.chez.DeadCodeElimination.expr





//cps to hvm
def transform(mod: cps.ModuleDecl): Book = { 
  //Decl=>adt, extern=> Extern, def=>Def, path=> entrypoint
  val decls = mod.decls.flatMap(transform)
  val externs = mod.externs.map(transform)
  val defns = mod.definitions.map(transform)
  val defMap: MutableMap[Name, Definition] = MutableMap()
  decls.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  externs.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  defns.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  Book(defMap, MutableMap(), MutableMap(), Some(Name(mod.path)))
}

def transform(decl: cps.Declaration): List[Definition] = decl match {
  case cps.Declaration.Data(id, _, ctors) => List(Definition(id.name.name, List(), true))
  case cps.Declaration.Interface(id, _, operations) => List(Definition(id.name.name, List(), true))
}

def transform(decl: cps.Extern): Definition = decl match {
  case cps.Extern.Def(id, tparams, params, ret, body) => Definition(id.name.name, List(), true)
  case cps.Extern.Include(contents) => Definition(Id(contents).name.name, List(), true)
}

def transform(definition: cps.Definition): Definition = definition match {
  //params + body => rule(patterns, body)
  case cps.Definition.Function(name, params, cont, body) => 
    Definition(name.name.name, List(Rule((params map transform) :+ VarPattern(Some(cont.name.name)), transform(body))), true)
  case cps.Definition.Let(id, expr) => Definition(id.name.name, List(Rule(List(VarPattern(Some(id.name.name))), transform(expr))), true)
}

def transform(param: cps.Param): Pattern = param match {
  case cps.Param.ValueParam(id, _) => VarPattern(Some(id.name.name))
  case _ => ???
}

def transform(term: cps.Term): Term = term match {
  case cps.Term.AppCont(id, arg) => App(Auto, Var(id.name.name), transform(arg))
  case _ => println(term); ???
}

def transform(expr: cps.Expr): Term = expr match {
  case cps.Expr.Lit(n) => Num(n)
  case _ => ???
}