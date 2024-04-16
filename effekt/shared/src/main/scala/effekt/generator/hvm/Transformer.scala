package effekt
package generator
package hvm

import effekt.core.Id

import scala.collection.mutable.{Map => MutableMap}
import effekt.util.intercalate






//cps to hvm
def transform(mod: cps.ModuleDecl): Book = { 
  //Decl=>adt, extern=> Extern, def=>Def, path=> entrypoint
  val decls = mod.decls.flatMap(transform)
  val externs = mod.externs.map(transform)
  val defns = mod.definitions.map(transform)
  val defMap: MutableMap[Name, Definition] = MutableMap()
  decls.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  //externs.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  defns.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  Book(defMap, externs, MutableMap(), MutableMap(), Some(Name(mod.path)))
}


def transform(decl: cps.Declaration): List[Definition] = decl match {
  case cps.Declaration.Data(id, tparams, ctors) => List(Definition(id.name.name, transformConstructors(tparams, ctors), false))
  case cps.Declaration.Interface(id, tparams, operations) => List(Definition(id.name.name, transformProperties(tparams, operations), false))
}

def transform(decl: cps.Extern): Verbatim = decl match {
  case cps.Extern.Def(id, tparams, params, ret, body) =>
    Verbatim.Def(id.name.name, params map transform, transform(body))
  case cps.Extern.Include(contents) => Verbatim.Include(contents)//Definition("", List(), false)
    //Definition(Id(contents).name.name, List(Rule(List(StrPattern(contents)), Str(contents))), false)
}

def transform(template: Template[cps.Expr]): String = intercalate(template.strings, template.args map transform).mkString

def transform(definition: cps.Definition): Definition = definition match {
  //params + body => rule(patterns, body)
  case cps.Definition.Function(name, params, cont, body) => 
    Definition(name.name.name, List(Rule((params map transform) :+ VarPattern(Some(cont.name.name)), transform(body))), false)
  case cps.Definition.Let(id, expr) => Definition(id.name.name, List(Rule(List(VarPattern(Some(id.name.name))), transform(expr))), false)
}

def transform(param: cps.Param): Pattern = param match {
  case cps.Param.ValueParam(id, _) => VarPattern(Some(id.name.name))
  case cps.Param.EvidenceParam(id) => VarPattern(Some(id.name.name))
  case _ => println(param); ??? 
}
 

def transform(term: cps.Term): Term = term match {
  case cps.Term.AppCont(id, arg) => App(Auto, Var(id.name.name), Tup(arg map transform))
  case _ => println(term); ???
}

def transform(expr: cps.Expr): Term = expr match {
  case cps.Expr.Lit(n) => Num(n)
  case cps.Expr.Var(name) => Var(name.name.name)
  case _ => println(expr); ???
}

def transformConstructors(tparams: List[cps.Id], constructors: List[cps.Constructor]): List[Rule] = (tparams, constructors) match {
  case (List(), constructors) => Rule(List(), Var(constructors.head.id.name.name)) :: transformConstructors(List(), constructors.tail)
  case (tparams, List()) => Rule(List(VarPattern(Some(tparams.head.name.name))), Var(" ")) :: transformConstructors(tparams, List())
  case (tparams, constructors) => Rule(List(VarPattern(Some(tparams.head.name.name))), Var(constructors.head.id.name.name)) :: transformConstructors(tparams.tail, constructors.tail)
}

def transformProperties(tparams: List[cps.Id], properties: List[cps.Property]): List[Rule] = (tparams, properties) match {
  case (List(), properties) => Rule(List(), Var(properties.head.id.name.name)) :: transformProperties(List(), properties.tail)
  case (tparams, List()) => Rule(List(VarPattern(Some(tparams.head.name.name))), Var(" ")) :: transformProperties(tparams, List())
  case (tparams, properties) => Rule(List(VarPattern(Some(tparams.head.name.name))), Var(properties.head.id.name.name)) :: transformProperties(tparams.tail, properties.tail)
}