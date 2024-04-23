package effekt
package generator
package hvm

import effekt.core.Id

import scala.collection.mutable.{Map => MutableMap}
import effekt.util.intercalate
import effekt.core.CoreParsers.definition




//cps to hvm
def transform(mod: cps.ModuleDecl): Book = {
  val decls = mod.decls.flatMap(transform)
  val externs = mod.externs.map(transform)
  val defns = mod.definitions.map(transform)
  val defMap: MutableMap[Name, Definition] = MutableMap()
  decls.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  defns.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  Book(defMap, externs, MutableMap(), MutableMap(), Some(Name(mod.path)))
}


def transform(decl: cps.Declaration): List[Definition] = decl match {
  case cps.Declaration.Data(id, tparams, ctors) => List(Definition(id.name.name, transformConstructors(tparams, ctors), false))
  case cps.Declaration.Interface(id, tparams, operations) => List(Definition(id.name.name, transformProperties(tparams, operations), false))
}

def transform(decl: cps.Extern): Verbatim = decl match {
  case cps.Extern.Def(id, tparams, params, ret, body) =>
    Verbatim.Def(id.name.name, params map idToPattern, transform(body))//let tmp für jedes body.args
  case cps.Extern.Include(contents) => Verbatim.Include(contents)
}

def transform(template: Template[cps.Expr]): String = 
  intercalate(template.strings, template.args map {case cps.Expr.Var(name) => name case _ => ???}).mkString // keine map

def transform(definition: cps.Definition): Definition = definition match {
  //params + body => rule(patterns, body)
  case cps.Definition.Function(name, params, cont, body) => 
    Definition(name.name.name, List(Rule((params map idToPattern) :+ VarPattern(Some(cont.name.name)), transform(body))), false)
  case cps.Definition.Let(id, expr) => Definition(id.name.name, List(Rule(List(VarPattern(Some(id.name.name))), transform(expr))), false)
}

def transform(param: cps.Param): Pattern = param match {
  case cps.Param.ValueParam(id, _) => VarPattern(Some(id.name.name))
  case cps.Param.EvidenceParam(id) => VarPattern(Some(id.name.name))
  case _ => println(param); ??? 
}
 

def transform(term: cps.Term): Term = term match {
  case cps.Term.AppCont(id, arg) => App(Auto, Var(id.name.name), transform(arg))
  case cps.Term.App(id, args, cont) => chainApp(Var(cont.name.name)::(args map transform))
  case cps.Term.Scope(definitions, body) => transform(definitions, body)
  case cps.Term.If(cond, thn, els) => 
    Swt(List(transform(cond)), List(Rule(List(NumPattern(NumCtr.Num(0))), transform(els)), Rule(List(VarPattern(Some("_"))), transform(thn))))
  case cps.Term.Match(scrutinee, clauses, None) => ???//Swt(List(transform(scrutinee)), clauses map ((_, blockLit) => transform(blockLit)))
  case cps.Term.Match(scrutinee, clauses, Some(default)) => ???//Swt(List(transform(scrutinee)), (clauses map ((x: cps.Id, blockLit: cps.Block.BlockLit) => transform(blockLit))) :+ Rule(List(VarPattern(Some("_"))), transform(default)))
  case _ => ??? 
}

//BlockLit=> Rule
def transform(blockLit: cps.BlockLit): Rule = blockLit match {
  case _ => ???
}
def transform(block: cps.Block): Term = block match {
  case cps.Block.BlockVar(id, annotatedType) => Var(id.name.name)
  case _ => ???
}

def transform(arg: Either[cps.Expr, cps.Block]): Term = arg match{
  case Left(expr) => transform(expr)
  case Right(block) => transform(block)
}

def transform(definitions: List[cps.Definition], body: cps.Term): Term = definitions match {
  case Nil => transform(body)
  case _ => definitions.head match {
    case cps.Definition.Let(id, bindings) => Let(VarPattern(Some(id.name.name)), transform(bindings), transform(definitions.tail, body))
    case cps.Definition.Function(name, params, cont, body) => Let(VarPattern(Some(name.name.name)), Num(5), App(Auto, Var(cont.name.name), transform(body)))
  }
}

def transform(expr: cps.Expr): Term = expr match {
  case cps.Expr.Lit(n) => Num(n)
  case cps.Expr.Var(name) => Var(name.name.name)
  case cps.Expr.PureApp(b, _, args) => chainApp(Var(b.name.name) :: (args map transform))
  case _ => ???
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

//helper functions:
def chainApp(args: List[Term]): Term = {//println(args);
  val reverseArgs = args.reverse
  chainAppHelper(reverseArgs)
}

def chainAppHelper(args: List[Term]): Term = args match {
 case Nil => Err
 case head :: Nil => head
 case head:: tail => App(Auto, chainAppHelper(tail), head)
}

def idToPattern(id: Id): Pattern = VarPattern(Some(id.name.name))