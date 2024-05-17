package effekt
package generator
package hvm

import effekt.core.Id

import scala.collection.mutable.{Map => MutableMap}
import effekt.util.intercalate
import effekt.core.CoreParsers.definition



def transform(mod: cps.ModuleDecl): Book = {
  val decls = transform(mod.decls, MutableMap()) //Adt
  val externs = mod.externs.map(transform)
  val defns = mod.definitions.map(transform)
  val defMap: MutableMap[Name, Definition] = MutableMap()
  defns.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  mod.decls.foreach({case cps.Declaration.InterfaceDecl(id, operations) => defMap ++ transform(id, operations, defMap); case _ => ()})
  Book(defMap, externs, decls, MutableMap(), Some(Name(mod.path)))
}

//destructor
def transform(id: cps.Id, operations: List[cps.Property], map: MutableMap[Name, Definition]): MutableMap[Name, Definition] =
  val newMap = map
  operations.foreach(operation => 
  map += (Name(id.toString + "." + operation.id.toString) -> 
        Definition(id.toString + "." + operation.id.toString, List(Rule(List(Ctr(id.toString, (operations map (x => x match {case cps.Property(operation.id, _) => VarPattern(Some(operation.id.toString)); case _ => VarPattern(Some("_"))})))), Var(operation.id.toString))), false)))
  map

def transform(decls: List[cps.Declaration], map: MutableMap[Name, Adt]): MutableMap[Name, Adt]= decls match {
  case Nil => map
  case cps.Declaration.Data(id, ctors) :: rest => transform(rest, map += (Name(id.toString) -> transform(ctors, Adt(MutableMap(), false))))
  case cps.Declaration.InterfaceDecl(id, operations) :: rest => 
    transform(rest, map += (Name(id.toString) -> Adt(MutableMap(Name(id.toString) -> (operations map (x=>x.id.toString()))), false)))
}

def transform(decl: cps.Extern): Verbatim = decl match {
  case cps.Extern.Def(id, params, body) =>
    Verbatim.Def(id.name.name, params map idToPattern, transform(body))
  case cps.Extern.Include(contents) => Verbatim.Include(contents)
}

def transform(template: Template[cps.Expr]): String = 
  //intercalate(template.strings, template.args).mkString // keine map
  intercalate(template.strings, template.args map exprToString).mkString // keine map

def transform(definition: cps.Definition): Definition = 
  //println(definition)
  definition match {
  //params + body => rule(patterns, body)
  case cps.Definition.Function(name, params, cont, body) => 
    Definition(name.name.name, List(Rule((params map idToPattern) :+ VarPattern(Some(cont.name.name)), transform(body))), false)
  case cps.Definition.Let(id, expr) => Definition(id.name.name, List(Rule(List(VarPattern(Some(id.name.name))), transform(expr))), false)
}

def transform(term: cps.Term): Term = term match {
  case cps.Term.AppCont(id, arg) => App(Auto, idToVar(id), transform(arg))
  case cps.Term.App(id, args, cont) =>
    chainApp((transform(id)::(args map transform)):+idToVar(cont))
    //App(Auto, Var(cont.toString), chainApp(transform(id)::(args map transform)))
  case cps.Term.Scope(definitions, body) => transform(definitions, body)
  case cps.Term.If(cond, thn, els) => 
    Swt(List(transform(cond)), List(Rule(List(NumPattern(NumCtr.Num(0))), transform(els)), Rule(List(VarPattern(Some("_"))), transform(thn))))
  case cps.Term.Match(scrutinee, clauses, None) => Mat(List(transform(scrutinee)), clauses map ((_, blockLit) => transform(blockLit)))
  case cps.Term.Match(scrutinee, clauses, Some(default)) =>Mat(List(transform(scrutinee)), (clauses map ((_, blockLit) => transform(blockLit))) :+ Rule(List(VarPattern(Some("_"))), transform(default)))
  case cps.Term.Let(name, expr, rest) => Let(idToPattern(name), transform(expr), transform(rest))
  case cps.Term.LetCont(name, param, body, rest) => Let(idToPattern(name), Lam(Auto, Some(param.name.name), transform(body)), transform(rest))
  case cps.Term.Val(id, binding, body) => Let(idToPattern(id), transform(binding), transform(body)) 
  case cps.Term.Fun(name, params, cont, body) => println(term); ???
  case cps.Term.Reset(ev, body) => App(Auto, Let(VarPattern(Some("ev")), Var("lift"), Let(VarPattern(Some("k")), Var("pure"), transform(body))), Var("k"))
}

def transform(blockLit: cps.BlockLit): Rule = blockLit match {
  case cps.Expr.BlockLit(params, body) => Rule(params map idToPattern, transform(body))
}

def transform(definitions: List[cps.Definition], body: cps.Term): Term = definitions match {
  case Nil => transform(body)
  case _ => definitions.head match {
    case cps.Definition.Let(id, bindings) => Let(idToPattern(id), transform(bindings), transform(definitions.tail, body))
    case cps.Definition.Function(name, params, cont, body) => ???
  }
}

def transform(expr: cps.Expr): Term = expr match {
  case cps.Expr.Lit(n) => Num(n)
  case cps.Expr.Var(name) => idToVar(name)
  case cps.Expr.PureApp(b, args) => chainApp(transform(b) :: (args map transform))
  case cps.Expr.Box(b) => transform(b)
  case cps.Expr.Run(t) => transform(t)
  case cps.Expr.BlockLit(params, body) => chainLam(params map (x => x.toString()), transform(body))
  case cps.Expr.Make(data, tag, vargs) => App(Auto, idToVar(data), chainApp(vargs map transform))
  case cps.Expr.Select(target, field) => ???
  case cps.Member(interface, field, tpe) => println(expr); App(Auto, Var(tpe.name.toString + "." + field.toString), transform(interface))
  case cps.New(impl) => chainApp(idToVar(impl.interface._1) :: (impl.operations map (x => transform(x.implementation: cps.Expr))))
}

def transform(constructors: List[cps.Constructor], adt: Adt): Adt = constructors match {
  case Nil => adt
  case cps.Constructor(id, fields) :: rest => transform(rest, Adt(adt.ctrs += (Name(id.toString) -> (fields map (x=>x.toString()))), false))
}

def transform(operation: cps.Id): MutableMap[Name, Adt] =
  val adts: MutableMap[Name, Adt] = MutableMap()
  adts += (Name(operation.toString) -> Adt(MutableMap() += Name(operation.toString) -> List("x"), false))

def transformConstructors(tparams: List[cps.Id], constructors: List[cps.Constructor]): List[Rule] = (tparams, constructors) match {
  case (List(), List()) => List()
  case (List(), constructors) => Rule(List(), Var(constructors.head.id.name.name)) :: transformConstructors(List(), constructors.tail)
  case (tparams, List()) => Rule(List(VarPattern(Some(tparams.head.name.name))), Var(" ")) :: transformConstructors(tparams, List())
  case (tparams, constructors) => Rule(List(VarPattern(Some(tparams.head.name.name))), Var(constructors.head.id.name.name)) :: transformConstructors(tparams.tail, constructors.tail)
}

def transformProperties(tparams: List[cps.Id], properties: List[cps.Id]): List[Rule] = (tparams, properties) match {
  case (List(), List()) => List()
  case (List(), properties) => Rule(List(), Var(properties.head.name.name)) :: transformProperties(List(), properties.tail)
  case (tparams, List()) => Rule(List(VarPattern(Some(tparams.head.name.name))), Var(" ")) :: transformProperties(tparams, List())
  case (tparams, properties) => Rule(List(VarPattern(Some(tparams.head.name.name))), Var(properties.head.name.name)) :: transformProperties(tparams.tail, properties.tail)
}

//helper functions:
def chainApp(args: List[Term]): Term = {
  val reverseArgs = args.reverse
  chainAppHelper(reverseArgs)
}

def chainAppHelper(args: List[Term]): Term = args match {
 case Nil => Err
 case head :: Nil => head
 case head:: tail => App(Auto, chainAppHelper(tail), head)
}

def chainLam(list: List[String], body: Term): Term = list match {
  case Nil => body
  case head :: next => Lam(Auto, Some(head), chainLam(next, body))
}

def chainCtr(args: List[StrPattern]): Pattern = args match {
  case Nil => VarPattern(None)
  case head :: Nil => head
  case StrPattern(string) :: next => Ctr(string, next)
}

def exprToString(expr: cps.Expr): String = expr match {
    case cps.Var(name) => name.name.name
    case cps.Expr.Lit(n) => n.toString()
    case _ => ???
}

def idToPattern(id: Id): Pattern = VarPattern(Some(id.name.name))

def idToVar(id: Id): Var = Var(id.name.name)