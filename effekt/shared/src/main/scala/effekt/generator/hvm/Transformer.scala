package effekt
package generator
package hvm

import effekt.core.Id

import scala.collection.mutable.{Map => MutableMap}
import effekt.util.intercalate
import scala.annotation.targetName


//using enviroment lift (Context)
case class Environment(adts: Adts) {
  def findKeyInAdts(targetName: Name): Option[Name] = {
  // Iterate through the Adts map
  adts.find { case (key, adt) =>
    // Check if the targetName is in the ctrs map of the current Adt instance
    adt.ctrs.contains(targetName)
  }.map(_._1) // Return the key if found, otherwise return None
  }
  def getName(constructor: lifted.Id): String = findKeyInAdts(idToName(constructor)) match {
    case Some(Name(string)) => string + "/" + constructor.name.name
    case _ => ""

  }
  def getFields(constructor: lifted.Id, scrutinee: String): List[String] =
    // Iterate through all Adt instances in the Adts map
    adts.collectFirst {
      case (_, adt) if adt.ctrs.contains(idToName(constructor)) => adt.ctrs(idToName(constructor))
    } match {
      case Some(fields) => fields map (x => scrutinee + "." + x)
      case None => List()
  }
}

def transform(mod: cps.ModuleDecl): Book = {
  val decls = transform(mod.decls, MutableMap()) //Adt
  val defMap: MutableMap[Name, Definition] = MutableMap() // Definitions
  mod.decls.foreach({
    case cps.Declaration.InterfaceDecl(id, operations) => defMap ++= transform(id, operations); 
    case cps.Declaration.Data(id, ctors) => defMap ++= transform(id, ctors)
  })
  val externs = mod.externs.map(x => transform(x)(using Environment(decls)))
  val defns = mod.definitions.map(x => transform(x)(using Environment(decls)))
  
  defns.foreach({case Definition(name, rules, builtin) => defMap += (Name(name) -> Definition(name, rules, builtin))})
  
  Book(defMap, externs, decls, MutableMap(), Some(Name(mod.path)))
}

//destructor
def transform(id: cps.Id, operations: List[cps.Property]): MutableMap[Name, Definition] =
  val map: MutableMap[Name, Definition] = MutableMap() 
  operations.foreach(operation => 
  map += (Name(id.toString + "." + operation.id.toString) -> 
        Definition(id.toString + "." + operation.id.toString, List(Rule(List(Ctr(id.toString + "/" + id.toString, (operations map (x => x match {case cps.Property(operation.id, _) => VarPattern(Some(operation.id.toString)); case _ => VarPattern(Some("_"))})))), Var(operation.id.toString))), false)))
  map

//ADT constructor
@targetName("transformWithConstructors")
def transform(data: cps.Id, ctors: List[cps.Constructor]): MutableMap[Name, Definition] = 
  val defMap: MutableMap[Name, Definition] = MutableMap() 
  ctors.foreach(x => defMap += (Name(x.id.toString) -> Definition(x.id.toString, List(Rule(x.fields map idToPattern, chainApp(Var(data.toString + "/" + x.id.toString), x.fields map idToVar))), false)))
  defMap

def transform(decls: List[cps.Declaration], map: MutableMap[Name, Adt]): MutableMap[Name, Adt]= decls match {
  case Nil => map
  case cps.Declaration.Data(id, ctors) :: rest => transform(rest, map += (Name(id.toString) -> transform(ctors, Adt(MutableMap(), false))))
  case cps.Declaration.InterfaceDecl(id, operations) :: rest => 
    transform(rest, map += (Name(id.toString) -> Adt(MutableMap(Name(id.toString) -> (operations map (x=>x.id.toString()))), false)))
}

def transform(decl: cps.Extern)(using env: Environment): Verbatim = decl match {
  case cps.Extern.Def(id, params, body) =>
    Verbatim.Def(id.name.name, params map idToPattern, transform(body))
  case cps.Extern.Include(contents) => Verbatim.Include(contents)
}

def transform(template: Template[cps.Expr]): String = 
  //intercalate(template.strings, template.args).mkString // keine map
  intercalate(template.strings, template.args map exprToString).mkString // keine map

def transform(definition: cps.Definition)(using env: Environment): Definition = 
  //println(definition)
  definition match {
  //params + body => rule(patterns, body)
  case cps.Definition.Function(name, params, cont, body) => 
    Definition(name.name.name, List(Rule((params map idToPattern) :+ VarPattern(Some(cont.name.name)), transform(body)(using env))), false)
  case cps.Definition.Let(id, expr) => Definition(id.name.name, List(Rule(List(VarPattern(Some(id.name.name))), transform(expr))), false)
}

def transform(term: cps.Term)(using env: Environment): Term = term match {
  case cps.Term.AppCont(id, arg) => App(Auto, idToVar(id), transform(arg))
  case cps.Term.App(id, args, cont) =>
    chainApp(transform(id), (args map transform):+idToVar(cont))
  case cps.Term.Scope(definitions, body) => transform(definitions, body)
  case cps.Term.If(cond, thn, els) => 
    //println(term)
    Swt(List(transform(cond)), List(Rule(List(NumPattern(NumCtr.Num(0))), transform(els)), Rule(List(VarPattern(Some("_"))), transform(thn))))
  case cps.Term.Match(scrutinee, clauses, None) =>
    Mat(List(transform(scrutinee)), clauses map ((id, blockLit) => transform(id, blockLit, scrutinee)(using env)))
  case cps.Term.Match(scrutinee, clauses, Some(default)) =>
    println(term)
    Mat(List(transform(scrutinee)), (clauses map ((id, blockLit) => transform(id, blockLit, scrutinee)(using env))) :+ Rule(List(VarPattern(Some("_"))), transform(default)))
  case cps.Term.Let(name, expr, rest) => Let(idToPattern(name), transform(expr), transform(rest))
  case cps.Term.LetCont(name, param, body, rest) => Let(idToPattern(name), Lam(Auto, Some(param.name.name), transform(body)(using env)), transform(rest)) 
  case cps.Term.Fun(name, params, cont, body) => println(term); ???
  case cps.Term.Reset(ev, body) => chainApp(chainLam(List("ev", "k"), transform(body)(using env)), (List(Var("lift"), Var("pure"), Var("k")))) //(@ev @k transfomr(body)) lift pure k
  case cps.Term.Shift(ev, cont, body) => App(Auto, App(Auto, idToVar(ev), chainLam(List("kTemp", "k"), Let(idToPattern(cont), chainLam(List("eTemp", "xTemp"), App(Auto, Var("eTemp"), App(Auto, Var("kTemp"), Var("xTemp")))), transform(body)(using env)))), Var("k"))//((ev (@kTemp @k let cont = @eTemp @xtemp (eTemp (kTemp xTemp)); transform(body)(using env)) k)
}

def transform(id: cps.Id, blockLit: cps.BlockLit, scrutinee: cps.Expr)(using env: Environment): Rule = blockLit match {
  case cps.Expr.BlockLit(params, body) => 
    Rule(List(VarPattern(Some(env.getName(id)))), chainLet(params map idToPattern, env.getFields(id, exprToString(scrutinee)) map (x => Var(x)), transform(body)(using env))) //let params = scrutinee.fields, transform(body)(using env)
}

def transform(definitions: List[cps.Definition], body: cps.Term)(using env: Environment): Term = definitions match {
  case Nil => transform(body)(using env)
  case _ => definitions.head match {
    case cps.Definition.Let(id, bindings) => Let(idToPattern(id), transform(bindings), transform(definitions.tail, body))
    case cps.Definition.Function(name, params, cont, body) => ???
  }
}

def transform(expr: cps.Expr)(using env: Environment): Term = expr match {
  case cps.Expr.Lit(n) => Num(n)
  case cps.Expr.Var(name) => idToVar(name)
  case cps.Expr.PureApp(b, args) => chainApp(transform(b), (args map transform))
  case cps.Expr.Box(b) => transform(b)
  case cps.Expr.Run(t) => transform(t)
  case cps.Expr.BlockLit(params, body) => chainLam(params map (x => x.toString()), transform(body)(using env))
  case cps.Expr.Make(data, tag, vargs) => chainApp(idToVar(data), vargs map transform)
  case cps.Expr.Select(target, field) => ???
  case cps.Member(interface, field, tpe) => App(Auto, Var(tpe.name.toString + "." + field.toString), transform(interface))
  case cps.New(impl) => chainApp(Var(impl.interface._1.name.name + "/" + impl.interface._1.name), impl.operations map (x => transform(x.implementation: cps.Expr)))
  case cps.Expr.Evidence(lifts) => lifts.foldRight(Var("here")) {(l,x) => l match { //rekursiv besser
    case cps.Lift.Try() => ???
    case cps.Lift.Reg() => ???
    case cps.Lift.Var(ev) => App(Auto, App(Auto, Var("plus"), Var(ev.name.name)), x)
  }}
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
def chainApp(name: Term, args: List[Term]): Term = { //add parameter body
  val reverseArgs = args.reverse
  chainAppHelper(name, reverseArgs)
}

def chainAppHelper(name: Term, args: List[Term]): Term = args match {
 case Nil => name
 case head :: tail => App(Auto, chainAppHelper(name, tail), head)
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

def chainLet(patterns: List[Pattern], values: List[Term], body: Term): Term = patterns match { //assuming patterns and values have the same size
  case Nil => body
  case head :: Nil => body //head = k,
  case head :: next => Let(head, values.head, chainLet(next, values.tail, body))
}
 

def exprToString(expr: cps.Expr): String = expr match {
    case cps.Var(name) => name.name.name
    case cps.Expr.Lit(n) => n.toString()
    case _ => ???
}

def idToPattern(id: Id): Pattern = {
  val updatedName = id.name.name.replace('$', '.')
  VarPattern(Some(updatedName))
}

def idToVar(id: Id): Var = {
  val updatedName = id.name.name.replace('$', '.')
  Var(updatedName)
}

def idToName(id: Id): Name = Name(id.name.name)