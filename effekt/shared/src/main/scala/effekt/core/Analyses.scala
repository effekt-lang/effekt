package effekt
package core

import scala.annotation.targetName
import scala.collection.{GenMap, mutable}
//TODO: reorder functions
//TODO: List().flatMap{...} ? flatMap{...}.toMap ?

//TODO: add wrapper to remove main?
def collectAliases(module: ModuleDecl): Map[Id, Id] =
  module.definitions.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

def collectAliases(definition: Definition): Map[Id, Id] =
  definition match
    case Definition.Def(id, BlockVar(id2, _, _)) =>
      Map[Id, Id](id -> id2)

    case Definition.Def(_, block) =>
      collectAliases(block)

    case Definition.Let(_, binding) =>
      collectAliases(binding)

def collectAliases(expr: Expr): Map[Id, Id] =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      collectAliases(b) ++
        vargs.map(collectAliases).fold(Map[Id, Id]())(_ ++ _) ++
        bargs.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

    case Run(s) =>
      collectAliases(s)

    case p: Pure =>
       collectAliases(p)

def collectAliases(statement: Stmt): Map[Id, Id] =
  statement match
    case Scope(definitions, body) =>
      definitions.map(collectAliases).fold(Map[Id, Id]())(_ ++ _) ++ collectAliases(body)

    case Return(p) =>
      collectAliases(p)

    case Val(_, binding, body) =>
      collectAliases(binding) ++ collectAliases(body)

    case App(b, _, vargs, bargs) =>
      collectAliases(b) ++
        vargs.map(collectAliases).fold(Map[Id, Id]())(_ ++ _) ++
        bargs.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

    case If(cond, thn, els) =>
      collectAliases(cond) ++ collectAliases(thn) ++ collectAliases(els)

    case Match(scrutinee, clauses, default) =>
      collectAliases(scrutinee) ++
        clauses.map{case (_, b) => collectAliases(b)}.fold(Map[Id, Id]())(_ ++ _) ++
        {default match
          case Some(s: Stmt) => collectAliases(s)
          case _ => Map[Id, Id]()}

    case State(_, init, _, body) =>
      collectAliases(init) ++ collectAliases(body)

    case Try(body, _) =>
      collectAliases(body)

    case Region(body) =>
      collectAliases(body)

    case _: Hole =>
      Map[Id, Id]()

def collectAliases(block: Block): Map[Id, Id] =
  block match
    case _: BlockVar =>
      Map[Id, Id]()

    case BlockLit(_, _, _, _, body) =>
      collectAliases(body)

    case Member(b, _, _) =>
      collectAliases(b)

    case Unbox(p) =>
      collectAliases(p)

    case _: New =>
      Map[Id, Id]()

def collectAliases(pure: Pure): Map[Id, Id] =
  pure match
    case _:ValueVar =>
      Map[Id, Id]()

    case _:Literal =>
      Map[Id, Id]()

    case PureApp(b, _, vargs) =>
      collectAliases(b) ++ vargs.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

    case Select(target, _, _) =>
      collectAliases(target)

    case Box(b, _) =>
      collectAliases(b)

def collectFunctionDefinitions(module: ModuleDecl): Map[Id,Block] =
  module.definitions.map(collectFunctionDefinitions).fold(Map[Id,Block]())(_ ++ _)

def collectFunctionDefinitions(definition: Definition): Map[Id, Block] =
  definition match
    case Definition.Def(id, block) =>
      Map[Id, Block](id -> block) ++ collectFunctionDefinitions(block)

    case Definition.Let(_, binding) =>
      collectFunctionDefinitions(binding)

def collectFunctionDefinitions(expr: Expr): Map[Id, Block] =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      collectFunctionDefinitions(b) ++
        vargs.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _) ++
        bargs.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _)

    case Run(s) =>
      collectFunctionDefinitions(s)

    case p: Pure =>
      collectFunctionDefinitions(p)

def collectFunctionDefinitions(statement: Stmt): Map[Id, Block] =
  statement match
    case Scope(definitions, body) =>
      definitions.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _) ++
        collectFunctionDefinitions(body)

    case Return(p) =>
      collectFunctionDefinitions(p)

    case Val(_, binding, body) =>
      collectFunctionDefinitions(binding) ++ collectFunctionDefinitions(body)

    case App(callee, _, vargs, bargs) =>
      collectFunctionDefinitions(callee) ++
        vargs.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _) ++
        bargs.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _)

    case If(cond, thn, els) =>
      collectFunctionDefinitions(cond) ++ collectFunctionDefinitions(thn) ++ collectFunctionDefinitions(els)

    case Match(scrutinee, clauses, default: Option[Stmt]) =>
      collectFunctionDefinitions(scrutinee) ++
        clauses.map{case (_,b) => collectFunctionDefinitions(b)}.fold(Map[Id, Block]())(_ ++ _) ++
        {default match
          case Some(s: Stmt) => collectFunctionDefinitions(s)
          case _ => Map[Id, Block]()}

    case State(_, init, _, body) =>
      collectFunctionDefinitions(init) ++ collectFunctionDefinitions(body)

    case Try(body, _) =>
      collectFunctionDefinitions(body)

    case Region(body) =>
      collectFunctionDefinitions(body)

    case _: Hole =>
      Map[Id, Block]()

def collectFunctionDefinitions(block: Block): Map[Id, Block] =
  block match
    case _: BlockVar =>
      Map[Id, Block]()

    case BlockLit(_, _, _, _, body) =>
      collectFunctionDefinitions(body)

    case Member(b, _, _) =>
      collectFunctionDefinitions(b)

    case Unbox(p) =>
      collectFunctionDefinitions(p)

    case _: New =>
      Map[Id, Block]()

def collectFunctionDefinitions(pure: Pure): Map[Id, Block] =
  pure match
    case _: ValueVar =>
      Map[Id, Block]()

    case _: Literal =>
      Map[Id, Block]()

    case PureApp(b, _, vargs) =>
      collectFunctionDefinitions(b) ++
        vargs.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _)

    case Select(target, _, _) =>
      collectFunctionDefinitions(target)

    case Box(b, _) =>
      collectFunctionDefinitions(b)

def countFunctionCalls(tree: Tree): Map[Id, Int] =
  val res = mutable.Map[Id, Int]()

  tree match
    case m: ModuleDecl =>
      countFunctionCalls(m)(using res)

    case a: Argument =>
      countFunctionCalls(a)(using res)

    case e: Expr =>
      countFunctionCalls(e)(using res)

    case s: Stmt =>
      countFunctionCalls(s)(using res)

    case _ =>

  res.toMap[Id, Int] - Id("main")

def countFunctionCalls(definition: Definition): Map[Id, Int] =
  val res = mutable.Map[Id, Int]()
  countFunctionCalls(definition)(using res)
  res.toMap[Id, Int] - Id("main")

def countFunctionCalls(module: ModuleDecl)(using count: mutable.Map[Id, Int]): Unit =
  module.definitions.foreach(countFunctionCalls(_)(using count))

def countFunctionCalls(definition: Definition)(using count: mutable.Map[Id, Int]): Unit =
  definition match
    case Definition.Def(id, block) =>
      if(!count.contains(id)) count += (id -> 0)
      else countFunctionCalls(block)

    case Definition.Let(_, binding) =>
      countFunctionCalls(binding)

def countFunctionCalls(expr: Expr)(using count: mutable.Map[Id, Int]): Unit =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      countFunctionCalls(b)
      vargs.foreach(countFunctionCalls)
      bargs.foreach(countFunctionCalls)

    case Run(s) =>
      countFunctionCalls(s)

    case p: Pure =>
      countFunctionCalls(p)

def countFunctionCalls(statement: Stmt)(using count: mutable.Map[Id, Int]): Unit =
  statement match
    case Scope(definitions, body) =>
      definitions.foreach(countFunctionCalls(_)(using count))
      countFunctionCalls(body)

    case Return(p) =>
      countFunctionCalls(p)

    case Val(_, binding, body) =>
      countFunctionCalls(binding)
      countFunctionCalls(body)

    case App(b, _, vargs, bargs) =>
      countFunctionCalls(b)
      vargs.foreach(countFunctionCalls)
      bargs.foreach(countFunctionCalls)

    case If(cond, thn, els) =>
      countFunctionCalls(cond)
      countFunctionCalls(thn)
      countFunctionCalls(els)

    case Match(scrutinee, clauses, default) =>
      countFunctionCalls(scrutinee)
      clauses.foreach{(_, b) => countFunctionCalls(b)}
      default match
        case Some(s: Stmt) => countFunctionCalls(s)
        case _ =>

    case State(_, init, _, body) =>
      countFunctionCalls(init)
      countFunctionCalls(body)

    case Try(body, _) =>
      countFunctionCalls(body)

    case Region(body) =>
      countFunctionCalls(body)

    case _: Hole =>

def countFunctionCalls(block: Block)(using count: mutable.Map[Id, Int]): Unit =
  block match
    case BlockVar(id, _, _) =>
      if(count.contains(id)) count(id) += 1
      else count += (id -> 1)

    case BlockLit(_, _, _, _, body) =>
      countFunctionCalls(body)

    case Member(b, _, _) =>
      countFunctionCalls(b)

    case Unbox(p) =>
      countFunctionCalls(p)

    case _: New =>

def countFunctionCalls(arg: Argument)(using count: mutable.Map[Id, Int]): Unit =
  arg match
    case p: Pure =>
      countFunctionCalls(p)

    case b: Block =>
      countFunctionCalls(b)

def countFunctionCalls(pure: Pure)(using count: mutable.Map[Id, Int]): Unit =
  pure match
    case _: ValueVar =>
    case _: Literal =>

    case PureApp(b, _, vargs) =>
      countFunctionCalls(b)
      vargs.foreach(countFunctionCalls)

    case Select(target, _, _) =>
      countFunctionCalls(target)

    case Box(b, _) =>
      countFunctionCalls(b)


// TODO: Detect Mutual Recursion (call graph)
