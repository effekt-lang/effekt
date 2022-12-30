package effekt
package core

import scala.annotation.targetName
import scala.collection.{GenMap, mutable}
//TODO: reorder functions
//TODO: List().flatMap{...} ? flatMap{...}.toMap ?
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

def countFunctionCalls(start: Tree|Definition): Map[Id, Int] =
  val res = mutable.Map[Id, Int]()

  start match
    case m: ModuleDecl =>
      countFunctionCallsWorker(m)(using res)

    case a: Argument =>
      countFunctionCallsWorker(a)(using res)

    case e: Expr =>
      countFunctionCallsWorker(e)(using res)

    case s: Stmt =>
      countFunctionCallsWorker(s)(using res)

    case d: Definition =>
      countFunctionCallsWorker(d)(using res)

    case _ =>
  res.toMap[Id, Int]

def countFunctionCallsWorker(module: ModuleDecl)(using count: mutable.Map[Id, Int]): Unit =
  module.definitions.foreach(countFunctionCallsWorker(_)(using count))

def countFunctionCallsWorker(definition: Definition)(using count: mutable.Map[Id, Int]): Unit =
  definition match
    case Definition.Def(id, block) =>
      if(!count.contains(id))
        count += (id -> 0)
      countFunctionCallsWorker(block)

    case Definition.Let(_, binding) =>
      countFunctionCallsWorker(binding)

def countFunctionCallsWorker(expr: Expr)(using count: mutable.Map[Id, Int]): Unit =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      countFunctionCallsWorker(b)
      vargs.foreach(countFunctionCallsWorker)
      bargs.foreach(countFunctionCallsWorker)

    case Run(s) =>
      countFunctionCallsWorker(s)

    case p: Pure =>
      countFunctionCallsWorker(p)

def countFunctionCallsWorker(statement: Stmt)(using count: mutable.Map[Id, Int]): Unit =
  statement match
    case Scope(definitions, body) =>
      definitions.foreach(countFunctionCallsWorker(_)(using count))
      countFunctionCallsWorker(body)

    case Return(p) =>
      countFunctionCallsWorker(p)

    case Val(_, binding, body) =>
      countFunctionCallsWorker(binding)
      countFunctionCallsWorker(body)

    case App(b, _, vargs, bargs) =>
      countFunctionCallsWorker(b)
      vargs.foreach(countFunctionCallsWorker)
      bargs.foreach(countFunctionCallsWorker)

    case If(cond, thn, els) =>
      countFunctionCallsWorker(cond)
      countFunctionCallsWorker(thn)
      countFunctionCallsWorker(els)

    case Match(scrutinee, clauses, default) =>
      countFunctionCallsWorker(scrutinee)
      clauses.foreach{(_, b) => countFunctionCallsWorker(b)}
      default match
        case Some(s: Stmt) => countFunctionCallsWorker(s)
        case _ =>

    case State(_, init, _, body) =>
      countFunctionCallsWorker(init)
      countFunctionCallsWorker(body)

    case Try(body, _) =>
      countFunctionCallsWorker(body)

    case Region(body) =>
      countFunctionCallsWorker(body)

    case _: Hole =>

def countFunctionCallsWorker(block: Block)(using count: mutable.Map[Id, Int]): Unit =
  block match
    case BlockVar(id, _, _) =>
      if(count.contains(id)) count(id) += 1
      else count += (id -> 1)

    case BlockLit(_, _, _, _, body) =>
      countFunctionCallsWorker(body)

    case Member(b, _, _) =>
      countFunctionCallsWorker(b)

    case Unbox(p) =>
      countFunctionCallsWorker(p)

    case _: New =>

def countFunctionCallsWorker(arg: Argument)(using count: mutable.Map[Id, Int]): Unit =
  arg match
    case p: Pure =>
      countFunctionCallsWorker(p)

    case b: Block =>
      countFunctionCallsWorker(b)

def countFunctionCallsWorker(pure: Pure)(using count: mutable.Map[Id, Int]): Unit =
  pure match
    case _: ValueVar =>
    case _: Literal =>

    case PureApp(b, _, vargs) =>
      countFunctionCallsWorker(b)
      vargs.foreach(countFunctionCallsWorker)

    case Select(target, _, _) =>
      countFunctionCallsWorker(target)

    case Box(b, _) =>
      countFunctionCallsWorker(b)


// TODO: Detect Mutual Recursion (call graph)
