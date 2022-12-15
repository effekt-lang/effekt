package effekt
package core

import scala.collection.mutable

// TODO: CollectFunctionDefinitions and Counting as one Function?
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
    case DirectApp(b, _, _, bargs) =>
      collectFunctionDefinitions(b) ++ bargs.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _)

    case Run(s) =>
      collectFunctionDefinitions(s)

    case _: Pure =>
      Map[Id, Block]()

def collectFunctionDefinitions(statement: Stmt): Map[Id, Block] =
  statement match
    case Scope(definitions, body) =>
      definitions.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _) ++
        collectFunctionDefinitions(body)

    case _: Return =>
      Map[Id, Block]()

    case Val(_, binding, body) =>
      collectFunctionDefinitions(binding) ++ collectFunctionDefinitions(body)

    case App(callee, _, _, bargs) =>
      collectFunctionDefinitions(callee) ++ bargs.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _)

    case If(_, thn, els) =>
      collectFunctionDefinitions(thn) ++ collectFunctionDefinitions(els)

    case Match(_, clauses, default: Option[Stmt]) =>
      clauses.map{case (_,b) => collectFunctionDefinitions(b)}.fold(Map[Id, Block]())(_ ++ _) ++
        {default match
          case Some(s: Stmt) => collectFunctionDefinitions(s)
          case _ => Map[Id, Block]()}
    // TODO: List().flatMap{...} ? flatMap{...}.toMap ?
    case State(_, _, _, body) =>
      collectFunctionDefinitions(body)

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

    case _: Unbox =>
      Map[Id, Block]()

    case _: New =>
      Map[Id, Block]()

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
    case DirectApp(b, _, _, bargs) =>
      collectAliases(b) ++ bargs.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

    case Run(s) =>
      collectAliases(s)

    case _: Pure =>
      Map[Id, Id]()

def collectAliases(statement: Stmt): Map[Id, Id] =
  statement match
    case Scope(definitions, body) =>
      definitions.map(collectAliases).fold(Map[Id, Id]())(_ ++ _) ++ collectAliases(body)

    case _: Return =>
      Map[Id, Id]()

    case Val(_, binding, body) =>
      collectAliases(binding) ++ collectAliases(body)

    case App(b, _, _, bargs) =>
      collectAliases(b) ++ bargs.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

    case If(_, thn, els) =>
      collectAliases(thn) ++ collectAliases(els)

    case Match(_, clauses, default) =>
      clauses.map{case (_, b) => collectAliases(b)}.fold(Map[Id, Id]())(_ ++ _) ++
        {default match
          case Some(s: Stmt) => collectAliases(s)
          case _ => Map[Id, Id]()}

    case State(_, _, _, body) =>
      collectAliases(body)

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

    case _: Unbox =>
      Map[Id, Id]()

    case _: New =>
      Map[Id, Id]()

def countFunctionCalls(module: ModuleDecl): Map[Id, Int] =
  val res = mutable.Map[Id, Int]()
  module.definitions.foreach(x => countFunctionCalls(x)(using res))
  // TODO: Remove main
  res.toMap[Id, Int]

def countFunctionCalls(definition: Definition)(using count: mutable.Map[Id, Int]): Unit =
  definition match
    case Definition.Def(id, block) =>
      if(!count.contains(id)) count += (id -> 0)
      else countFunctionCalls(block)

    case Definition.Let(_, binding) =>
      countFunctionCalls(binding)

def countFunctionCalls(expr: Expr)(using count: mutable.Map[Id, Int]): Unit =
  expr match
    case DirectApp(b, _, _, bargs) =>
      countFunctionCalls(b)
      bargs.foreach(countFunctionCalls)

    case Run(s) =>
      countFunctionCalls(s)

    case _: Pure =>

def countFunctionCalls(statement: Stmt)(using count: mutable.Map[Id, Int]): Unit =
  statement match
    case Scope(definitions, body) =>
      definitions.foreach(countFunctionCalls)
      countFunctionCalls(body)

    case _: Return =>

    case Val(_, binding, body) =>
      countFunctionCalls(binding)
      countFunctionCalls(body)

    case App(b, _, _, bargs) =>
      countFunctionCalls(b)
      bargs.foreach(countFunctionCalls)

    case If(_, thn, els) =>
      countFunctionCalls(thn)
      countFunctionCalls(els)

    case Match(_, clauses, default) =>
      clauses.foreach{(_, b) => countFunctionCalls(b)}
      default match
        case Some(s: Stmt) => countFunctionCalls(s)
        case _ =>

    case State(_, _, _, body) =>
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

    case _: Unbox =>

    case _: New =>


// TODO: Detect Mutual Recursion (call graph)
