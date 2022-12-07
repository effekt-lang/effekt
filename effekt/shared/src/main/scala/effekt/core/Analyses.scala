package effekt
package core

import scala.collection.mutable
import effekt.symbols.* // TODO: Import individual symbols

// TODO: CollectFunctionDefinitions and Counting as one Function?
def collectFunctionDefinitions(module: ModuleDecl): Map[BlockSymbol,Block] =
  module.definitions.map(collectFunctionDefinitions).fold(Map[BlockSymbol,Block]())(_ ++ _)

def collectFunctionDefinitions(definition: Definition): Map[BlockSymbol, Block] =
  definition match
    case Def(id, _, block) =>
      Map[BlockSymbol, Block](id -> block) ++ collectFunctionDefinitions(block)
    case Let(_, _, binding) =>
      collectFunctionDefinitions(binding)

def collectFunctionDefinitions(statement: Stmt): Map[BlockSymbol, Block] =
  statement match
    case Scope(definitions, body) =>
      definitions.map(collectFunctionDefinitions).fold(Map[BlockSymbol, Block]())(_ ++ _) ++
        collectFunctionDefinitions(body)

    case _: Return =>
      Map[BlockSymbol, Block]()

    case Val(_, _, binding, body) =>
      collectFunctionDefinitions(binding) ++ collectFunctionDefinitions(body)

    case App(b, _, args) =>
      collectFunctionDefinitions(b) ++ args.map{
        case x:Block => collectFunctionDefinitions(x)
        case _ => Map[BlockSymbol, Block]()
      }.fold(Map[BlockSymbol, Block]())(_ ++ _)

    case If(_, thn, els) =>
      collectFunctionDefinitions(thn) ++ collectFunctionDefinitions(els)

    case Match(_, clauses, default) =>
      clauses.map{case (_,b) => collectFunctionDefinitions(b)}.fold(Map[BlockSymbol, Block]())(_ ++ _) ++
        default match
          case Some(s) => collectFunctionDefinitions(s)
          case None => Map[BlockSymbol, Block]()

    case State(_, _, _, body) =>
      collectFunctionDefinitions(body)

    case Try(body, _, _) =>
      collectFunctionDefinitions(body)

    case Region(body, _) =>
      collectFunctionDefinitions(body)

    case _: Hole.type =>
      Map[BlockSymbol, Block]()

def collectFunctionDefinitions(block: Block): Map[BlockSymbol, Block] =
  block match
    case _: BlockVar =>
      Map[BlockSymbol, Block]()

    case BlockLit(_, body) =>
      collectFunctionDefinitions(body)

    case Member(b, _) =>
      collectFunctionDefinitions(b)

    case _: Unbox =>
      Map[BlockSymbol, Block]()

    case _: New =>
      Map[BlockSymbol, Block]()

def collectAliases(module: ModuleDecl): Map[BlockSymbol, BlockSymbol] =
  module.definitions.map(collectAliases).fold(Map[BlockSymbol, BlockSymbol]())(_ ++ _)

def collectAliases(definition: Definition): Map[BlockSymbol, BlockSymbol] =
  definition match
    case Def(id, _, BlockVar(id2)) =>
      Map[BlockSymbol, BlockSymbol](id -> id2)

    case Def(_, _, block) =>
      collectAliases(block)

    case _ =>
      Map[BlockSymbol, BlockSymbol]()

def collectAliases(statement: Stmt): Map[BlockSymbol, BlockSymbol] =
  statement match
    case Scope(definitions, body) =>
      definitions.map(collectAliases).fold(Map[BlockSymbol, BlockSymbol]())(_ ++ _) ++ collectAliases(body)

    case _: Return =>
      Map[BlockSymbol, BlockSymbol]()

    case Val(_, _, binding, body) =>
      collectAliases(binding) ++ collectAliases(body)

    case App(b, _, args) =>
      collectAliases(b) ++ args.map{
        case x:Block => collectAliases(x)
        case _ => Map[BlockSymbol, BlockSymbol]()
      }.fold(Map[BlockSymbol, BlockSymbol]())(_ ++ _)

    case If(_, thn, els) =>
      collectAliases(thn) ++ collectAliases(els)

    case Match(_, clauses, default) =>
      clauses.map{case (_, b) => collectAliases(b)}.fold(Map[BlockSymbol, BlockSymbol]())(_ ++ _) ++
        default match
          case Some(s) => collectAliases(s)
          case None => Map[BlockSymbol, BlockSymbol]()

    case State(_, _, _, body) =>
      collectAliases(body)

    case Try(body, _, _) =>
      collectAliases(body)

    case Region(body, _) =>
      collectAliases(body)

    case _: Hole.type =>
      Map[BlockSymbol, BlockSymbol]()

def collectAliases(block: Block): Map[BlockSymbol, BlockSymbol] =
  block match
    case BlockVar(_) =>
      Map[BlockSymbol, BlockSymbol]()

    case BlockLit(_, body) =>
      collectAliases(body)

    case Member(b, _) =>
      collectAliases(b)

    case _: Unbox =>
      Map[BlockSymbol, BlockSymbol]()

    case _: New =>
      Map[BlockSymbol, BlockSymbol]()

def countFunctionCalls(module: ModuleDecl): Map[BlockSymbol, Int] =
  val res = mutable.Map[BlockSymbol, Int]()
  module.definitions.foreach(x => countFunctionCalls(x)(using res))
  // TODO: Remove main: res - BlockSymbol(main) ?
  res.toMap[BlockSymbol, Int]

def countFunctionCalls(definition: Definition)(using count: mutableMap[BlockSymbol, Int]): Unit =
  definition match
    case Definition.Def(id, _, block) =>
      if(!count.contains(id)) count += (id -> 0)
      else countFunctionCalls(block)

    case Definition.Let(_, _, binding) =>
      countFunctionCalls(binding)

def countFunctionCalls(statement: Statement)(using count: mutableMap[BlockSymbol, Int]): Unit =
  statement match
    case Scope(definitions, body) => // List[Definition], Stmt
      definitions.foreach(countFunctionCalls)
      countFunctionCalls(body)

    case _: Return =>

    case Val(_, _, binding, body) => // ValueSymbol, ValueType, Stmt, Stmt
      countFunctionCalls(binding)
      countFunctionCalls(body)

    case App(b, _, args) => // Block, List[Type], List[Argument]
      countFunctionCalls(b)
      args.foreach{
        case b: Block => countFunctionCalls(b)
        case _ =>
      }

    case If(_, thn, els) => //  Pure, Stmt, Stmt
      countFunctionCalls(thn)
      countFunctionCalls(els)

    case Match(_, clauses, default) => // Pure, List[Constructor, BlockLit], Option[Stmt]
      clauses.foreach{(_, b) => countFunctionCalls(b)}
      default match
        case Some(s) => countFunctionCalls(s)
        case None =>

    case State(_, _, _, body) => // Symbol, Pure, Symbol, Stmt
      countFunctionCalls(body)

    case Try(body, _, _) => // Block, ValueType, List[Implementation]
      countFunctionCalls(body)

    case Region(body, _) => // Block, ValueType
      countFunctionCalls(body)

    case _: Hole.type =>

def countFunctionCalls(block: Block)(using count: mutableMap[BlockSymbol, Int]): Unit =
  block match
    case BlockVar(id) => // BlockSymbol
      if(count.contains(id)) count(id) += 1
      else count += (id -> 1)

    case BlockLit(_, body) => // List[Param], Stmt
      countFunctionCalls(body)

    case Member(b, _) => // Block, TermSymbol
      countFunctionCalls(b)

    case _: Unbox =>

    case _: New =>


// TODO: Detect Mutual Recursion (call graph)

/*
statement match
  case Scope(definitions, body) => // List[Definition], Stmt
    ???

  case x:Return => // Pure
    x

  case Val(id, tpe, binding, body) => // ValueSymbol, ValueType, Stmt, Stmt
    ???

  case App(b, targs, args) => // Block, List[Type], List[Argument]
    ???

  case If(cond, thn, els) => //  Pure, Stmt, Stmt
    ???

  case Match(scrutinee, clauses, default) => // Pure, List[Constructor, BlockLit], Option[Stmt]
    ???

  case State(id, init, region, body) => // Symbol, Pure, Symbol, Stmt
    ???

  case Try(body, answerType, handler) => // Block, ValueType, List[Implementation]
    ???

  case Region(body, answerType) => // Block, ValueType
    ???

  case h: Hole.type =>
    h


block match
  case BlockVar(id) => // BlockSymbol
    ???

  case BlockLit(params, body) => // List[Param], Stmt
    ???

  case Member(b, field) => // Block, TermSymbol
    ???

  case x:Unbox =>

  case New(impl) => // Implementation
    ???
*/