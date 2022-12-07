package effekt
package core

import scala.collection.mutable
import effekt.symbols.* // TODO: Import individual symbols

def dealiasing(module: ModuleDecl)(using aliases: Map[BlockSymbol, BlockSymbol]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.filter{
        case Def(id, _, _) => !aliases.contains(id)
        case _ => true
      }.map(dealiasing), exports)

def dealiasing(definition: Definition)(using aliases: Map[BlockSymbol, BlockSymbol]): Definition =
  definition match
    case Def(id, tpe, block) => Def(id, tpe, dealiasing(block))

    case _ => definition

def dealiasing(statement: Stmt)(using aliases: Map[BlockSymbol, BlockSymbol]): Stmt =
  statement match
    case Scope(definitions, body) => // List[Definition], Stmt
      Scope(definitions.filter{
        case Def(id, _, _) => !aliases.contains(id)
        case _ => true
      }.map(dealiasing), deliasing(body))

    case r: Return => // Pure
      r

    case Val(id, tpe, binding, body) => // ValueSymbol, ValueType, Stmt, Stmt
      Val(id, tpe, dealiasing(binding), dealiasing(body))

    case App(b, targs, args) => // Block, List[Type], List[Argument]
      App(dealiasing(b), targs, args.map{
        case b:Block => dealiasing(b)
        case x => x
      })

    case If(cond, thn, els) => //  Pure, Stmt, Stmt
      If(cond, dealiasing(thn), dealiasing(els))

    case Match(scrutinee, clauses, default) => // Pure, List[Constructor, BlockLit], Option[Stmt]
      Match(scrutinee, clauses.map{(c,b) => (c, dealiasing(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(dealiasing(s))
          case None => None
      )

    case State(id, init, region, body) => // Symbol, Pure, Symbol, Stmt
      State(id, init, region, dealiasing(body))

    case Try(body, answerType, handler) => // Block, ValueType, List[Implementation]
      Try(dealiasing(body), answerType, handler)

    case Region(body, answerType) => // Block, ValueType
      Region(dealiasing(body), answerType)

    case h: Hole.type =>
      h

def dealiasing(block: Block)(using aliases: Map[BlockSymbol, BlockSymbol]): Block =
  block match
    case BlockVar(id) => // BlockSymbol
      if (aliases.contains(id))
        var og = aliases(id)
        while (aliases.contains(og))
          og = aliases(og)
        BlockVar(og)

      else BlockVar(id)

    case BlockLit(params, body) => // List[Param], Stmt
      BlockLit(params, dealiasing(body))

    case Member(b, field) => // Block, TermSymbol
      Member(dealiasing(b), field)

    case u: Unbox =>
      u

    case n:New => // Implementation
      n

def removeUnusedFunctions(module: ModuleDecl)(using count: Map[BlockSymbol, Int]): ModuleDecl =
  val rm = count.filter((_, n) => n == 0).keySet

  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.filter{
        case Def(id, _, _) => !rm.contains(id)
        case _ => true
      }.map(removeUnusedFunctions(using rm)), exports)

def removeUnusedFunctions(definition: Definition)(using rm: Set[BlockSymbol]): Definition =
  definition match
    case Def(id, tpe, block) =>
      Def(id, tpe, removeUnusedFunctions(block))

    case _ => definition


def removeUnusedFunctions(statement: Stmt)(using rm: Set[BlockSymbol]): Stmt =
  statement match
    case Scope(definitions, body) => // List[Definition], Stmt
      Scope(definitions.filter {
        case Def(id, _, _) => !rm.contains(id)
        case _ => true
      }.map(removeUnusedFunctions), removeUnusedFunctions(body))

    case r: Return => // Pure
      r

    case Val(id, tpe, binding, body) => // ValueSymbol, ValueType, Stmt, Stmt
      Val(id, tpe, removeUnusedFunctions(binding), removeUnusedFunctions(body))

    case App(b, targs, args) => // Block, List[Type], List[Argument]
      App(removeUnusedFunctions(b), targs, args.map{
        case b: Block => removeUnusedFunctions(b)
        case x => x
      })

    case If(cond, thn, els) => //  Pure, Stmt, Stmt
      If(cond, removeUnusedFunctions(thn), removeUnusedFunctions(els))

    case Match(scrutinee, clauses, default) => // Pure, List[Constructor, BlockLit], Option[Stmt]
      Match(scrutinee, clauses.map{case (c, b) => (c, removeUnusedFunctions(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(removeUnusedFunctions(s))
          case None => None)

    case State(id, init, region, body) => // Symbol, Pure, Symbol, Stmt
      State(id, init, region, removeUnusedFunctions(body))

    case Try(body, answerType, handler) => // Block, ValueType, List[Implementation]
      Try(removeUnusedFunctions(body), answerType, handler)

    case Region(body, answerType) => // Block, ValueType
      Region(removeUnusedFunctions(body), answerType)

    case h: Hole.type =>
      h

def removeUnusedFunctions(block: Block)(using rm: Set[BlockSymbol]): Block =
  block match
    case b: BlockVar => // BlockSymbol
      b

    case BlockLit(params, body) => // List[Param], Stmt
      BlockLit(params, removeUnusedFunctions(body))

    case Member(b, field) => // Block, TermSymbol
      Member(removeUnusedFunctions(b), field)

    case u: Unbox =>
      u

    case n: New => // Implementation
      n

def inlineUniqueFunctions(module: ModuleDecl, summary: Map[BlockSymbol, Block], count: Map[BlockSymbol, Int]): ModuleDecl =
  var sum = summary.filter((id, _) => count.contains(id) && count(id) == 1)
  //TODO: Remove mutually recursive functions
  sum = for {id -> block <- sum}
    yield id -> inlineUniqueFunctions(block)(using sum) //TODO: repeat until no changes

  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.filter{
        case Def(id, _, _) => !sum.contains(id)
        case _ => true
      }.map(inlineUniqueFunctions)(using sum), exports)

def inlineUniqueFunctions(definition: Definition)(using summary: Map[BlockSymbol, Block]): Stmt =
  definition match
    case Def(id, tpe, block) =>
      Def(id,tpe, inlineUniqueFunctions(block))

    case l: Let =>
      l

def inlineUniqueFunctions(statement: Stmt)(using summary: Map[BlockSymbol, Block]): Stmt =
  statement match
    case Scope(definitions, body) => // List[Definition], Stmt
      Scope(definitions.filter {
        case Def(id, _, _) => !sum.contains(id)
        case _ => true
      }.map(inlineUniqueFunctions), inlineUniqueFunctions(body))

    case r: Return => // Pure
      r

    case Val(id, tpe, binding, body) => // ValueSymbol, ValueType, Stmt, Stmt
      Val(id, tpe, inlineUniqueFunctions(binding), inlineUniqueFunctions(body))

    case App(b, targs, args) => // Block, List[Type], List[Argument]
      if(b.isInstanceOf[BlockVar] && summary.contains(b.id))
        substitute(summary(b).body, summary(b).params, args)

      else App(inlineUniqueFunctions(b), targs, args.map{
        case b: Block => inlineUniqueFunctions(b)
        case x => x
      })

    case If(cond, thn, els) => //  Pure, Stmt, Stmt
      If(cond, inlineUniqueFunctions(thn), inlineUniqueFunctions(els))

    case Match(scrutinee, clauses, default) => // Pure, List[Constructor, BlockLit], Option[Stmt]
      Match(scrutinee, clauses.map{case (c, b) => (c, inlineUniqueFunctions(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(inlineUniqueFunctions(s))
          case None => None)

    case State(id, init, region, body) => // Symbol, Pure, Symbol, Stmt
      State(id, init, region, inlineUniqueFunctions(body))

    case Try(body, answerType, handler) => // Block, ValueType, List[Implementation]
      Try(inlineUniqueFunctions(body), answerType, handler) //TODO: Inlining possible here?

    case Region(body, answerType) => // Block, ValueType
      Region(inlineUniqueFunctions(body), answerType) //TODO: Inlining possible here?

    case h: Hole.type =>
      h

def inlineUniqueFunctions(block: Block)(using summary: Map[BlockSymbol, Block]): Block =
  block match
    case b: BlockVar => // BlockSymbol
      b

    case BlockLit(params, body) => // List[Param], Stmt
      BlockLit(params, inlineUniqueFunctions(body))

    case Member(b, field) => // Block, TermSymbol
      Member(inlineUniqueFunctions(b), field)

    case u: Unbox =>
      u

    case n: New => // Implementation
      n

def substitute(module: ModuleDecl, params: List[Param], args: List[Argument]): ModuleDecl =
  var valInserts = Map[ValueSymbol, Pure]()
  var blockInserts = Map[BlockSymbol, Block]()

  for (id -> arg <- params.map(x => x.id) zip args)
    (id, arg) match
      case (p: ValueSymbol, a: Pure) => valInserts += (p -> a)
      case (p: BlockSymbol, a: Block) => blockInserts += (p -> a)

  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.map(substitute(using valInserts, blockInserts)), exports)

def substitute(definition: Definition)(using valInserts: Map[ValueSymbol, Pure], blockInserts: Map[BlockSymbol, Block]): Definition =
  definition match
    case Def(id, tpe, block) =>
      Def(id, tpe, substitute(block)(using blockInserts - id))

    case Let(id, tpe, binding) =>
      Let(id, tpe, substitute(binding)(using valInserts - id))

def substitute(statement: Stmt)(using valInserts: Map[ValueSymbol, Pure], blockInserts: Map[BlockSymbol, Block]): Stmt =
  statement match
    case Scope(definitions, body) => // List[Definition], Stmt
      var boundBlock = Set[BlockSymbol]()
      var boundVal = Set[ValueSymbol]()

      for(d <- definitions)
        d match
          case Def(id, _, _) => boundBlock += id
          case Let(id, _, _) => boundVal += id // TODO: Is this necessary for ModuleDecl definitions as well?

      Scope(definitions.map(substitute), substitute(body)(using valInserts - boundVal, blockInserts - boundBlock))

    case Return(p) => // Pure
      Return(substitute(p))

    case Val(id, tpe, binding, body) => // ValueSymbol, ValueType, Stmt, Stmt
      Val(id, tpe, substitute(binding), substitute(body)(using valInserts - id))

    case App(b, targs, args) => // Block, List[Type], List[Argument]
      App(substitute(b), targs, args.map(substitute))

    case If(cond, thn, els) => //  Pure, Stmt, Stmt
      If(substitute(cond), substitute(thn), substitute(els))

    case Match(scrutinee, clauses, default) => // Pure, List[Constructor, BlockLit], Option[Stmt]
      Match(substitute(scrutinee), clauses.map{(c, b) => (c, substitute(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(substitute(s))
          case None => None)

    case State(id, init, region, body) => // Symbol, Pure, Symbol, Stmt
      State(id, substitute(init), region, substitute(body)) //TODO: Does id/region need to be removed from inserts?

    case Try(body, answerType, handler) => // Block, ValueType, List[Implementation]
      Try(substitute(body), answerType, handler)

    case Region(body, answerType) => // Block, ValueType
      Region(substitute(body), answerType)

    case h: Hole.type =>
      h

def substitute(block: Block)(using valInserts: Map[ValueSymbol, Pure], blockInserts: Map[BlockSymbol, Block]): Block =
  block match
    case BlockVar(id) => // BlockSymbol
      if(blockInserts.contains(id)) blockInserts(id)
      else BlockVar(id)

    case BlockLit(params, body) => // List[Param], Stmt
      BlockLit(params, substitute(body))

    case Member(b, field) => // Block, TermSymbol
      Member(substitute(b), field)

    case u: Unbox =>
      u

    case n: New => // Implementation
      n

def substitute(expression: Expr)(using valInserts: Map[ValueSymbol, Pure], blockInserts: Map[BlockSymbol, Block]): Expr =
  expression match
    case DirectApp(b, targs, args) =>
      DirectApp(substitute(b), targs, args.map(substitute))

    case Run(s, tpe) =>
      Run(substitute(s), tpe)

    case pure: Pure =>
      substitute(pure)

def substitute(pure: Pure)(using valInserts: Map[ValueSymbol, Pure], blockInserts: Map[BlockSymbol, Block]): Pure =
  pure match
    case ValueVar(id) =>
      if(valInserts.contains(id)) valInserts(id)
      else ValueVar(id)

    case l: Literal =>
      l

    case PureApp(b, targs, args) =>
      PureApp(substitute(b), targs, args.map(substitute))

    case Select(target, field) =>
      Select(substitute(target), field)

    case Box(b) =>
      Box(substitute(b))

def substitute(arg: Argument)(using valInserts: Map[ValueSymbol, Pure], blockInserts: Map[BlockSymbol, Block]): Argument =
  arg match
    case pure: Pure =>
      substitute(pure)

    case block: Block =>
      substitute(block)


/*
statement match
  case Scope(definitions, body) => // List[Definition], Stmt
    ???

  case r:Return => // Pure
    r

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
  case b: BlockVar => // BlockSymbol
    b

  case BlockLit(params, body) => // List[Param], Stmt
    ???

  case Member(b, field) => // Block, TermSymbol
    ???

  case u:Unbox =>
    u

  case n: New => // Implementation
    n
*/