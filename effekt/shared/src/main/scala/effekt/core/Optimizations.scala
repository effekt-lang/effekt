package effekt
package core

import scala.collection.{GenMap, mutable}

def dealiasing(module: ModuleDecl)(using aliases: Map[Id, Id]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.filter{
        case Definition.Def(id, _) => !aliases.contains(id)
        case _ => true
      }.map(dealiasing), exports)

def dealiasing(definition: Definition)(using aliases: Map[Id, Id]): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, dealiasing(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, dealiasing(binding))

def dealiasing(expression: Expr)(using aliases: Map[Id, Id]): Expr =
  expression match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(dealiasing(b), targs, vargs.map(dealiasing), bargs.map(dealiasing))

    case Run(s) =>
      Run(dealiasing(s))

    case p: Pure =>
      dealiasing(p)

def dealiasing(statement: Stmt)(using aliases: Map[Id, Id]): Stmt =
  statement match
    case Scope(definitions, body) =>
      Scope(definitions.filter{
        case Definition.Def(id, _) => !aliases.contains(id)
        case _ => true
      }.map(dealiasing), dealiasing(body))

    case Return(p) =>
      Return(dealiasing(p))

    case Val(id, binding, body) =>
      Val(id, dealiasing(binding), dealiasing(body))

    case App(callee, targs, vargs, bargs) =>
      App(dealiasing(callee), targs, vargs.map(dealiasing), bargs.map(dealiasing))

    case If(cond, thn, els) =>
      If(dealiasing(cond), dealiasing(thn), dealiasing(els))

    case Match(scrutinee, clauses, default) =>
      Match(dealiasing(scrutinee), clauses.map{(c,b) => (c, dealiasing(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(dealiasing(s))
          case None => None
      )

    case State(id, init, region, body) =>
      State(id, dealiasing(init), region, dealiasing(body))

    case Try(body, handlers) =>
      Try(dealiasing(body), handlers)

    case Region(body) =>
      Region(dealiasing(body))

    case h: Hole =>
      h

def dealiasing(block: Block)(using aliases: Map[Id, Id]): Block =
  block match
    case BlockVar(id, annotatedTpe, annotatedCapt) =>
      if (aliases.contains(id))
        var og = aliases(id)
        while (aliases.contains(og))
          og = aliases(og)
        BlockVar(og, annotatedTpe, annotatedCapt)

      else block

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, dealiasing(body))

    case Member(b, field, annotatedType) =>
      Member(dealiasing(b), field, annotatedType)

    case Unbox(p) =>
      Unbox(dealiasing(p))

    case n:New =>
      n

def dealiasing(pure: Pure)(using aliases: Map[Id, Id]): Pure =
  pure match
    case v: ValueVar =>
      v

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(dealiasing(b), targs, vargs.map(dealiasing))

    case Select(target, field, annotatedType) =>
      Select(dealiasing(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(dealiasing(b), annotatedCapture)

def removeUnusedFunctions(tree: Tree, count: Map[Id, Int]): Tree =
  val rm = count.filter((_, n) => n == 0).keySet

  tree match
    case m: ModuleDecl =>
      removeUnusedFunctions(m)(using rm)

    case a: Argument =>
      removeUnusedFunctions(a)(using rm)

    case e: Expr =>
      removeUnusedFunctions(e)(using rm)

    case s: Stmt =>
      removeUnusedFunctions(s)(using rm)

    case x => x

def removeUnusedFunctions(definition: Definition, count: Map[Id, Int]): Definition =
  val rm = count.filter((_, n) => n == 0).keySet
  removeUnusedFunctions(definition)(using rm)

def removeUnusedFunctions(arg: Argument)(using rm: Set[Id]): Argument =
  arg match
    case p: Pure =>
      removeUnusedFunctions(p)

    case b: Block =>
      removeUnusedFunctions(b)

def removeUnusedFunctions(module: ModuleDecl)(using rm: Set[Id]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.filter{
        case Definition.Def(id, _) => !rm.contains(id)
        case _ => true
      }.map(removeUnusedFunctions), exports)

def removeUnusedFunctions(definition: Definition)(using rm: Set[Id]): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, removeUnusedFunctions(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, removeUnusedFunctions(binding))

def removeUnusedFunctions(expression: Expr)(using rm:Set[Id]): Expr =
  expression match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(removeUnusedFunctions(b), targs, vargs.map(removeUnusedFunctions), bargs.map(removeUnusedFunctions))

    case Run(s) =>
      Run(removeUnusedFunctions(s))

    case p: Pure =>
      removeUnusedFunctions(p)

def removeUnusedFunctions(statement: Stmt)(using rm: Set[Id]): Stmt =
  statement match
    case Scope(definitions, body) => // TODO: if definitions = empty return only body?
      Scope(definitions.filter {
        case Definition.Def(id, _) => !rm.contains(id)
        case _ => true
      }.map(removeUnusedFunctions), removeUnusedFunctions(body))

    case Return(p) =>
      Return(removeUnusedFunctions(p))

    case Val(id, binding, body) =>
      Val(id, removeUnusedFunctions(binding), removeUnusedFunctions(body))

    case App(callee, targs, vargs, bargs) =>
      App(removeUnusedFunctions(callee), targs, vargs.map(removeUnusedFunctions), bargs.map(removeUnusedFunctions))

    case If(cond, thn, els) =>
      If(removeUnusedFunctions(cond), removeUnusedFunctions(thn), removeUnusedFunctions(els))

    case Match(scrutinee, clauses, default) =>
      Match(removeUnusedFunctions(scrutinee), clauses.map{case (c, b) => (c, removeUnusedFunctions(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(removeUnusedFunctions(s))
          case None => None)

    case State(id, init, region, body) =>
      State(id, removeUnusedFunctions(init), region, removeUnusedFunctions(body))

    case Try(body, handlers) =>
      Try(removeUnusedFunctions(body), handlers)

    case Region(body) =>
      Region(removeUnusedFunctions(body))

    case h: Hole =>
      h

def removeUnusedFunctions(block: Block)(using rm: Set[Id]): Block =
  block match
    case b: BlockVar =>
      b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, removeUnusedFunctions(body))

    case Member(b, field, annotatedType) =>
      Member(removeUnusedFunctions(b), field, annotatedType)

    case Unbox(p) =>
      Unbox(removeUnusedFunctions(p))

    case n: New =>
      n

def removeUnusedFunctions(pure: Pure)(using rm: Set[Id]): Pure =
  pure match
    case v: ValueVar =>
      v

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(removeUnusedFunctions(b), targs, vargs.map(removeUnusedFunctions))

    case Select(target, field, annotatedType) =>
      Select(removeUnusedFunctions(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(removeUnusedFunctions(b), annotatedCapture)

//TODO: rework wrapper

def inlineUniqueFunctions(module: ModuleDecl, summary: Map[Id, Block], count: Map[Id, Int]): ModuleDecl =
  var sum = summary.filter((id, _) => count.contains(id) && count(id) == 1)
  //TODO: Remove mutually recursive functions
  sum = for {id -> block <- sum}
    yield id -> inlineUniqueFunctions(block)(using sum) //TODO: repeat until no changes

  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.filter{
        case Definition.Def(id, _) => !sum.contains(id)
        case _ => true
      }.map(inlineUniqueFunctions(_)(using sum)), exports)

def inlineUniqueFunctions(definition: Definition)(using summary: Map[Id, Block]): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, inlineUniqueFunctions(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, inlineUniqueFunctions(binding))

def inlineUniqueFunctions(expression: Expr)(using summary: Map[Id, Block]): Expr =
  expression match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(inlineUniqueFunctions(b), targs, vargs, bargs)

    case Run(s) =>
      Run(inlineUniqueFunctions(s))

    case p: Pure =>
      p

def inlineUniqueFunctions(statement: Stmt)(using summary: Map[Id, Block]): Stmt =
  statement match
    case Scope(definitions, body) =>
      Scope(definitions.filter {
        case Definition.Def(id, _) => !summary.contains(id)
        case _ => true
      }.map(inlineUniqueFunctions), inlineUniqueFunctions(body))

    case r: Return =>
      r

    case Val(id, binding, body) =>
      Val(id, inlineUniqueFunctions(binding), inlineUniqueFunctions(body))

    case App(callee, targs, vargs, bargs) =>
      callee match
        case BlockVar(id, _, _) =>
          if(summary.contains(id)) summary(id) match
            case BlockLit(tparams, cparams, vparams, bparams, body) =>
              substitute(body, tparams, cparams, vparams, bparams, targs, vargs, bargs).asInstanceOf[Stmt]

            case _ =>
              ??? //TODO: Does summary only contain BlockLit after dealiasing?
          else App(callee, targs, vargs, bargs.map(inlineUniqueFunctions))

        case _ => App(inlineUniqueFunctions(callee), targs, vargs, bargs.map(inlineUniqueFunctions))

    case If(cond, thn, els) =>
      If(cond, inlineUniqueFunctions(thn), inlineUniqueFunctions(els))

    case Match(scrutinee, clauses, default) =>
      Match(scrutinee, clauses.map{case (c, b) => (c, inlineUniqueFunctions(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(inlineUniqueFunctions(s))
          case None => None)

    case State(id, init, region, body) =>
      State(id, init, region, inlineUniqueFunctions(body))

    case Try(body, handlers) =>
      Try(inlineUniqueFunctions(body), handlers)

    case Region(body) =>
      Region(inlineUniqueFunctions(body))

    case h: Hole =>
      h

def inlineUniqueFunctions(block: Block)(using summary: Map[Id, Block]): Block =
  block match
    case b: BlockVar =>
      b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, inlineUniqueFunctions(body))

    case Member(b, field, annotatedType) =>
      Member(inlineUniqueFunctions(b), field, annotatedType)

    case u: Unbox =>
      u

    case n: New =>
      n
// TODO: tpe and capt variables don't need to be considered right?
def substitute(tree: Tree,
               tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam],
               targs: List[ValueType], vargs: List[Pure], bargs: List[Block]): Tree =
  var tSubst = (tparams zip targs).toMap
  var cSubst = (cparams zip bargs.map(_.capt)).toMap
  var vSubst = (vparams.map(_.id) zip vargs).toMap
  var bSubst = (bparams.map(_.id) zip bargs).toMap

  //substitute(tree)(using tSubst, cSubst, vSubst, bSubst) TODO: is match necessary here?
  tree match
    case m: ModuleDecl =>
      substitute(m)(using tSubst, cSubst, vSubst, bSubst)

    case a: Argument =>
      substitute(a)(using tSubst, cSubst, vSubst, bSubst)

    case e: Expr =>
      substitute(e)(using tSubst, cSubst, vSubst, bSubst)

    case p: Param =>
      substitute(p)(using tSubst, cSubst, vSubst, bSubst)

    case s: Stmt =>
      substitute(s)(using tSubst, cSubst, vSubst, bSubst)
    case _ =>
      tree

def substitute(module: ModuleDecl)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                                   vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.map(substitute), exports)

def substitute(definition: Definition)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                                       vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, substitute(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, substitute(binding))

def substitute(expression: Expr)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                                 vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Expr =
  expression match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(substitute(b), targs.map(Type.substitute(_, tSubst, cSubst)), vargs.map(substitute), bargs.map(substitute))

    case Run(s) =>
      Run(substitute(s))

    case p: Pure =>
      substitute(p)

def substitute(statement: Stmt)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                                vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Stmt =
  statement match
    case Scope(definitions, body) =>
      Scope(definitions.map(substitute), substitute(body)(using tSubst, cSubst, vSubst, bSubst -- definitions.map{
        case Definition.Def(id, _) => id
        case Definition.Let(id, _) => id
      }))

    case Return(expr) =>
      Return(substitute(expr))

    case Val(id, binding, body) =>
      Val(id, substitute(binding), substitute(body)(using tSubst, cSubst, vSubst - id, bSubst - id)) //TODO: Val id in which map?

    case App(callee, targs, vargs, bargs) =>
      App(substitute(callee), targs.map(Type.substitute(_, tSubst, cSubst)), vargs.map(substitute), bargs.map(substitute))

    case If(cond, thn, els) =>
      If(substitute(cond), substitute(thn), substitute(els))

    case Match(scrutinee, clauses, default) =>
      Match(substitute(scrutinee), clauses.map{case (id, b) => (id, substitute(b).asInstanceOf[BlockLit])}, //TODO: clauses id replacable?
        default match
          case Some(s) => Some(substitute(s))
          case None => None)

    case State(id, init, region, body) =>
      State(id, substitute(init), region, substitute(body))

    case Try(body, handlers) =>
      Try(substitute(body), handlers)

    case Region(body) =>
      Region(substitute(body))

    case Hole() =>
      Hole()

def substitute(block: Block)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                             vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Block =
  block match
    case BlockVar(id, annotatedTpe, annotatedCapt) =>
      if(bSubst.contains(id)) bSubst(id)
      BlockVar(id, Type.substitute(annotatedTpe, tSubst, cSubst), Type.substitute(annotatedCapt, cSubst))

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams.map(substitute(_).asInstanceOf[ValueParam]), bparams.map(substitute(_).asInstanceOf[BlockParam]), substitute(body))

    case Member(block, field, annotatedTpe) =>
      Member(substitute(block), field, Type.substitute(annotatedTpe, tSubst, cSubst))

    case Unbox(pure) =>
      Unbox(substitute(pure))

    case n: New =>
      n

def substitute(param: Param)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                             vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Param =
  param match
    case ValueParam(id, tpe) =>
      ValueParam(id, Type.substitute(tpe, tSubst, cSubst))

    case BlockParam(id, tpe) =>
      BlockParam(id, Type.substitute(tpe, tSubst, cSubst))

def substitute(pure: Pure)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                           vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Pure =
  pure match
    case ValueVar(id, annotatedType) =>
      if(vSubst.contains(id)) vSubst(id)
      ValueVar(id, Type.substitute(annotatedType, tSubst, cSubst))

    case Literal(value, annotatedType) =>
      Literal(value, Type.substitute(annotatedType, tSubst, cSubst))

    case PureApp(b, targs, vargs) =>
      PureApp(substitute(b), targs.map(Type.substitute(_, tSubst, cSubst)), vargs.map(substitute))

    case Select(target, field, annotatedType) =>
      Select(substitute(target), field, Type.substitute(annotatedType, tSubst, cSubst))

    case Box(b, annotatedCapture) =>
      Box(substitute(b), Type.substitute(annotatedCapture, cSubst))

def substitute(arg: Argument)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                              vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Argument =
  arg match
    case p: Pure =>
      substitute(p)

    case b: Block =>
      substitute(b)