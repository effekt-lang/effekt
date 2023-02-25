package effekt
package core

import scala.collection.{GenMap, mutable}

def rmIdKey[T](input: Map[Id, T], rm: Set[String]): Map[Id, T] =
  input.filter((x, _) => !rm.contains(x.name.name))

//TODO: make more general
def substitute(block: BlockLit, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]): Stmt =
  block match
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      val tSubst = (tparams zip targs).toMap
      val cSubst = (cparams zip bargs.map(_.capt)).toMap
      val vSubst = (vparams.map(_.id) zip vargs).toMap
      val bSubst = (bparams.map(_.id) zip bargs).toMap

      substitute(body)(using tSubst, cSubst, vSubst, bSubst)

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
      Val(id, substitute(binding), substitute(body)(using tSubst, cSubst, vSubst - id, bSubst))

    case App(callee, targs, vargs, bargs) =>
      App(substitute(callee), targs.map(Type.substitute(_, tSubst, cSubst)), vargs.map(substitute), bargs.map(substitute))

    case If(cond, thn, els) =>
      If(substitute(cond), substitute(thn), substitute(els))

    case Match(scrutinee, clauses, default) =>
      Match(substitute(scrutinee), clauses.map{case (id, b) => (id, substitute(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(substitute(s))
          case None => None)

    case State(id, init, region, body) =>
      State(id, substitute(init), region, substitute(body))

    case Try(body, handlers) =>
      Try(substitute(body), handlers.map(substitute))

    case Region(body) =>
      Region(substitute(body))

    case Hole() =>
      Hole()

def substitute(block: Block)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                             vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Block =
  block match
    case BlockVar(id, annotatedTpe, annotatedCapt) =>
      if(bSubst.contains(id)) bSubst(id)
      else BlockVar(id, Type.substitute(annotatedTpe, tSubst, cSubst), Type.substitute(annotatedCapt, cSubst))

    case BlockLit(tparams, cparams, vparams, bparams, body) => //TODO: Do I have to look at params?
      BlockLit(tparams, cparams, vparams, bparams, substitute(body))

    case Member(block, field, annotatedTpe) =>
      Member(substitute(block), field, Type.substitute(annotatedTpe, tSubst, cSubst))

    case Unbox(pure) =>
      Unbox(substitute(pure))

    case New(impl) =>
      New(substitute(impl))

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
      else ValueVar(id, Type.substitute(annotatedType, tSubst, cSubst))

    case Literal(value, annotatedType) =>
      Literal(value, Type.substitute(annotatedType, tSubst, cSubst))

    case PureApp(b, targs, vargs) =>
      PureApp(substitute(b), targs.map(Type.substitute(_, tSubst, cSubst)), vargs.map(substitute))

    case Select(target, field, annotatedType) =>
      Select(substitute(target), field, Type.substitute(annotatedType, tSubst, cSubst))

    case Box(b, annotatedCapture) =>
      Box(substitute(b), Type.substitute(annotatedCapture, cSubst))

def substitute(impl: Implementation)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                                     vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(substitute))

def substitute(op: Operation)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                              vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, substitute(body))

def renameBoundIds(module: ModuleDecl)(using newNames: Map[Id, Id]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      val initialNames = rmIdKey[Id](definitions.map{
        case Definition.Def(id, _) => Map[Id, Id](id -> symbols.TmpBlock())
        case Definition.Let(id, _) => Map[Id, Id](id -> symbols.TmpValue())}.fold(Map[Id, Id]())(_ ++ _), Set("main"))
      ModuleDecl(path, imports, declarations, externs, definitions.map(renameBoundIds(_)(using initialNames)), exports)

def renameBoundIds(definition: Definition)(using newNames: Map[Id, Id]): Definition =
  definition match
    case Definition.Def(id, block) =>
      if(id.name.name == "main") Definition.Def(id, renameBoundIds(block))
      else
        if(newNames.contains(id)) Definition.Def(newNames(id), renameBoundIds(block))
        else
          val newName = symbols.TmpBlock()
          Definition.Def(newName, renameBoundIds(block)(using newNames ++ Map[Id, Id](id -> newName)))

    case Definition.Let(id, binding) =>
      if (newNames.contains(id)) Definition.Let(newNames(id), renameBoundIds(binding))
      else
        val newName = symbols.TmpValue()
        Definition.Let(newName, renameBoundIds(binding)(using newNames ++ Map[Id, Id](id -> newName)))

def renameBoundIds(expr: Expr)(using newNames: Map[Id, Id]): Expr =
  expr match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(renameBoundIds(b), targs, vargs.map(renameBoundIds), bargs.map(renameBoundIds))

    case Run(s) =>
      Run(renameBoundIds(s))

    case p: Pure =>
      renameBoundIds(p)

def renameBoundIds(statement: Stmt)(using newNames: Map[Id, Id]): Stmt =
  statement match
    case Scope(definitions, body) =>
      val scopeNames = definitions.map{
        case Definition.Def(id, _) => Map[Id, Id](id -> symbols.TmpBlock())
        case Definition.Let(id, _) => Map[Id, Id](id -> symbols.TmpValue())}.fold(Map[Id, Id]())(_ ++ _)
      Scope(definitions.map(renameBoundIds(_)(using newNames ++ scopeNames)), renameBoundIds(body)(using newNames ++ scopeNames))

    case Return(expr) =>
      Return(renameBoundIds(expr))

    case Val(id, binding, body) =>
      if (newNames.contains(id)) Val(newNames(id), renameBoundIds(binding), renameBoundIds(body))
      else
        val newName = symbols.TmpValue()
        Val(newName, renameBoundIds(binding)(using newNames ++ Map[Id, Id](id -> newName)), renameBoundIds(body)(using newNames ++ Map[Id, Id](id -> newName)))

    case App(callee, targs, vargs, bargs) =>
      App(renameBoundIds(callee), targs, vargs.map(renameBoundIds), bargs.map(renameBoundIds))

    case If(cond, thn, els) =>
      If(renameBoundIds(cond), renameBoundIds(thn), renameBoundIds(els))

    case Match(scrutinee, clauses, default) =>
      Match(renameBoundIds(scrutinee), clauses.map{
        case (id, b) => (id, renameBoundIds(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(renameBoundIds(s))
          case None => None)

    case State(id, init, region, body) =>
      if(newNames.contains(id)) State(newNames(id), renameBoundIds(init), if (newNames.contains(region)) newNames(region) else region, renameBoundIds(body))
      else
        val newName = symbols.TmpBlock()
        State(newName,
          renameBoundIds(init)(using newNames ++ Map[Id, Id](id -> newName)),
          if (newNames.contains(region)) newNames(region) else region,
          renameBoundIds(body)(using newNames ++ Map[Id, Id](id -> newName)))

    case Try(body, handlers) =>
      Try(renameBoundIds(body), handlers.map(renameBoundIds))

    case Region(body) =>
      Region(renameBoundIds(body))

    case h: Hole =>
      h

def renameBoundIds(block: Block)(using newNames: Map[Id, Id]): Block =
  block match
    case b@BlockVar(id, annotatedTpe, annotatedCapt) =>
      if(newNames.contains(id)) BlockVar(newNames(id), annotatedTpe, annotatedCapt)
      else b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, renameBoundIds(body))

    case Member(block, field, annotatedTpe) =>
      Member(renameBoundIds(block), field, annotatedTpe)

    case Unbox(pure) =>
      Unbox(renameBoundIds(pure))

    case New(impl) =>
      New(renameBoundIds(impl))

def renameBoundIds(pure: Pure)(using newNames: Map[Id, Id]): Pure =
  pure match
    case v@ValueVar(id, annotatedType) =>
      if(newNames.contains(id)) ValueVar(newNames(id), annotatedType)
      else v

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(renameBoundIds(b), targs, vargs.map(renameBoundIds))

    case Select(target, field, annotatedType) =>
      Select(renameBoundIds(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(renameBoundIds(b), annotatedCapture)

def renameBoundIds(impl: Implementation)(using newNames: Map[Id, Id]): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(renameBoundIds))

def renameBoundIds(op: Operation)(using newNames: Map[Id, Id]): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, renameBoundIds(body))
