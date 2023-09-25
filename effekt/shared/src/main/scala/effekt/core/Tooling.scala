package effekt
package core

import scala.collection.{GenMap, mutable}

/*
No functions in this file call other functions
rmIdKey, substitute, renameBoundIds
*/

//Filters keys from input, that have a name contained in rm
def rmIdKey[T](input: Map[Id, T], rm: Set[String]): Map[Id, T] =
  input.filter((x, _) => !rm.contains(x.name.name))

//Starting point for inlining, creates Maps(params -> args) and passes to normal substitute
def substitute(block: BlockLit, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]): Stmt =
  block match
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      val tSubst = (tparams zip targs).toMap
      val cSubst = (cparams zip bargs.map(_.capt)).toMap
      val vSubst = (vparams.map(_.id) zip vargs).toMap
      val bSubst = (bparams.map(_.id) zip bargs).toMap

      substitute(body)(using tSubst, cSubst, vSubst, bSubst)

//Replaces all variables contained in one of the Maps with their value
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
      Scope(definitions.map(substitute), substitute(body)(using tSubst, cSubst, vSubst, bSubst --
        definitions.flatMap{
          case Definition.Def(id, _) => List(id)
          case Definition.Let(id, _) => id}))

    case Return(exprs) =>
      Return(exprs.map(substitute))

    case Val(ids, binding, body) =>
      Val(ids, substitute(binding), substitute(body)(using tSubst, cSubst, vSubst -- ids, bSubst))

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
      if(bSubst.contains(region))
        bSubst(region) match
          //case _: BlockLit => Context.panic("Should not happen since block lits never have type Region")
          case BlockVar(x, _, _) => State(id, substitute(init), x, substitute(body)(using tSubst, cSubst, vSubst, bSubst - id))
          case b =>
            val name = symbols.TmpBlock()
            Scope(List(Definition.Def(name, b)),
              State(id,
                substitute(init)(using tSubst, cSubst, vSubst, bSubst - name),
                name,
                substitute(body)(using tSubst, cSubst, vSubst, bSubst -- Set(name, id))))

      else State(id, substitute(init), region, substitute(body)(using tSubst, cSubst, vSubst, bSubst - id))


    case Try(body, handlers) =>
      Try(substitute(body), handlers.map(substitute))

    case Region(body) =>
      Region(substitute(body))

    case h:Hole =>
      h

def substitute(block: Block)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                             vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Block =
  block match
    case b@BlockVar(id, _, _) =>
      if(bSubst.contains(id)) bSubst(id)
      else b

    case BlockLit(tparams, cparams, vparams, bparams, body) => //Removing these params from maps is really important.
      BlockLit(tparams, cparams, vparams, bparams,             //If not inlining functions after SAT will break them because params in worker are named the same.
        substitute(body)(using tSubst -- tparams, cSubst -- cparams, vSubst -- vparams.map(_.id), bSubst -- bparams.map(_.id)))

    case Member(block, field, annotatedTpe) =>
      Member(substitute(block), field, Type.substitute(annotatedTpe, tSubst, cSubst))

    case Unbox(pure) =>
      Unbox(substitute(pure))

    case New(impl) =>
      New(substitute(impl))

def substitute(pure: Pure)(using tSubst: Map[Id, ValueType], cSubst: Map[Id, Captures],
                           vSubst: Map[Id, Pure], bSubst: Map[Id, Block]): Pure =
  pure match
    case v@ValueVar(id, _) =>
      if(vSubst.contains(id)) vSubst(id)
      else v

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

//Generates new names for all definitions (Def, Let, Val, State), replaces old names in rest of program
def renameBoundIds(module: ModuleDecl)(using newNames: Map[Id, Id]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      val initialNames = rmIdKey[Id](definitions.flatMap{
        case Definition.Def(id, _) => List(Map[Id, Id](id -> symbols.TmpBlock()))
        case Definition.Let(ids, _) => ids.map(id => Map[Id, Id](id -> symbols.TmpValue()))}.fold(Map[Id, Id]())(_ ++ _), Set("main"))
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

    case Definition.Let(ids, binding) => { // TODO MRV 5
      val newNamesInDefinition = Map[Id, Id]()
      val newIDs = ids map {
        id =>
          if (newNames.contains(id)) newNames(id)
          else {
            val newId = symbols.TmpValue()
            newNamesInDefinition ++ Map[Id, Id](id -> newId)
            newId
          }
      }
      Definition.Let(newIDs, renameBoundIds(binding)(using newNames ++ newNamesInDefinition))
    }

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
      val scopeNames = definitions.flatMap{
        case Definition.Def(id, _) => List(Map[Id, Id](id -> symbols.TmpBlock()))
        case Definition.Let(ids, _) => ids map { id => Map[Id, Id](id -> symbols.TmpValue())}}.fold(Map[Id, Id]())(_ ++ _)
      Scope(definitions.map(renameBoundIds(_)(using newNames ++ scopeNames)), renameBoundIds(body)(using newNames ++ scopeNames))

    case Return(exprs) =>
      Return(exprs.map(renameBoundIds))

    case Val(ids, binding, body) => {
      val newNamesInDefinition = Map[Id, Id]()
      val newIDs = ids map {
        id =>
          if (newNames.contains(id)) newNames(id)
          else {
            val newId = symbols.TmpValue()
            newNamesInDefinition ++ Map[Id, Id](id -> newId)
            newId
          }
      }
      Val(newIDs, renameBoundIds(binding)(using newNames ++ newNamesInDefinition), renameBoundIds(body)(using newNames ++ newNamesInDefinition))
    }

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
