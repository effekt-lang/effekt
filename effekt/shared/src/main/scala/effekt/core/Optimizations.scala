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
      Try(dealiasing(body), handlers.map(dealiasing))

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

    case New(impl) =>
      New(dealiasing(impl))

def dealiasing(pure: Pure)(using aliases: Map[Id, Id]): Pure =
  pure match
    case ValueVar(id, annotatedType) =>
      if (aliases.contains(id))
        var og = aliases(id)
        while (aliases.contains(og))
          og = aliases(og)
        ValueVar(og, annotatedType)

      else pure

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(dealiasing(b), targs, vargs.map(dealiasing))

    case Select(target, field, annotatedType) =>
      Select(dealiasing(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(dealiasing(b), annotatedCapture)

def dealiasing(impl: Implementation)(using aliases: Map[Id, Id]): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(dealiasing))

def dealiasing(op: Operation)(using aliases: Map[Id, Id]): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, dealiasing(body))

def removeUnusedFunctions(start: Tree|Definition, count: Map[Id, Int]): Tree|Definition =
  val rm = count.filter((_, n) => n == 0).keySet

  start match
    case m: ModuleDecl =>
      removeUnusedFunctionsWorker(m)(using rm)

    case a: Argument =>
      removeUnusedFunctionsWorker(a)(using rm)

    case e: Expr =>
      removeUnusedFunctionsWorker(e)(using rm)

    case s: Stmt =>
      removeUnusedFunctionsWorker(s)(using rm)

    case i: Implementation =>
      removeUnusedFunctionsWorker(i)(using rm)

    case d: Definition =>
      removeUnusedFunctionsWorker(d)(using rm)

    case x => x

def removeUnusedFunctionsWorker(arg: Argument)(using rm: Set[Id]): Argument =
  arg match
    case p: Pure =>
      removeUnusedFunctionsWorker(p)

    case b: Block =>
      removeUnusedFunctionsWorker(b)

def removeUnusedFunctionsWorker(module: ModuleDecl)(using rm: Set[Id]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.filter{
        case Definition.Def(id, _) => !rm.contains(id)
        case _ => true
      }.map(removeUnusedFunctionsWorker), exports)

def removeUnusedFunctionsWorker(definition: Definition)(using rm: Set[Id]): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, removeUnusedFunctionsWorker(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, removeUnusedFunctionsWorker(binding))

def removeUnusedFunctionsWorker(expression: Expr)(using rm:Set[Id]): Expr =
  expression match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(removeUnusedFunctionsWorker(b), targs, vargs.map(removeUnusedFunctionsWorker), bargs.map(removeUnusedFunctionsWorker))

    case Run(s) =>
      Run(removeUnusedFunctionsWorker(s))

    case p: Pure =>
      removeUnusedFunctionsWorker(p)

def removeUnusedFunctionsWorker(statement: Stmt)(using rm: Set[Id]): Stmt =
  statement match
    case Scope(definitions, body) =>
      val defs = definitions.filter {
        case Definition.Def(id, _) => !rm.contains(id)
        case _ => true
      }.map(removeUnusedFunctionsWorker)

      if(defs.isEmpty) removeUnusedFunctionsWorker(body)
      else Scope(defs, removeUnusedFunctionsWorker(body))

    case Return(p) =>
      Return(removeUnusedFunctionsWorker(p))

    case Val(id, binding, body) =>
      Val(id, removeUnusedFunctionsWorker(binding), removeUnusedFunctionsWorker(body))

    case App(callee, targs, vargs, bargs) =>
      App(removeUnusedFunctionsWorker(callee), targs, vargs.map(removeUnusedFunctionsWorker), bargs.map(removeUnusedFunctionsWorker))

    case If(cond, thn, els) =>
      If(removeUnusedFunctionsWorker(cond), removeUnusedFunctionsWorker(thn), removeUnusedFunctionsWorker(els))

    case Match(scrutinee, clauses, default) =>
      Match(removeUnusedFunctionsWorker(scrutinee), clauses.map{case (c, b) => (c, removeUnusedFunctionsWorker(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(removeUnusedFunctionsWorker(s))
          case None => None)

    case State(id, init, region, body) =>
      State(id, removeUnusedFunctionsWorker(init), region, removeUnusedFunctionsWorker(body))

    case Try(body, handlers) =>
      Try(removeUnusedFunctionsWorker(body), handlers.map(removeUnusedFunctionsWorker))

    case Region(body) =>
      Region(removeUnusedFunctionsWorker(body))

    case h: Hole =>
      h

def removeUnusedFunctionsWorker(block: Block)(using rm: Set[Id]): Block =
  block match
    case b: BlockVar =>
      b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, removeUnusedFunctionsWorker(body))

    case Member(b, field, annotatedType) =>
      Member(removeUnusedFunctionsWorker(b), field, annotatedType)

    case Unbox(p) =>
      Unbox(removeUnusedFunctionsWorker(p))

    case New(impl) =>
      New(removeUnusedFunctionsWorker(impl))

def removeUnusedFunctionsWorker(pure: Pure)(using rm: Set[Id]): Pure =
  pure match
    case v: ValueVar =>
      v

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(removeUnusedFunctionsWorker(b), targs, vargs.map(removeUnusedFunctionsWorker))

    case Select(target, field, annotatedType) =>
      Select(removeUnusedFunctionsWorker(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(removeUnusedFunctionsWorker(b), annotatedCapture)

def removeUnusedFunctionsWorker(impl: Implementation)(using rm: Set[Id]): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(removeUnusedFunctionsWorker))

def removeUnusedFunctionsWorker(op: Operation)(using rm: Set[Id]): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, removeUnusedFunctionsWorker(body))

def staticArgumentTransformation(start: Tree|Definition): Tree|Definition =
  start match
    case m: ModuleDecl =>
      staticArgumentTransformationWorker(m)(using constructCallGraph(m).filter((id, fs) => fs.contains(id)).keySet)

    case a: Argument =>
      staticArgumentTransformationWorker(a)(using constructCallGraph(a).filter((id, fs) => fs.contains(id)).keySet)

    case e: Expr =>
      staticArgumentTransformationWorker(e)(using constructCallGraph(e).filter((id, fs) => fs.contains(id)).keySet)

    case s: Stmt =>
      staticArgumentTransformationWorker(s)(using constructCallGraph(s).filter((id, fs) => fs.contains(id)).keySet)

    case i: Implementation =>
      staticArgumentTransformationWorker(i)(using constructCallGraph(i).filter((id, fs) => fs.contains(id)).keySet)

    case d: Definition =>
      staticArgumentTransformationWorker(d)(using constructCallGraph(d).filter((id, fs) => fs.contains(id)).keySet)

    case _ =>
      start

def staticArgumentTransformationWorker(module: ModuleDecl)(using recursiveFunctions: Set[Id]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.map(staticArgumentTransformationWorker), exports)

def staticArgumentTransformationWorker(definition: Definition)(using recursiveFunctions: Set[Id]): Definition =
  definition match
    case d@Definition.Def(id, block) =>
      if(recursiveFunctions.contains(id))
        val staticPs = findStaticArguments(d)
        if(staticPs.nonEmpty)
          val transformed = transformStaticArguments(d, staticPs)
          transformed match
            case Definition.Def(id, block) => Definition.Def(id, staticArgumentTransformationWorker(block))

        else Definition.Def(id, staticArgumentTransformationWorker(block))
      else Definition.Def(id, staticArgumentTransformationWorker(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, staticArgumentTransformationWorker(binding))

def staticArgumentTransformationWorker(arg: Argument)(using recursiveFunctions: Set[Id]): Argument =
  arg match
    case pure: Pure =>
      staticArgumentTransformationWorker(pure)

    case block: Block =>
      staticArgumentTransformationWorker(block)

def staticArgumentTransformationWorker(expr: Expr)(using recursiveFunctions: Set[Id]): Expr =
  expr match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(staticArgumentTransformationWorker(b), targs, vargs.map(staticArgumentTransformationWorker), bargs.map(staticArgumentTransformationWorker))

    case Run(s) =>
      Run(staticArgumentTransformationWorker(s))

    case p: Pure =>
      staticArgumentTransformationWorker(p)

def staticArgumentTransformationWorker(statement: Stmt)(using recursiveFunctions: Set[Id]): Stmt =
  statement match
    case Scope(definitions, body) =>
      Scope(definitions.map(staticArgumentTransformationWorker), staticArgumentTransformationWorker(body))

    case Return(expr) =>
      Return(staticArgumentTransformationWorker(expr))

    case Val(id, binding, body) =>
      Val(id, staticArgumentTransformationWorker(binding), staticArgumentTransformationWorker(body))

    case App(callee, targs, vargs, bargs) =>
      App(staticArgumentTransformationWorker(callee), targs, vargs.map(staticArgumentTransformationWorker), bargs.map(staticArgumentTransformationWorker))

    case If(cond, thn, els) =>
      If(staticArgumentTransformationWorker(cond), staticArgumentTransformationWorker(thn), staticArgumentTransformationWorker(els))

    case Match(scrutinee, clauses, default) =>
      Match(staticArgumentTransformationWorker(scrutinee), clauses.map{case (id, b) => (id, staticArgumentTransformationWorker(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(staticArgumentTransformationWorker(s))
          case None => None)

    case State(id, init, region, body) =>
      State(id, staticArgumentTransformationWorker(init), region, staticArgumentTransformationWorker(body))

    case Try(body, handlers) =>
      Try(staticArgumentTransformationWorker(body), handlers.map(staticArgumentTransformationWorker))

    case Region(body) =>
      Region(staticArgumentTransformationWorker(body))

    case h: Hole =>
      h

def staticArgumentTransformationWorker(block: Block)(using recursiveFunctions: Set[Id]): Block =
  block match
    case b: BlockVar =>
      b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, staticArgumentTransformationWorker(body))

    case Member(block, field, annotatedTpe) =>
      Member(staticArgumentTransformationWorker(block), field, annotatedTpe)

    case Unbox(p) =>
      Unbox(staticArgumentTransformationWorker(p))

    case New(impl) =>
      New(staticArgumentTransformationWorker(impl))

def staticArgumentTransformationWorker(pure: Pure)(using recursiveFunctions: Set[Id]): Pure =
  pure match
    case v: ValueVar =>
      v

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(b, targs, vargs.map(staticArgumentTransformationWorker))

    case Select(target, field, annotatedType) =>
      Select(staticArgumentTransformationWorker(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(staticArgumentTransformationWorker(b), annotatedCapture)

def staticArgumentTransformationWorker(impl: Implementation)(using recursiveFunctions: Set[Id]): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(staticArgumentTransformationWorker))

def staticArgumentTransformationWorker(op: Operation)(using recursiveFunctions: Set[Id]): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, staticArgumentTransformationWorker(body))

def transformStaticArguments(definition: Definition.Def, params: StaticParams): Definition.Def =
  definition match
    case Definition.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
      val (ti, ci, vi, bi) = params.unpackIndices
      val workerName = symbols.TmpBlock()

      val newTParams = tparams.zipWithIndex.filter(x => !ti.contains(x._2)).map(_._1)
      val newCParams = cparams.zipWithIndex.filter(x => !ci.contains(x._2)).map(_._1)
      val newVParams = vparams.zipWithIndex.filter(x => !vi.contains(x._2)).map(_._1)
      val newBParams = bparams.zipWithIndex.filter(x => !bi.contains(x._2)).map(_._1)

      val newBody = BlockLit(newTParams, newCParams, newVParams, newBParams, replaceCalls(body)(using workerName, params))

      val worker = Definition.Def(workerName, newBody)

      Definition.Def(id, BlockLit(tparams, cparams, vparams, bparams, Scope(List(worker),
        App(BlockVar(workerName, newBody.tpe, newBody.capt),
          newTParams.map(ValueType.Var(_)),
          newVParams.map{case ValueParam(id, tpe) => ValueVar(id, tpe)},
          newBParams.map{case BlockParam(id, tpe) => BlockVar(id, tpe, Set(id))}))))
    case _ =>
      definition

//TODO: Muss hier nur App betrachtet werden oder auch andere BlockVariablen?
def replaceCalls(statement: Stmt)(using newName: Id, params: StaticParams): Stmt =
  statement match
    case Scope(definitions, body) =>
      Scope(definitions.map(replaceCalls), replaceCalls(body))

    case Return(expr) =>
      Return(replaceCalls(expr))

    case Val(id, binding, body) =>
      Val(id, replaceCalls(binding), replaceCalls(body))

    case App(callee@BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCaptures), targs, vargs, bargs) =>
      val (ti, ci, vi, bi) = params.unpackIndices
      val (_, _, _, bs) = params.unpackParams
      if(id == params.id)
        App(BlockVar(newName,
          BlockType.Function(tparams.zipWithIndex.filter(x => !ti.contains(x._2)).map(_._1),
            cparams.zipWithIndex.filter(x => !ci.contains(x._2)).map(_._1),
            vparams.zipWithIndex.filter(x => !vi.contains(x._2)).map(_._1),
            bparams.zipWithIndex.filter(x => !bi.contains(x._2)).map(_._1),
            result), annotatedCaptures ++ bs),
          targs.zipWithIndex.filter(x => !ti.contains(x._2)).map(_._1),
          vargs.zipWithIndex.filter(x => !vi.contains(x._2)).map(_._1),
          bargs.zipWithIndex.filter(x => !bi.contains(x._2)).map(_._1))

      else App(callee, targs, vargs.map(replaceCalls), bargs.map(replaceCalls))

    case App(callee, targs, vargs, bargs) =>
      App(replaceCalls(callee), targs, vargs.map(replaceCalls), bargs.map(replaceCalls))

    case If(cond, thn, els) =>
      If(replaceCalls(cond), replaceCalls(thn), replaceCalls(els))

    case Match(scrutinee, clauses, default) =>
      Match(replaceCalls(scrutinee), clauses.map{case (id, b) => (id, replaceCalls(b).asInstanceOf[BlockLit])}, default match
        case Some(s) => Some(replaceCalls(s))
        case None => None)

    case State(id, init, region, body) =>
      State(id, replaceCalls(init), region, replaceCalls(body))

    case Try(body, handlers) =>
      Try(replaceCalls(body), handlers.map(replaceCalls))

    case Region(body) =>
      Region(replaceCalls(body))

    case h: Hole =>
      h

def replaceCalls(definition: Definition)(using newName: Id, params: StaticParams): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, replaceCalls(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, replaceCalls(binding))

def replaceCalls(expr: Expr)(using newName: Id, params: StaticParams): Expr =
  expr match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(replaceCalls(b), targs, vargs.map(replaceCalls), bargs.map(replaceCalls))

    case Run(s) =>
      Run(replaceCalls(s))

    case p: Pure =>
      replaceCalls(p)

def replaceCalls(block: Block)(using newName: Id, params: StaticParams): Block =
  block match
    case b:BlockVar => //TODO: Modify this also
      b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, replaceCalls(body))

    case Member(block, field, annotatedTpe) =>
      Member(replaceCalls(block), field, annotatedTpe)

    case Unbox(pure) =>
      Unbox(replaceCalls(pure))

    case New(impl) =>
      New(replaceCalls(impl))

def replaceCalls(pure: Pure)(using newName: Id, params: StaticParams): Pure =
  pure match
    case v:ValueVar =>
      v

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(replaceCalls(b), targs, vargs.map(replaceCalls))

    case Select(target, field, annotatedType) =>
      Select(replaceCalls(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(replaceCalls(b), annotatedCapture)

def replaceCalls(impl: Implementation)(using newName: Id, params: StaticParams): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(replaceCalls))

def replaceCalls(op: Operation)(using newName: Id, params: StaticParams): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, replaceCalls(body))

/*
def substitute(tree: Tree,
               tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam],
               targs: List[ValueType], vargs: List[Pure], bargs: List[Block]): Tree =
  val tSubst = (tparams zip targs).toMap
  val cSubst = (cparams zip bargs.map(_.capt)).toMap
  val vSubst = (vparams.map(_.id) zip vargs).toMap
  val bSubst = (bparams.map(_.id) zip bargs).toMap

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
*/
//TODO: Inlining, Constant Propagation, Case-of-known-case