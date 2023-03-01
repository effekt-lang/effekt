package effekt
package core

import scala.collection.{GenMap, mutable}

/*


*/

// substitutes all alias BlockVars with BlockVar of the original function
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

// Removes functions that are defined but never mentioned
// Also removes recursive functions, that are only mentioned within themselves
// Doesn't remove exports
def removeUnusedFunctions(start: ModuleDecl, count: Map[Id, Int], recursiveFunctions: Set[Id], exports: List[Id]): ModuleDecl =
  val calls = count.filter(!exports.contains(_))
  removeUnusedFunctionsWorker(start)(using calls, recursiveFunctions)

def removeUnusedFunctionsWorker(module: ModuleDecl)(using count: Map[Id, Int], recursiveFunctions: Set[Id]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.filter{
        case Definition.Def(id, body) =>
          if(recursiveFunctions.contains(id))
            val recursiveOccurences = countFunctionOccurences(body)
            !(recursiveOccurences.contains(id) && count.contains(id) && count(id) == recursiveOccurences(id))
          else
            !(count.contains(id) && count(id) == 0)
        case _ => true
      }.map(removeUnusedFunctionsWorker), exports)

def removeUnusedFunctionsWorker(definition: Definition)(using count: Map[Id, Int], recursiveFunctions: Set[Id]): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, removeUnusedFunctionsWorker(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, removeUnusedFunctionsWorker(binding))

def removeUnusedFunctionsWorker(expression: Expr)(using count: Map[Id, Int], recursiveFunctions: Set[Id]): Expr =
  expression match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(removeUnusedFunctionsWorker(b), targs, vargs.map(removeUnusedFunctionsWorker), bargs.map(removeUnusedFunctionsWorker))

    case Run(s) =>
      Run(removeUnusedFunctionsWorker(s))

    case p: Pure =>
      removeUnusedFunctionsWorker(p)

def removeUnusedFunctionsWorker(statement: Stmt)(using count: Map[Id, Int], recursiveFunctions: Set[Id]): Stmt =
  statement match
    case Scope(definitions, body) =>
      val defs = definitions.filter {
        case Definition.Def(id, _) =>
          if (recursiveFunctions.contains(id))
            val recursiveOccurences = countFunctionOccurences(body)
            !(recursiveOccurences.contains(id) && count.contains(id) && count(id) == recursiveOccurences(id))
          else
            !(count.contains(id) && count(id) == 0)
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
      Match(removeUnusedFunctionsWorker(scrutinee), clauses.map{case (id, b) => (id, removeUnusedFunctionsWorker(b).asInstanceOf[BlockLit])},
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

def removeUnusedFunctionsWorker(block: Block)(using count: Map[Id, Int], recursiveFunctions: Set[Id]): Block =
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

def removeUnusedFunctionsWorker(pure: Pure)(using count: Map[Id, Int], recursiveFunctions: Set[Id]): Pure =
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

def removeUnusedFunctionsWorker(impl: Implementation)(using count: Map[Id, Int], recursiveFunctions: Set[Id]): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(removeUnusedFunctionsWorker))

def removeUnusedFunctionsWorker(op: Operation)(using count: Map[Id, Int], recursiveFunctions: Set[Id]): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, removeUnusedFunctionsWorker(body))

// Applies SAT to Module. Only inspects recursive functions with static arguments
def staticArgumentTransformation(module: ModuleDecl, recursiveFunctions: Set[Id]): ModuleDecl =
  staticArgumentTransformationWorker(module)(using recursiveFunctions)

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

// Input definition is recursive function with static arguments
// Returns worker definition without static arguments and call of that worker
def transformStaticArguments(definition: Definition.Def, params: StaticParams): Definition.Def =
  definition match
    case Definition.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
      val (ti, ci, vi, bi) = params.unpackIndices
      val workerName = symbols.TmpBlock()

      val newTParams = tparams.zipWithIndex.filter(x => !ti.contains(x._2)).map(_._1) //TODO: refactor
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

// Replaces old calls to function before SAT with calls to worker.
// Needs StaticParams to remove static args from BlockType
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
    case b@BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCaptures) =>
      if(id ==params.id)
        val (ti, ci, vi, bi) = params.unpackIndices
        val (_, _, _, bs) = params.unpackParams
        BlockVar(newName,
          BlockType.Function(tparams.zipWithIndex.filter(x => !ti.contains(x._2)).map(_._1),
            cparams.zipWithIndex.filter(x => !ci.contains(x._2)).map(_._1),
            vparams.zipWithIndex.filter(x => !vi.contains(x._2)).map(_._1),
            bparams.zipWithIndex.filter(x => !bi.contains(x._2)).map(_._1),
            result), annotatedCaptures ++ bs)
      else b

    case b: BlockVar =>
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

// Inlines all functions contained in inlines Map
def inliningWorker(module: ModuleDecl)(using inlines: Map[Id, Block]): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.map(inliningWorker), exports)

def inliningWorker(definition: Definition)(using inlines: Map[Id, Block]): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, inliningWorker(block)(using inlines - id))

    case Definition.Let(id, binding) =>
      Definition.Let(id, inliningWorker(binding))

def inliningWorker(expr: Expr)(using inlines: Map[Id, Block]): Expr =
  expr match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(inliningWorker(b), targs, vargs.map(inliningWorker), bargs.map(inliningWorker))

    case Run(s) =>
      Run(inliningWorker(s))

    case p: Pure =>
      inliningWorker(p)

def inliningWorker(statement: Stmt)(using inlines: Map[Id, Block]): Stmt =
  statement match
    case Scope(definitions, body) =>
      Scope(definitions.map{
        case d@Definition.Def(id, _) => inliningWorker(d)(using inlines - id)
        case l@Definition.Let(id,_) => inliningWorker(l)(using inlines - id)}, inliningWorker(body))

    case Return(expr) =>
      Return(inliningWorker(expr))

    case Val(id, binding, body) =>
      Val(id, inliningWorker(binding), inliningWorker(body))

    case App(b@BlockVar(id, _, _), targs, vargs, bargs) =>
      if(inlines.contains(id) && inlines(id).isInstanceOf[BlockLit])
        renameBoundIds(substitute(inlines(id).asInstanceOf[BlockLit], targs, vargs, bargs))(using Map[Id, Id]())
      else
        App(b, targs, vargs.map(inliningWorker), bargs.map(inliningWorker))

    case App(callee, targs, vargs, bargs) =>
      App(inliningWorker(callee), targs, vargs.map(inliningWorker), bargs.map(inliningWorker))

    case If(cond, thn, els) =>
      If(inliningWorker(cond), inliningWorker(thn), inliningWorker(els))

    case Match(scrutinee, clauses, default) =>
      Match(inliningWorker(scrutinee), clauses.map{case (i, b) => (i, inliningWorker(b).asInstanceOf[BlockLit])}, default match
        case Some(s) => Some(inliningWorker(s))
        case None => None)

    case State(id, init, region, body) =>
      State(id, inliningWorker(init), region, inliningWorker(body))

    case Try(body, handlers) =>
      Try(inliningWorker(body), handlers.map(inliningWorker))

    case Region(body) =>
      Region(inliningWorker(body))

    case h: Hole =>
      h

def inliningWorker(block: Block)(using inlines: Map[Id, Block]): Block =
  block match
    case b@BlockVar(id, _, _) =>
      if(inlines.contains(id)) renameBoundIds(inlines(id))(using Map[Id, Id]())
      else b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, inliningWorker(body))

    case Member(block, field, annotatedTpe) =>
      Member(inliningWorker(block), field, annotatedTpe)

    case Unbox(pure) =>
      Unbox(inliningWorker(pure))

    case New(impl) =>
      New(inliningWorker(impl))

def inliningWorker(pure: Pure)(using inlines: Map[Id, Block]): Pure =
  pure match
    case v: ValueVar =>
      v

    case l: Literal  =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(inliningWorker(b), targs, vargs.map(inliningWorker))

    case Select(target, field, annotatedType) =>
      Select(inliningWorker(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(inliningWorker(b), annotatedCapture)

def inliningWorker(impl: Implementation)(using inlines: Map[Id, Block]): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(inliningWorker))

def inliningWorker(op: Operation)(using inlines: Map[Id, Block]): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, inliningWorker(body))

// Wrapper, passes unique functions to inlining worker
def inlineUnique(module: ModuleDecl, bodies: Map[Id, Block], count: Map[Id, Int]): ModuleDecl =
  val inlines = bodies.filter((id, _) => count.contains(id) && count(id) == 1)
  inliningWorker(module)(using inlines)

// Wrapper, passes functions with max size of inlineThreshhold to inliningWorker
def inlineGeneral(module: ModuleDecl, bodies: Map[Id, Block], inlineThreshhold: Int): ModuleDecl =
  val callSizes = bodies.map((id, b) => (id, size(b)))
  val inlines = bodies.filter((id, b) => callSizes(id) <= inlineThreshhold)
  inliningWorker(module)(using inlines)

// Helper function for constantPropagation
// Returns input list without constants and Map of constants
def extractConstants(definitions: List[Definition]): (List[Definition], Map[Id, Literal]) =
  val constants = definitions.map{
    case Definition.Let(id, binding: Literal) => Map[Id, Literal](id -> binding)
    case _ => Map[Id, Literal]()}.fold(Map[Id, Literal]())(_ ++ _)

  val newDefinitions = definitions.filter{
    case Definition.Let(id, _) => !constants.contains(id)
    case _ => true
  }
  (newDefinitions, constants)

// Performs constant Propagation on Tree
def constantPropagation(module: ModuleDecl): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      val (defsNoConstants, constants) = extractConstants(definitions)
      ModuleDecl(path, imports, declarations, externs, defsNoConstants.map(constantPropagation(_)(using constants)), exports)

def constantPropagation(definition: Definition)(using constants: Map[Id, Literal]): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, constantPropagation(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, constantPropagation(binding))

def constantPropagation(expr: Expr)(using constants: Map[Id, Literal]): Expr =
  expr match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(constantPropagation(b), targs, vargs.map(constantPropagation), bargs.map(constantPropagation))

    case Run(s) =>
      Run(constantPropagation(s))

    case p: Pure =>
      constantPropagation(p)

def constantPropagation(statement: Stmt)(using constants: Map[Id, Literal]): Stmt =
  statement match
    case Scope(definitions, body) =>
      val (defsNoConstants, extraConstants) = extractConstants(definitions)
      val newConstants = extraConstants ++ constants
      Scope(defsNoConstants.map(constantPropagation(_)(using newConstants)), constantPropagation(body)(using newConstants))

    case Return(expr) =>
      Return(constantPropagation(expr))

    case Val(id, binding, body) =>
      Val(id, constantPropagation(binding), constantPropagation(body))

    case App(callee, targs, vargs, bargs) =>
      App(constantPropagation(callee), targs, vargs.map(constantPropagation), bargs.map(constantPropagation))

    case If(cond, thn, els) =>
      If(constantPropagation(cond), constantPropagation(thn), constantPropagation(els))

    case Match(scrutinee, clauses, default) =>
      Match(constantPropagation(scrutinee), clauses.map{case (id, b) => (id, constantPropagation(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(constantPropagation(s))
          case None => None)

    case State(id, init, region, body) =>
      State(id, constantPropagation(init), region, constantPropagation(body))

    case Try(body, handlers) =>
      Try(constantPropagation(body), handlers.map(constantPropagation))

    case Region(body) =>
      Region(constantPropagation(body))

    case h: Hole =>
      h

def constantPropagation(block: Block)(using constants: Map[Id, Literal]): Block =
  block match
    case b: BlockVar =>
      b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, constantPropagation(body))

    case Member(block, field, annotatedTpe) =>
      Member(constantPropagation(block), field, annotatedTpe)

    case Unbox(pure) =>
      Unbox(constantPropagation(pure))

    case New(impl) =>
      New(constantPropagation(impl))

def constantPropagation(pure: Pure)(using constants: Map[Id, Literal]): Pure =
  pure match
    case ValueVar(id, annotatedType) =>
      if(constants.contains(id)) constants(id)
      else ValueVar(id, annotatedType)

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(constantPropagation(b), targs, vargs.map(constantPropagation))

    case Select(target, field, annotatedType) =>
      Select(constantPropagation(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(constantPropagation(b), annotatedCapture)

def constantPropagation(impl: Implementation)(using constants: Map[Id, Literal]): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(constantPropagation))

def constantPropagation(op: Operation)(using constants: Map[Id, Literal]): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, constantPropagation(body))

// Apps applying arguments to BlockLits are replaced with body of BlockLit, filled with arguments
def betaReduction(module: ModuleDecl): ModuleDecl =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ModuleDecl(path, imports, declarations, externs, definitions.map(betaReduction), exports)

def betaReduction(definition: Definition): Definition =
  definition match
    case Definition.Def(id, block) =>
      Definition.Def(id, betaReduction(block))

    case Definition.Let(id, binding) =>
      Definition.Let(id, betaReduction(binding))

def betaReduction(expr: Expr): Expr =
  expr match
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(betaReduction(b), targs, vargs.map(betaReduction), bargs.map(betaReduction))

    case Run(s) =>
      Run(betaReduction(s))

    case p: Pure =>
      betaReduction(p)

def betaReduction(statement: Stmt): Stmt =
  statement match
    case Scope(definitions, body) =>
      Scope(definitions.map(betaReduction), betaReduction(body))

    case Return(expr) =>
      Return(betaReduction(expr))

    case Val(id, binding, body) =>
      Val(id, betaReduction(binding), betaReduction(body))

    case App(callee: BlockLit, targs, vargs, bargs) =>
      substitute(callee, targs, vargs, bargs)

    case App(callee, targs, vargs, bargs) =>
      App(betaReduction(callee), targs, vargs.map(betaReduction), bargs.map(betaReduction))

    case If(cond, thn, els) =>
      If(betaReduction(cond), betaReduction(thn), betaReduction(els))

    case Match(scrutinee, clauses, default) =>
      Match(betaReduction(scrutinee), clauses.map{case (id, b) => (id, betaReduction(b).asInstanceOf[BlockLit])},
        default match
          case Some(s) => Some(betaReduction(s))
          case None => None)

    case State(id, init, region, body) =>
      State(id, betaReduction(init), region, betaReduction(body))

    case Try(body, handlers) =>
      Try(betaReduction(body), handlers.map(betaReduction))

    case Region(body) =>
      Region(betaReduction(body))

    case h: Hole =>
      h

def betaReduction(block: Block): Block =
  block match
    case b: BlockVar =>
      b

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams, bparams, betaReduction(body))

    case Member(block, field, annotatedTpe) =>
      Member(betaReduction(block), field, annotatedTpe)

    case Unbox(pure) =>
      Unbox(betaReduction(pure))

    case New(impl) =>
      New(betaReduction(impl))

def betaReduction(pure: Pure): Pure =
  pure match
    case v: ValueVar =>
      v

    case l: Literal =>
      l

    case PureApp(b, targs, vargs) =>
      PureApp(betaReduction(b), targs, vargs.map(betaReduction))

    case Select(target, field, annotatedType) =>
      Select(betaReduction(target), field, annotatedType)

    case Box(b, annotatedCapture) =>
      Box(betaReduction(b), annotatedCapture)

def betaReduction(impl: Implementation): Implementation =
  impl match
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map(betaReduction))

def betaReduction(op: Operation): Operation =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      Operation(name, tparams, cparams, vparams, bparams, resume, betaReduction(body))
