package effekt
package core

import scala.collection.{GenMap, mutable}

class StaticParamsUnfinished(fname: Id,
                             tparams: Array[Option[Id]],
                             cparams: Array[Option[Id]],
                             vparams: Array[Option[Id]],
                             bparams: Array[Option[Id]]):
  def unpackParams: (Array[Option[Id]], Array[Option[Id]], Array[Option[Id]], Array[Option[Id]]) =
    (tparams, cparams, vparams, bparams)

  def id: Id = fname

class StaticParams(fname: Id,
                   tparams: Set[Id],
                   cparams: Set[Id],
                   vparams: Set[Id],
                   bparams: Set[Id],
                   tIndices: List[Int],
                   cIndices: List[Int],
                   vIndices: List[Int],
                   bIndices: List[Int]):
  def id: Id = fname
  
  def unpackParams: (Set[Id], Set[Id], Set[Id], Set[Id]) =
    (tparams, cparams, vparams, bparams)

  def unpackIndices: (List[Int], List[Int], List[Int], List[Int]) =
    (tIndices, cIndices, vIndices, bIndices)
    
  def nonEmpty: Boolean =
    tparams.nonEmpty || cparams.nonEmpty || vparams.nonEmpty || bparams.nonEmpty


def collectAliases(module: ModuleDecl): Map[Id, Id] =
  module.definitions.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

def collectAliases(definition: Definition): Map[Id, Id] =
  definition match
    case Definition.Def(id, BlockVar(id2, _, _)) =>
      Map[Id, Id](id -> id2)

    case Definition.Def(_, block) =>
      collectAliases(block)

    case Definition.Let(id, ValueVar(id2, _)) =>
      Map[Id, Id](id -> id2)

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

    case Try(body, handlers) =>
      collectAliases(body) ++ handlers.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

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

    case New(impl) =>
      collectAliases(impl)

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

def collectAliases(impl: Implementation): Map[Id, Id] =
  impl match
    case Implementation(_, operations) =>
      operations.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

def collectAliases(op: Operation): Map[Id, Id] =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      collectAliases(body)

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

    case Try(body, handlers) =>
      collectFunctionDefinitions(body) ++ handlers.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _)

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

    case New(impl) =>
      collectFunctionDefinitions(impl)

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

def collectFunctionDefinitions(impl: Implementation): Map[Id, Block] =
  impl match
    case Implementation(_, operations) =>
      operations.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _)

def collectFunctionDefinitions(op: Operation): Map[Id, Block] =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      collectFunctionDefinitions(body)

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

    case i: Implementation =>
      countFunctionCallsWorker(i)(using res)

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

    case Try(body, handlers) =>
      countFunctionCallsWorker(body)
      handlers.foreach(countFunctionCallsWorker)

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

    case New(impl) =>
      countFunctionCallsWorker(impl)

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

def countFunctionCallsWorker(impl: Implementation)(using count: mutable.Map[Id, Int]): Unit =
  impl match
    case Implementation(_, operations) =>
      operations.foreach(countFunctionCallsWorker)

def countFunctionCallsWorker(op: Operation)(using ocunt: mutable.Map[Id, Int]): Unit =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      countFunctionCallsWorker(body)

def calledFunctions(expr: Expr): Set[Id] =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      calledFunctions(b) ++
        vargs.map(calledFunctions).fold(Set[Id]())(_ ++ _) ++
        bargs.map(calledFunctions).fold(Set[Id]())(_ ++ _)

    case Run(s) =>
      calledFunctions(s)

    case pure: Pure =>
      calledFunctions(pure)

def calledFunctions(statement: Stmt): Set[Id] =
  statement match
    case Scope(_, body) =>
      calledFunctions(body)

    case Return(expr) =>
      calledFunctions(expr)

    case Val(_, _, body) =>
      calledFunctions(body)

    case App(callee, _, vargs, bargs) =>
      {callee match
        case BlockVar(id, _, _) => Set(id)
        case _ => calledFunctions(callee)} ++
      vargs.map(calledFunctions).fold(Set[Id]())(_ ++ _) ++
      bargs.map(calledFunctions).fold(Set[Id]())(_ ++ _)

    case If(cond, thn, els) =>
      calledFunctions(cond) ++ calledFunctions(thn) ++ calledFunctions(els)
      
    case Match(scrutinee, clauses, default) =>
      calledFunctions(scrutinee) ++
        clauses.map{case (_, b) => calledFunctions(b)}.fold(Set[Id]())(_ ++ _) ++
        {default match
          case Some(s) => calledFunctions(s)
          case None => Set[Id]()}

    case State(_, init, _, body) =>
      calledFunctions(init) ++ calledFunctions(body)

    case Try(body, handlers) =>
      calledFunctions(body) ++
        handlers.map(calledFunctions).fold(Set[Id]())(_ ++ _)

    case Region(body) =>
      calledFunctions(body)

    case _:Hole =>
      Set[Id]()

def calledFunctions(block: Block): Set[Id] =
  block match
    case _: BlockVar =>
      Set[Id]()

    case BlockLit(_, _, _, _, body) =>
      calledFunctions(body)

    case Member(block, _, _) =>
      calledFunctions(block)

    case Unbox(pure) =>
      calledFunctions(pure)

    case New(impl) =>
      calledFunctions(impl)

def calledFunctions(pure: Pure): Set[Id] =
  pure match
    case _: ValueVar =>
      Set[Id]()

    case _: Literal=>
      Set[Id]()

    case PureApp(b, _, vargs) =>
      calledFunctions(b) ++ vargs.map(calledFunctions).fold(Set[Id]())(_ ++ _)

    case Select(target, _, _) =>
      calledFunctions(target)

    case Box(b, _) =>
      calledFunctions(b)

def calledFunctions(impl: Implementation): Set[Id] =
  impl match
    case Implementation(_, operations) =>
      operations.map(calledFunctions).fold(Set[Id]())(_ ++ _)

def calledFunctions(op: Operation): Set[Id] =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      calledFunctions(body)

def constructCallGraph(module: ModuleDecl): Map[Id, Set[Id]] =
  module.definitions.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _)

def constructCallGraph(definition: Definition): Map[Id, Set[Id]] =
  definition match
    case Definition.Def(id, block) =>
      Map[Id, Set[Id]](id -> calledFunctions(block)) ++ constructCallGraph(block)

    case Definition.Let(_, binding) =>
      constructCallGraph(binding)

def constructCallGraph(arg: Argument): Map[Id, Set[Id]] =
  arg match
    case pure: Pure =>
      constructCallGraph(pure)

    case block: Block =>
      constructCallGraph(block)

def constructCallGraph(expr: Expr): Map[Id, Set[Id]] =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      constructCallGraph(b) ++
        vargs.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _) ++
        bargs.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _)

    case Run(s) =>
      constructCallGraph(s)

    case p: Pure =>
      constructCallGraph(p)

def constructCallGraph(statement: Stmt): Map[Id, Set[Id]] =
  statement match
    case Scope(definitions, body) =>
      definitions.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _) ++ constructCallGraph(body)

    case Return(expr) =>
      constructCallGraph(expr)

    case Val(_, binding, body) =>
      constructCallGraph(binding) ++ constructCallGraph(body)

    case App(callee, _, vargs, bargs) =>
      constructCallGraph(callee) ++
        vargs.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _) ++
        bargs.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _)

    case If(cond, thn, els) =>
      constructCallGraph(cond) ++ constructCallGraph(thn) ++ constructCallGraph(els)

    case Match(scrutinee, clauses, default) =>
      constructCallGraph(scrutinee) ++
        clauses.map{case (_, b) => constructCallGraph(b)}.fold(Map[Id, Set[Id]]())(_ ++ _) ++
        {default match
          case Some(s) => constructCallGraph(s)
          case None => Map[Id, Set[Id]]()}

    case State(_, init, _, body) =>
      constructCallGraph(init) ++ constructCallGraph(body)

    case Try(body, handlers) =>
      constructCallGraph(body) ++ handlers.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _)

    case Region(body) =>
      constructCallGraph(body)

    case Hole() =>
      Map[Id, Set[Id]]()

def constructCallGraph(block: Block): Map[Id, Set[Id]] =
  block match
    case _: BlockVar =>
      Map[Id, Set[Id]]()

    case BlockLit(_, _, _, _, body) =>
      constructCallGraph(body)

    case Member(block, _, _) =>
      constructCallGraph(block)

    case Unbox(pure) =>
      constructCallGraph(pure)

    case New(impl) =>
      constructCallGraph(impl)

def constructCallGraph(pure: Pure): Map[Id, Set[Id]] =
  pure match
    case _: ValueVar =>
      Map[Id, Set[Id]]()

    case _: Literal =>
      Map[Id, Set[Id]]()

    case PureApp(b, _, vargs) =>
      constructCallGraph(b) ++ vargs.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _)

    case Select(target, _, _) =>
      constructCallGraph(target)

    case Box(b, _) =>
      constructCallGraph(b)

def constructCallGraph(impl: Implementation): Map[Id, Set[Id]] =
  impl match
    case Implementation(_, operations) =>
      operations.map(constructCallGraph).fold(Map[Id, Set[Id]]())(_ ++ _)

def constructCallGraph(op: Operation): Map[Id, Set[Id]] =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      constructCallGraph(body)

def findStaticArguments(start: Definition.Def): StaticParams =
  start match
    case Definition.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
      val ts: Array[Option[Id]] = tparams.map(x => Some(x)).toArray
      val cs: Array[Option[Id]] = cparams.map(x => Some(x)).toArray
      val vs: Array[Option[Id]] = vparams.map(x => Some(x.id)).toArray
      val bs: Array[Option[Id]] = bparams.map(x => Some(x.id)).toArray

      findStaticArgumentsWorker(body)(using StaticParamsUnfinished(id, ts, cs, vs, bs))
      StaticParams(id,
        ts.filter(_.isDefined).map(_.get).toSet[Id],
        cs.filter(_.isDefined).map(_.get).toSet[Id],
        vs.filter(_.isDefined).map(_.get).toSet[Id],
        bs.filter(_.isDefined).map(_.get).toSet[Id],
        ts.toList.zipWithIndex.map{case (p, i) => if(p.isDefined) i else None}.filter(_.isInstanceOf[Int]).asInstanceOf[List[Int]],
        cs.toList.zipWithIndex.map{case (p, i) => if(p.isDefined) i else None}.filter(_.isInstanceOf[Int]).asInstanceOf[List[Int]],
        vs.toList.zipWithIndex.map{case (p, i) => if(p.isDefined) i else None}.filter(_.isInstanceOf[Int]).asInstanceOf[List[Int]],
        bs.toList.zipWithIndex.map{case (p, i) => if(p.isDefined) i else None}.filter(_.isInstanceOf[Int]).asInstanceOf[List[Int]])

    case _ => StaticParams(Id("None"), Set[Id](), Set[Id](), Set[Id](), Set[Id](), List[Int](), List[Int](), List[Int](), List[Int]())

def findStaticArgumentsWorker(module: ModuleDecl)(using params: StaticParamsUnfinished): Unit =
  module.definitions.foreach(findStaticArgumentsWorker)

def findStaticArgumentsWorker(definition: Definition)(using params: StaticParamsUnfinished): Unit =
  definition match
    case Definition.Def(_, block) =>
      findStaticArgumentsWorker(block)

    case Definition.Let(_, binding) =>
      findStaticArgumentsWorker(binding)

def findStaticArgumentsWorker(arg: Argument)(using params: StaticParamsUnfinished): Unit =
  arg match
    case p: Pure =>
      findStaticArgumentsWorker(p)

    case b: Block =>
      findStaticArgumentsWorker(b)

def findStaticArgumentsWorker(expr: Expr)(using params: StaticParamsUnfinished): Unit =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      findStaticArgumentsWorker(b)
      vargs.foreach(findStaticArgumentsWorker)
      bargs.foreach(findStaticArgumentsWorker)

    case Run(s) =>
      findStaticArgumentsWorker(s)

    case p: Pure =>
      findStaticArgumentsWorker(p)

def findStaticArgumentsWorker(statement: Stmt)(using params: StaticParamsUnfinished): Unit =
  statement match
    case Scope(definitions, body) =>
      definitions.foreach(findStaticArgumentsWorker)
      findStaticArgumentsWorker(body)

    case Return(expr) =>
      findStaticArgumentsWorker(expr)

    case Val(_, binding, body) =>
      findStaticArgumentsWorker(binding)
      findStaticArgumentsWorker(body)

    case App(BlockVar(id, _, _), targs, vargs, bargs) =>
      var (tparams, cparams, vparams, bparams) = params.unpackParams
      if(id == params.id)
        targs.zipWithIndex.foreach{
          case (ValueType.Var(id), idx) => if(!tparams(idx).contains(id)) tparams(idx) = None
          case (_, idx) => tparams(idx) = None}
        vargs.zipWithIndex.foreach{
          case (ValueVar(id, _), idx) => if(!vparams(idx).contains(id)) vparams(idx) = None
          case (_, idx) => vparams(idx) = None}
        bargs.zipWithIndex.foreach{
          case (BlockVar(id, _, _), idx) =>
            if(!bparams(idx).contains(id))
              bparams(idx) = None
              cparams(idx) = None
          case (_, idx) =>
            bparams(idx) = None
            cparams(idx) = None}

      vargs.foreach(findStaticArgumentsWorker)
      bargs.foreach(findStaticArgumentsWorker)

    case App(callee, _, vargs, bargs) =>
      findStaticArgumentsWorker(callee)
      vargs.foreach(findStaticArgumentsWorker)
      bargs.foreach(findStaticArgumentsWorker)

    case If(cond, thn, els) =>
      findStaticArgumentsWorker(cond)
      findStaticArgumentsWorker(thn)
      findStaticArgumentsWorker(els)

    case Match(scrutinee, clauses, default) =>
      findStaticArgumentsWorker(scrutinee)
      clauses.foreach{(_,b) => findStaticArgumentsWorker(b)}
      default match
        case Some(s) => findStaticArgumentsWorker(s)
        case None =>

    case State(_, init, _, body) =>
      findStaticArgumentsWorker(init)
      findStaticArgumentsWorker(body)

    case Try(body, handlers) =>
      findStaticArgumentsWorker(body)
      handlers.foreach(findStaticArgumentsWorker)

    case Region(body) =>
      findStaticArgumentsWorker(body)

    case _: Hole =>

def findStaticArgumentsWorker(block: Block)(using params: StaticParamsUnfinished): Unit =
  block match
    case _: BlockVar =>

    case BlockLit(_, _, _, _, body) =>
      findStaticArgumentsWorker(body)

    case Member(block, _, _) =>
      findStaticArgumentsWorker(block)

    case Unbox(pure) =>
      findStaticArgumentsWorker(pure)

    case New(impl) =>
      findStaticArgumentsWorker(impl)

def findStaticArgumentsWorker(pure: Pure)(using params: StaticParamsUnfinished): Unit =
  pure match
    case _: ValueVar =>

    case _: Literal =>

    case PureApp(b, _, vargs) =>
      findStaticArgumentsWorker(b)
      vargs.foreach(findStaticArgumentsWorker)

    case Select(target, _, _) =>
      findStaticArgumentsWorker(target)

    case Box(b, _) =>
      findStaticArgumentsWorker(b)

def findStaticArgumentsWorker(impl: Implementation)(using params: StaticParamsUnfinished): Unit =
  impl match
    case Implementation(_, operations) =>
      operations.foreach(findStaticArgumentsWorker)

def findStaticArgumentsWorker(op: Operation)(using params: StaticParamsUnfinished): Unit =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      findStaticArgumentsWorker(body)

/*
Template:

def constructCallGraph(module: ModuleDecl): Map[Id, Set[Id]] =
  module match
    case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      ???

def constructCallGraph(definition: Definition): Map[Id, Set[Id]] =
  definition match
    case Definition.Def(id, block) =>
      ???

    case Definition.Let(id, binding) =>
      ???

def constructCallGraph(expr: Expr): Map[Id, Set[Id]] =
  expr match
    case DirectApp(b, targs, vargs, bargs) =>
      ???

    case Run(s) =>
      ???

    case pure: Pure =>
      ???

def constructCallGraph(statement: Stmt): Map[Id, Set[Id]] =
  statement match
    case Scope(definitions, body) =>
      ???

    case Return(expr) =>
      ???

    case Val(id, binding, body) =>
      ???

    case App(callee, targs, vargs, bargs) =>
      ???

    case If(cond, thn, els) =>
      ???

    case Match(scrutinee, clauses, default) =>
      ???

    case State(id, init, region, body) =>
      ???

    case Try(body, handlers) =>
      ???

    case Region(body) =>
      ???

    case Hole() =>
      ???

def constructCallGraph(block: Block): Map[Id, Set[Id]] =
  block match
    case BlockVar(id, annotatedTpe, annotatedCapt) =>
      ???

    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      ???

    case Member(block, field, annotatedTpe) =>
      ???

    case Unbox(pure) =>
      ???

    case New(impl) =>
      ???

def constructCallGraph(pure: Pure): Map[Id, Set[Id]] =
  pure match
    case ValueVar(id, annotatedType) =>
      ???

    case Literal(value, annotatedType) =>
      ???

    case PureApp(b, targs, vargs) =>
      ???

    case Select(target, field, annotatedType) =>
      ???

    case Box(b, annotatedCapture) =>
      ???

def constructCallGraph(impl: Implementation): Map[Id, Set[Id]] =
  impl match
    case Implementation(interface, operations) =>
      ???

def constructCallGraph(op: Operation): Map[Id, Set[Id]] =
  op match
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      ???

*/