package effekt
package core

import scala.collection.{GenMap, mutable}

/*
Functions called by functions.

collectAliases: -

collectFunctionDefinitions: -

countFunctionOccurences: countFunctionOccurencesWorker

countFunctionOccurencesWorker: -

findRecursiveFunctions: countFunctionOccurences

findStaticArguments: findStaticArgumentsWorker

size: -
*/

// used by findStaticArguments
class StaticParamsUnfinished(fname: Id, //name of examined function
                             tparams: Array[Option[Id]], //Arrays of params
                             cparams: Array[Option[Id]], //None if param is not static
                             vparams: Array[Option[Id]], //Some(id) else
                             bparams: Array[Option[Id]]):
  def unpackParams: (Array[Option[Id]], Array[Option[Id]], Array[Option[Id]], Array[Option[Id]]) =
    (tparams, cparams, vparams, bparams)

  def id: Id = fname

// used by findStaticArguments
class StaticParams(fname: Id, //name of examined function
                   tparams: Set[Id], //Sets of static args
                   cparams: Set[Id], //obtained by filtering None from unfinished arrays
                   vparams: Set[Id],
                   bparams: Set[Id],
                   tIndices: List[Int], //List of indices of static args
                   cIndices: List[Int], //obtained by finding indices where unfinished arrays != None
                   vIndices: List[Int],
                   bIndices: List[Int]):
  def id: Id = fname
  
  def unpackParams: (Set[Id], Set[Id], Set[Id], Set[Id]) =
    (tparams, cparams, vparams, bparams)

  def unpackIndices: (List[Int], List[Int], List[Int], List[Int]) =
    (tIndices, cIndices, vIndices, bIndices)

  // Check if result of findStaticArguments is non empty. If #t apply SAT to function
  def nonEmpty: Boolean =
    tparams.nonEmpty || cparams.nonEmpty || vparams.nonEmpty || bparams.nonEmpty

// Finds all functions, defined as BlockVar of another
// Returns Map of alias along with original name
// Def(alias@id, og@BlockVar) => Map(alias -> og)
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
      p.map(collectAliases).fold(Map[Id, Id]())(_ ++ _)

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

// Collects all function definitions in Map (only Def)
// Def(id, body) => Map(alias -> body)
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
      p.map(collectFunctionDefinitions).fold(Map[Id, Block]())(_ ++ _)

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

// Counts occurences of every function (not just calls)
// Definition only => 0 occurences
// Wrapper passes mutable map to worker and returns immutable result
def countFunctionOccurences(start: Tree|Definition): Map[Id, Int] =
  val res = mutable.Map[Id, Int]()

  start match
    case m: ModuleDecl =>
      countFunctionOccurencesWorker(m)(using res) //TODO: There must be a nice way to do this

    case b: Block =>
      countFunctionOccurencesWorker(b)(using res)

    case p: Pure =>
      countFunctionOccurencesWorker(p)(using res)

    case e: Expr =>
      countFunctionOccurencesWorker(e)(using res)

    case s: Stmt =>
      countFunctionOccurencesWorker(s)(using res)

    case i: Implementation =>
      countFunctionOccurencesWorker(i)(using res)

    case d: Definition =>
      countFunctionOccurencesWorker(d)(using res)

    case _ =>
  res.toMap[Id, Int]

def countFunctionOccurencesWorker(module: ModuleDecl)(using count: mutable.Map[Id, Int]): Unit =
  module.definitions.foreach(countFunctionOccurencesWorker)

def countFunctionOccurencesWorker(definition: Definition)(using count: mutable.Map[Id, Int]): Unit =
  definition match
    case Definition.Def(id, block) =>
      if(!count.contains(id)) count += (id -> 0)
      countFunctionOccurencesWorker(block)

    case Definition.Let(_, binding) =>
      countFunctionOccurencesWorker(binding)

def countFunctionOccurencesWorker(expr: Expr)(using count: mutable.Map[Id, Int]): Unit =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      countFunctionOccurencesWorker(b)
      vargs.foreach(countFunctionOccurencesWorker)
      bargs.foreach(countFunctionOccurencesWorker)

    case Run(s) =>
      countFunctionOccurencesWorker(s)

    case p: Pure =>
      countFunctionOccurencesWorker(p)

def countFunctionOccurencesWorker(statement: Stmt)(using count: mutable.Map[Id, Int]): Unit =
  statement match
    case Scope(definitions, body) =>
      definitions.foreach(countFunctionOccurencesWorker)
      countFunctionOccurencesWorker(body)

    case Return(p) =>
      p.foreach(countFunctionOccurencesWorker)

    case Val(_, binding, body) =>
      countFunctionOccurencesWorker(binding)
      countFunctionOccurencesWorker(body)

    case App(b, _, vargs, bargs) =>
      countFunctionOccurencesWorker(b)
      vargs.foreach(countFunctionOccurencesWorker)
      bargs.foreach(countFunctionOccurencesWorker)

    case If(cond, thn, els) =>
      countFunctionOccurencesWorker(cond)
      countFunctionOccurencesWorker(thn)
      countFunctionOccurencesWorker(els)

    case Match(scrutinee, clauses, default) =>
      countFunctionOccurencesWorker(scrutinee)
      clauses.foreach{case (_, b) => countFunctionOccurencesWorker(b)}
      default match
        case Some(s) => countFunctionOccurencesWorker(s)
        case _ =>

    case State(_, init, _, body) =>
      countFunctionOccurencesWorker(init)
      countFunctionOccurencesWorker(body)

    case Try(body, handlers) =>
      countFunctionOccurencesWorker(body)
      handlers.foreach(countFunctionOccurencesWorker)

    case Region(body) =>
      countFunctionOccurencesWorker(body)

    case _: Hole =>

def countFunctionOccurencesWorker(block: Block)(using count: mutable.Map[Id, Int]): Unit =
  block match
    case BlockVar(id, _, _) =>
      if(count.contains(id)) count(id) += 1
      else count += (id -> 1)

    case BlockLit(_, _, _, _, body) =>
      countFunctionOccurencesWorker(body)

    case Member(b, _, _) =>
      countFunctionOccurencesWorker(b)

    case Unbox(p) =>
      countFunctionOccurencesWorker(p)

    case New(impl) =>
      countFunctionOccurencesWorker(impl)

def countFunctionOccurencesWorker(pure: Pure)(using count: mutable.Map[Id, Int]): Unit =
  pure match
    case _: ValueVar =>
    case _: Literal =>

    case PureApp(b, _, vargs) =>
      countFunctionOccurencesWorker(b)
      vargs.foreach(countFunctionOccurencesWorker)

    case Select(target, _, _) =>
      countFunctionOccurencesWorker(target)

    case Box(b, _) =>
      countFunctionOccurencesWorker(b)

def countFunctionOccurencesWorker(impl: Implementation)(using count: mutable.Map[Id, Int]): Unit =
  impl match
    case Implementation(_, operations) =>
      operations.foreach(countFunctionOccurencesWorker)

def countFunctionOccurencesWorker(op: Operation)(using ocunt: mutable.Map[Id, Int]): Unit =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      countFunctionOccurencesWorker(body)

// Returns Set of Ids of directly recursive functions
// Def(id, body) recursive if count(body)[id] > 0
def findRecursiveFunctions(module: ModuleDecl): Set[Id] =
  module match
    case ModuleDecl(_, _, _, _, definitions, _) =>
      definitions.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _)

def findRecursiveFunctions(definition: Definition): Set[Id] =
  definition match
    case Definition.Def(id, block) =>
      findRecursiveFunctions(block) ++
        (if(countFunctionOccurences(block).contains(id)) Set(id)
        else Set[Id]())

    case Definition.Let(_, binding) =>
      findRecursiveFunctions(binding)

def findRecursiveFunctions(expr: Expr): Set[Id] =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      findRecursiveFunctions(b) ++
        vargs.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _) ++
        bargs.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _)

    case Run(s) =>
      findRecursiveFunctions(s)

    case p: Pure =>
      findRecursiveFunctions(p)

def findRecursiveFunctions(statement: Stmt): Set[Id] =
  statement match
    case Scope(definitions, body) =>
      definitions.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _) ++ findRecursiveFunctions(body)

    case Return(exprs) =>
      exprs.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _)

    case Val(_, binding, body) =>
      findRecursiveFunctions(binding) ++ findRecursiveFunctions(body)

    case App(callee, _, vargs, bargs) =>
      findRecursiveFunctions(callee) ++
        vargs.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _) ++
        bargs.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _)

    case If(cond, thn, els) =>
      findRecursiveFunctions(cond) ++ findRecursiveFunctions(thn) ++ findRecursiveFunctions(els)

    case Match(scrutinee, clauses, default) =>
      findRecursiveFunctions(scrutinee) ++
        clauses.map{case (_, b) => findRecursiveFunctions(b)}.fold(Set[Id]())(_ ++ _) ++
        (default match
          case Some(s) => findRecursiveFunctions(s)
          case None => Set[Id]())

    case State(_, init, _, body) =>
      findRecursiveFunctions(init) ++ findRecursiveFunctions(body)

    case Try(body, handlers) =>
      findRecursiveFunctions(body) ++
        handlers.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _)

    case Region(body) =>
      findRecursiveFunctions(body)

    case _: Hole =>
      Set[Id]()

def findRecursiveFunctions(block: Block): Set[Id] =
  block match
    case _: BlockVar =>
      Set[Id]()

    case BlockLit(_, _, _, _, body) =>
      findRecursiveFunctions(body)

    case Member(block, _, _) =>
      findRecursiveFunctions(block)

    case Unbox(p) =>
      findRecursiveFunctions(p)

    case New(impl) =>
      findRecursiveFunctions(impl)

def findRecursiveFunctions(pure: Pure): Set[Id] =
  pure match
    case _: ValueVar =>
      Set[Id]()

    case _: Literal =>
      Set[Id]()

    case PureApp(b, _, vargs) =>
      findRecursiveFunctions(b) ++
        vargs.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _)

    case Select(target, _, _) =>
      findRecursiveFunctions(target)

    case Box(b, _) =>
      findRecursiveFunctions(b)

def findRecursiveFunctions(impl: Implementation): Set[Id] =
  impl match
    case Implementation(_, operations) =>
      operations.map(findRecursiveFunctions).fold(Set[Id]())(_ ++ _)

def findRecursiveFunctions(op: Operation): Set[Id] =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      findRecursiveFunctions(body)

// Finds all static arguments in recursive calls of input definition

// Wrapper that initializes worker and filters result
def findStaticArguments(start: Definition.Def): StaticParams =
  start match
    case Definition.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) => //TODO: Refactor
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

    case Return(exprs) =>
      exprs.foreach(findStaticArgumentsWorker)

    case Val(_, binding, body) =>
      findStaticArgumentsWorker(binding)
      findStaticArgumentsWorker(body)

    case App(BlockVar(id, _, _), targs, vargs, bargs) =>
      val (tparams, cparams, vparams, bparams) = params.unpackParams
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

// Computes size of Tree. Counts every node as 1
def size(module: ModuleDecl): Int =
  module match
    case ModuleDecl(_, _, _, _, definitions, _) =>
      1 + definitions.map(size).sum

def size(definition: Definition): Int =
  definition match
    case Definition.Def(_, block) =>
      1 + size(block)

    case Definition.Let(_, binding) =>
      1 + size(binding)

def size(expr: Expr): Int =
  expr match
    case DirectApp(b, _, vargs, bargs) =>
      1 + size(b) + vargs.map(size).sum + bargs.map(size).sum

    case Run(s) =>
      1 + size(s)

    case p: Pure =>
      size(p)

def size(statement: Stmt): Int =
  statement match
    case Scope(definitions, body) =>
      1 + definitions.map(size).sum + size(body)

    case Return(exprs) =>
      1 + exprs.map(size).sum

    case Val(_, binding, body) =>
      1 + size(binding) + size(body)

    case App(callee, _, vargs, bargs) =>
      1 + size(callee) + vargs.map(size).sum + bargs.map(size).sum

    case If(cond, thn, els) =>
      1 + size(cond) + size(thn) + size(els)

    case Match(scrutinee, clauses, default) =>
      1 + size(scrutinee) + clauses.map{case (_, b) => size(b)}.sum +
        {default match
          case Some(s) => size(s)
          case None => 0}

    case State(_, init, _, body) =>
      1 + size(init) + size(body)

    case Try(body, handlers) =>
      1 + size(body) + handlers.map(size).sum

    case Region(body) =>
      1 + size(body)

    case _: Hole =>
      1

def size(block: Block): Int =
  block match
    case _: BlockVar =>
      1

    case BlockLit(_, _, _, _, body) =>
      1 + size(body)

    case Member(block, _, _) =>
      1 + size(block)

    case Unbox(pure) =>
      1 + size(pure)

    case New(impl) =>
      1 + size(impl)

def size(pure: Pure): Int =
  pure match
    case _: ValueVar =>
      1

    case _: Literal =>
      1

    case PureApp(b, _, vargs) =>
      1 + size(b) + vargs.map(size).sum

    case Select(target, _, _) =>
      1 + size(target)

    case Box(b, _) =>
      1 + size(b)

def size(impl: Implementation): Int =
  impl match
    case Implementation(_, operations) =>
      1 + operations.map(size).sum

def size(op: Operation): Int =
  op match
    case Operation(_, _, _, _, _, _, body) =>
      1 + size(body)
