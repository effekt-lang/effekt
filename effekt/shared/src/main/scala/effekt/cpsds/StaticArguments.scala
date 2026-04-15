package effekt
package cpsds

import core.Id
import scala.collection.mutable
import cpsds.substitutions.{ Substitution, substitute }

object StaticArguments {

  class Context(
    val statics: Map[Id, List[Boolean]],
    val workers: mutable.Map[Id, Id] = mutable.Map.empty,
    var stack: List[Id] = Nil,
    val pendingWorkers: mutable.Map[Id, Worker] = mutable.Map.empty
  ) {
    def within(id: Id): Boolean = stack.contains(id)

    def hasStatics(id: Id): Boolean = statics.get(id).exists(_.exists(x => x))
  }

  case class Worker(id: Id, staticParams: List[Id], dynamicParams: List[Id], body: Stmt)

  private def dropStatic[A](isStatic: List[Boolean], args: List[A]): List[A] =
    isStatic.zip(args).collect { case (false, a) => a }

  private def keepStatic[A](isStatic: List[Boolean], args: List[A]): List[A] =
    isStatic.zip(args).collect { case (true, a) => a }

  /**
   * Build the specialized worker for a function.
   */
  def buildWorker(id: Id, params: List[Id], body: Stmt)(using ctx: Context): Worker = {
    val isStatic = ctx.statics(id)

    val workerId = Id(id.name.rename(original => s"${original}_worker"))
    ctx.workers(id) = workerId

    val staticParams = keepStatic(isStatic, params)
    val dynamicParams = dropStatic(isStatic, params)

    val before = ctx.stack
    ctx.stack = id :: ctx.stack
    val rewrittenBody = rewrite(body)
    ctx.stack = before

    Worker(workerId, staticParams, dynamicParams, rewrittenBody)
  }

  /**
   * Place a pending worker definition here, binding static args from the call site.
   * Returns a function that wraps a continuation statement with the worker definition.
   */
  private def placeWorkerHere(id: Id, args: List[Expr])(using ctx: Context): Stmt => Stmt = {
    val isStatic = ctx.statics(id)
    val Worker(workerId, staticParams, dynamicParams, workerBody) = ctx.pendingWorkers.remove(id).get
    val rewrittenArgs = args.map(rewrite)
    val staticArgs = keepStatic(isStatic, rewrittenArgs)

    rest => {
      val withDef = Stmt.Def(workerId, dynamicParams, workerBody, rest)
      staticParams.zip(staticArgs).foldRight(withDef: Stmt) {
        case ((param, Expr.Variable(argId)), r) =>
          val subst = Substitution(Map(param -> Expr.Variable(argId)))
          substitute(r)(using subst)
        case ((param, arg), r) =>
          Stmt.Let(param, arg, r)
      }
    }
  }

  /**
   * Rewrite a call to a function with static args: redirect to worker, drop static args.
   */
  private def rewriteCall(id: Id, args: List[Expr])(using ctx: Context): Stmt = {
    val isStatic = ctx.statics(id)
    Stmt.App(ctx.workers(id), dropStatic(isStatic, args.map(rewrite)), false)
  }

  /**
   * Find pending workers referenced in the free variables of a statement.
   */
  private def referencedWorkers(free: Set[Id])(using ctx: Context): Set[Id] =
    ctx.pendingWorkers.keySet.filter { id =>
      free.contains(id) || ctx.workers.get(id).exists(free.contains)
    }.toSet

  /**
   * Extract the immediate sub-statements of a statement.
   */
  private def children(s: Stmt): List[Stmt] = s match {
    case Stmt.Def(_, _, body, rest) => List(body, rest)
    case Stmt.New(_, _, ops, rest) => ops.map(_.body) :+ rest
    case Stmt.Let(_, _, rest) => List(rest)
    case Stmt.Run(_, _, _, _, rest) => List(rest)
    case Stmt.If(_, thn, els) => List(thn, els)
    case Stmt.Match(_, clauses, default) => clauses.map(_._2.body) ++ default.toList
    case Stmt.Region(_, _, rest) => List(rest)
    case Stmt.Alloc(_, _, _, rest) => List(rest)
    case Stmt.Var(_, _, _, rest) => List(rest)
    case Stmt.Dealloc(_, rest) => List(rest)
    case Stmt.Get(_, _, rest) => List(rest)
    case Stmt.Put(_, _, rest) => List(rest)
    case Stmt.Reset(_, _, _, body, _, _) => List(body)
    case Stmt.Shift(_, _, _, _, body, _, _) => List(body)
    case Stmt.Resume(_, _, _, body, _, _) => List(body)
    case _ => Nil
  }

  /**
   * After rewriting a statement, check if any pending workers are referenced
   * in multiple sub-statements of the result. If so, wrap them above.
   */
  private def placeWorkers(s: Stmt)(rewrite: Stmt => Stmt)(using ctx: Context): Stmt = {
    val subStmts = children(s)

    if subStmts.size < 2 then return rewrite(s)

    val frees = subStmts.map(s => referencedWorkers(s.free))
    val shared = frees.combinations(2).flatMap {
      case List(a, b) => a intersect b
      case _ => Set.empty
    }.toSet

    if shared.isEmpty then return rewrite(s)

    val wrappers = shared.toList.flatMap { id =>
      ctx.pendingWorkers.remove(id).map(id -> _)
    }

    wrappers.foldRight(rewrite(s)) { case ((id, Worker(workerId, staticParams, dynamicParams, workerBody)), rest) =>
      val isStatic = ctx.statics(id)
      val si = staticParams.iterator
      val di = dynamicParams.iterator
      val allParams = isStatic.map { s => if s then si.next() else Id(di.next()) }
      val dynamicWrapperArgs = isStatic.zip(allParams).collect { case (false, p) => Expr.Variable(p) }

      val wrappedBody = Stmt.Def(workerId, dynamicParams, workerBody,
        Stmt.App(workerId, dynamicWrapperArgs, false))

      Stmt.Def(id, allParams, wrappedBody, rest)
    }
  }

  // --- Rewrite ---

  // TODO
  // what if there are two workers that both are moved?
  //
  // def loop1() = ...
  // def loop2() = ... loop1() ...
  //
  // if () {
  //   loop2()
  // } else {
  //   loop2()
  // }
  //
  // the order of insertion matters since we want to sink all loops
  // but not too far

  def rewrite(s: Stmt)(using ctx: Context): Stmt = placeWorkers(s) {

    case Stmt.Def(id, params, body, rest) if ctx.hasStatics(id) =>
      ctx.pendingWorkers(id) = buildWorker(id, params, body)
      rewrite(rest)

    case Stmt.Def(id, params, body, rest) =>
      val before = ctx.stack
      ctx.stack = id :: ctx.stack
      val rewrittenBody = rewrite(body)
      ctx.stack = before
      Stmt.Def(id, params, rewrittenBody, rewrite(rest))

    // Recursive call: redirect to worker, drop static args
    case Stmt.App(id, args, direct) if ctx.hasStatics(id) && ctx.within(id) =>
      rewriteCall(id, args)

    // External call: place pending worker here, then rewrite the call
    case Stmt.App(id, args, direct) if ctx.pendingWorkers.contains(id) =>
      placeWorkerHere(id, args) {
        rewriteCall(id, args)
      }

    // Call to an already-placed worker: just rewrite the call
    case Stmt.App(id, args, direct) if ctx.workers.contains(id) && !ctx.within(id) =>
      rewriteCall(id, args)

    case Stmt.App(id, args, direct) =>
      Stmt.App(id, args.map(rewrite), direct)

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(rewrite))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args.map(rewrite), purity, rewrite(rest))

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(rewrite), rewrite(rest))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, rewrite(binding), rewrite(rest))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(rewrite(cond), rewrite(thn), rewrite(els))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(rewrite(scrutinee),
        clauses.map { case (id, cl) => (id, rewrite(cl)) },
        default.map(rewrite))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, rewrite(ks), rewrite(rest))

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, rewrite(init), region, rewrite(rest))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(id, rewrite(init), rewrite(ks), rewrite(rest))

    case Stmt.Dealloc(ref, rest) =>
      Stmt.Dealloc(ref, rewrite(rest))

    case Stmt.Get(ref, id, rest) =>
      Stmt.Get(ref, id, rewrite(rest))

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(ref, rewrite(value), rewrite(rest))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Stmt.Reset(p, ks, k, rewrite(body), rewrite(ks1), rewrite(k1))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(prompt, resume, ks, k, rewrite(body), rewrite(ks1), rewrite(k1))

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Stmt.Resume(r, ks, k, rewrite(body), rewrite(ks1), rewrite(k1))

    case h: Stmt.Hole => h
  }

  def rewrite(e: Expr)(using ctx: Context): Expr = e match {
    case Expr.Variable(_) => e
    case Expr.Literal(_, _) => e
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(rewrite))
    case Expr.Abort => e
    case Expr.Return => e
    case Expr.Toplevel => e
  }

  def rewrite(op: Operation)(using ctx: Context): Operation =
    Operation(op.name, op.params, rewrite(op.body))

  def rewrite(cl: Clause)(using ctx: Context): Clause =
    Clause(cl.params, rewrite(cl.body))

  // --- Toplevel ---

  def rewrite(d: ToplevelDefinition)(using ctx: Context): Option[ToplevelDefinition] = d match {
    case ToplevelDefinition.Def(id, params, body) if ctx.hasStatics(id) =>
      ctx.pendingWorkers(id) = buildWorker(id, params, body)
      None

    case ToplevelDefinition.Def(id, params, body) =>
      val before = ctx.stack
      ctx.stack = id :: ctx.stack
      val rewrittenBody = rewrite(body)
      ctx.stack = before
      Some(ToplevelDefinition.Def(id, params, rewrittenBody))

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      Some(ToplevelDefinition.Val(id, ks, k, rewrite(binding)))
  }

  // --- Entry point ---

  def transform(m: ModuleDecl): ModuleDecl = {
    val analysis = UsageAnalysis(m)
    given ctx: Context = initializeContext(analysis)

    m.copy(definitions = m.definitions.flatMap(d => rewrite(d)))
  }

  private def initializeContext(analysis: UsageAnalysis): Context = {
    val statics = mutable.Map.empty[Id, List[Boolean]]

    analysis.functions.foreach {
      case (id, info) if info.isRecursive =>
        val isInternallyStatic = info.staticArguments

        if info.externalCalls.size <= 1 then
          statics(id) = isInternallyStatic
        else
          val firstExt = info.externalCalls.head
          val isStatic = isInternallyStatic.zipWithIndex.map { case (intStatic, idx) =>
            intStatic && info.externalCalls.tail.forall { args =>
              args.length > idx && firstExt(idx) == args(idx)
            }
          }
          statics(id) = isStatic

      case _ => ()
    }
    new Context(statics.toMap)
  }
}
