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
    val uniqueArguments: Map[Id, List[Expr]] = Map.empty,
    // For unique-external-call functions: store the worker definition
    // to be emitted at the call site
    val pendingWorkers: mutable.Map[Id, Worker] = mutable.Map.empty
  ) {
    def within(id: Id): Boolean = stack.contains(id)

    def hasStatics(id: Id): Boolean = statics.get(id).exists(_.exists(x => x))

    def hasUniqueCall(id: Id): Boolean = uniqueArguments.contains(id)

    def shouldRelocate(id: Id): Boolean = hasStatics(id) && hasUniqueCall(id)
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
   * Wrap definition for the multiple-external-calls case (no relocation).
   */
  def wrapDefinition(id: Id, params: List[Id], body: Stmt)(using ctx: Context): (List[Id], Stmt) = {

    val Worker(workerId, staticParams, dynamicParams, workerBody) = buildWorker(id, params, body)

    // build wrapper
    val isStatic = ctx.statics(id)
    val wrapperParams = isStatic.zip(params).map {
      case (true, p) => p
      case (false, p) => Id(p)
    }

    val wrappedBody = Stmt.Def(workerId, dynamicParams, workerBody,
      Stmt.App(workerId, dropStatic(isStatic, wrapperParams.map(Expr.Variable(_)))))

    (wrapperParams, wrappedBody)
  }

  // --- Rewrite ---

  def rewrite(s: Stmt)(using ctx: Context): Stmt = s match {

    // Unique external call: remove the definition, build the worker for later insertion
    case Stmt.Def(id, params, body, rest) if ctx.shouldRelocate(id) =>
      ctx.pendingWorkers(id) = buildWorker(id, params, body)
      rewrite(rest)

    // Multiple external calls: wrap in place
    case Stmt.Def(id, params, body, rest) if ctx.hasStatics(id) =>
      val (freshParams, wrappedBody) = wrapDefinition(id, params, body)
      Stmt.Def(id, freshParams, wrappedBody, rewrite(rest))

    // Keep as is
    case Stmt.Def(id, params, body, rest) =>
      val before = ctx.stack
      ctx.stack = id :: ctx.stack
      val rewrittenBody = rewrite(body)
      ctx.stack = before
      Stmt.Def(id, params, rewrittenBody, rewrite(rest))

    // Recursive call: redirect to worker, drop static args
    case Stmt.App(id, args) if ctx.hasStatics(id) && ctx.within(id) =>
      val isStatic = ctx.statics(id)
      Stmt.App(ctx.workers(id), dropStatic(isStatic, args.map(rewrite)))

    // External call to a relocated function: emit the pending worker here, then call it
    case Stmt.App(id, args) if ctx.pendingWorkers.contains(id) =>
      val isStatic = ctx.statics(id)
      val Worker(workerId, staticParams, dynamicParams, workerBody) = ctx.pendingWorkers(id)
      val rewrittenArgs = args.map(rewrite)
      val dynamicArgs = dropStatic(isStatic, rewrittenArgs)
      val staticArgs = keepStatic(isStatic, rewrittenArgs)

      val loop = Stmt.Def(workerId, dynamicParams, workerBody,
        Stmt.App(workerId, dynamicArgs))

      // let y = 4;
      // def loop(x) = ...
      // loop(10)
      staticParams.zip(staticArgs).foldRight(loop) {
        case ((param, arg), call) => Let(param, arg, call)
      }

    case Stmt.App(id, args) =>
      Stmt.App(id, args.map(rewrite))

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(rewrite))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args.map(rewrite), purity, rewrite(rest))

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(rewrite), rewrite(rest))

    case Stmt.Val(id, binding, rest) =>
      Stmt.Val(id, rewrite(binding), rewrite(rest))

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
    // Relocated to call site — remove from toplevel
    case ToplevelDefinition.Def(id, params, body) if ctx.shouldRelocate(id) =>
      ctx.pendingWorkers(id) = buildWorker(id, params, body)
      None

    case ToplevelDefinition.Def(id, params, body) if ctx.hasStatics(id) =>
      val (freshParams, wrappedBody) = wrapDefinition(id, params, body)
      Some(ToplevelDefinition.Def(id, freshParams, wrappedBody))

    case ToplevelDefinition.Def(id, params, body) =>
      val before = ctx.stack
      ctx.stack = id :: ctx.stack
      val rewrittenBody = rewrite(body)
      ctx.stack = before
      Some(ToplevelDefinition.Def(id, params, rewrittenBody))

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      Some(ToplevelDefinition.Val(id, ks, k, rewrite(binding)))

    case ToplevelDefinition.Let(id, binding) =>
      Some(ToplevelDefinition.Let(id, rewrite(binding)))
  }

  // --- Entry point ---

  def transform(m: ModuleDecl): ModuleDecl = {
    val analysis = UsageAnalysis(m)
    given ctx: Context = initializeContext(analysis)

    m.copy(definitions = m.definitions.flatMap(d => rewrite(d)))
  }


  private def initializeContext(analysis: UsageAnalysis): Context = {
    val statics = mutable.Map.empty[Id, List[Boolean]]
    val extArgs = mutable.Map.empty[Id, List[Expr]]

    analysis.functions.foreach {
      case (id, info) if info.isRecursive =>
        val isInternallyStatic = info.staticArguments

        if info.externalCalls.size == 1 then
          statics(id) = isInternallyStatic
          extArgs(id) = info.externalCalls.head
        else if info.externalCalls.size > 1 then
          val firstExt = info.externalCalls.head
          val isStatic = isInternallyStatic.zipWithIndex.map { case (intStatic, idx) =>
            intStatic && info.externalCalls.tail.forall { args =>
              args.length > idx && firstExt(idx) == args(idx)
            }
          }
          statics(id) = isStatic
        else
          statics(id) = isInternallyStatic

      case _ => ()
    }
    new Context(statics.toMap, uniqueArguments = extArgs.toMap)
  }
}
