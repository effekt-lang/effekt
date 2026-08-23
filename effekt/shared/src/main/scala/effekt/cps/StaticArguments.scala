package effekt
package cps

import core.Id
import java.util.IdentityHashMap
import scala.collection.mutable
import cps.substitutions.substitute

object StaticArguments {

  private class FunctionInfo(
    val params: List[Id],
    val recursiveCalls: mutable.ListBuffer[List[Option[Expr]]] = mutable.ListBuffer.empty,
    val externalCalls: mutable.ListBuffer[List[Option[Expr]]] = mutable.ListBuffer.empty,
    var hasCompositionalCall: Boolean = false,
    var hasCpsReturn: Boolean = false
  ) {
    def isRecursive: Boolean = recursiveCalls.nonEmpty

    def staticArguments: List[Boolean] =
      params.zipWithIndex.map { case (param, index) =>
        recursiveCalls.nonEmpty && recursiveCalls.forall { args =>
          args(index) match {
            case Some(Expr.Variable(other)) => param == other
            case _ => false
          }
        }
      }

    /** A direct call removes the final CPS convention parameters. They must
     *  therefore remain on the specialized worker until calling-convention
     *  lowering has consumed them. */
    def admissibleStatics(statics: List[Boolean]): List[Boolean] =
      if !hasCompositionalCall then statics
      else statics.zipWithIndex.map { case (isStatic, index) =>
        isStatic && index < params.size - 2
      }
  }

  /** The call information needed only by static-argument specialization. */
  private class CallAnalysis(
    targetsByCall: IdentityHashMap[Stmt, Targets.CallTargets],
    val functions: mutable.Map[Id, FunctionInfo] = mutable.Map.empty,
    var stack: List[Id] = Nil
  ) {

    private def within[A](id: Id)(body: => A): A = {
      val before = stack
      stack = id :: stack
      val result = body
      stack = before
      result
    }

    private def register(id: Id, params: List[Id]): Unit =
      functions(id) = FunctionInfo(params)

    /** Static-argument specialization rewrites syntactically known calls.
     *  Indirect calls therefore constrain a target's calling convention, but
     *  are not themselves specialization sites. */
    private def recordKnown(
      callee: Id,
      args: List[Option[Expr]],
      compositional: Boolean
    ): Unit =
      functions.get(callee).filter(_.params.size == args.size).foreach { info =>
        info.hasCompositionalCall ||= compositional
        if stack.contains(callee) then info.recursiveCalls += args
        else info.externalCalls += args
      }

    private def markCompositionalTargets(call: Stmt.Call): Unit =
      Option(targetsByCall.get(call)).foreach { flow =>
        flow.targets.foreach { target =>
          functions.get(target).filter(_.params.size == call.args.size + 2).foreach {
            _.hasCompositionalCall = true
          }
        }
      }

    def process(stmt: Stmt): Unit = stmt match {
      case Stmt.Def(id, params, body, rest) =>
        register(id, params)
        within(id) { process(body) }
        process(rest)

      case Stmt.New(_, _, operations, rest) =>
        operations.foreach(operation => process(operation.body))
        process(rest)

      case Stmt.Let(_, _, rest) => process(rest)

      case call @ Stmt.Call(_, _, Callee.Function(id), args, ks, rest) =>
        if functions.contains(id) then
          recordKnown(
            id,
            args.map(Some(_)) ++ List(Some(ks), None),
            compositional = true)
        else markCompositionalTargets(call)
        process(rest)

      case Stmt.Call(_, _, Callee.Method(_, _), _, _, rest) =>
        process(rest)

      case Stmt.App(id, args) =>
        stack.headOption.flatMap(functions.get).foreach { owner =>
          val metaContinuation = owner.params.size - 2
          val returnsThroughConvention = args match {
            case List(_, Expr.Variable(ks)) if metaContinuation >= 0 =>
              id == owner.params.last && ks == owner.params(metaContinuation)
            case _ => false
          }
          owner.hasCpsReturn ||= returnsThroughConvention
        }
        recordKnown(id, args.map(Some(_)), compositional = false)

      case Stmt.Invoke(_, _, _) => ()
      case Stmt.Return(_) => ()
      case Stmt.Run(_, _, _, _, rest) => process(rest)
      case Stmt.If(_, thn, els) => process(thn); process(els)
      case Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => process(clause.body) }
        default.foreach(process)
      case Stmt.Region(_, _, rest) => process(rest)
      case Stmt.Alloc(_, _, _, rest) => process(rest)
      case Stmt.Var(_, _, _, rest) => process(rest)
      case Stmt.Dealloc(_, rest) => process(rest)
      case Stmt.Get(_, _, rest) => process(rest)
      case Stmt.Put(_, _, rest) => process(rest)
      case Stmt.Reset(_, _, _, body, _, _) => process(body)
      case Stmt.Shift(_, _, _, _, body, _, _) => process(body)
      case Stmt.Resume(_, _, _, body, _, _) => process(body)
      case Stmt.Hole(_) => ()
    }

    def process(module: ModuleDecl): Unit = {
      // Toplevel definitions are mutually visible, so register them first.
      module.definitions.foreach {
        case ToplevelDefinition.Def(id, params, _) => register(id, params)
        case _: ToplevelDefinition.Val => ()
      }
      module.definitions.foreach {
        case ToplevelDefinition.Def(id, _, body) => within(id) { process(body) }
        case ToplevelDefinition.Val(_, _, _, binding) => process(binding)
      }
    }
  }

  private object CallAnalysis {
    def apply(
      module: ModuleDecl,
      targetFlows: Vector[Targets.TargetResult]
    ): CallAnalysis = {
      val targetsByCall = new IdentityHashMap[Stmt, Targets.CallTargets]()
      targetFlows.foreach(_.callTargets.foreach { targets =>
        targetsByCall.put(targets.call, targets)
      })
      val analysis = new CallAnalysis(targetsByCall)
      analysis.process(module)
      analysis
    }
  }

  class Context(
    val statics: Map[Id, List[Boolean]],
    val wrapperSpecializations: Set[Id],
    val workers: mutable.Map[Id, Id] = mutable.Map.empty,
    var stack: List[Id] = Nil,
    val pendingWorkers: mutable.Map[Id, Worker] = mutable.Map.empty
  ) {
    def within(id: Id): Boolean = stack.contains(id)

    def hasStatics(id: Id): Boolean = statics.get(id).exists(_.exists(x => x))

    /** Discard workers introduced in a lexical subregion. Workers already
     *  visible on entry may still be placed there, so removals are retained.
     */
    def scoped[A](body: => A): A = {
      val visibleWorkers = workers.keySet.toSet
      val visiblePending = pendingWorkers.keySet.toSet
      try body
      finally {
        workers.keysIterator.filterNot(visibleWorkers).toList.foreach(workers.remove)
        pendingWorkers.keysIterator.filterNot(visiblePending).toList.foreach(pendingWorkers.remove)
      }
    }

    def withinBody[A](id: Id)(body: => A): A = scoped {
      val before = stack
      stack = id :: stack
      try body finally stack = before
    }
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
    val originalDynamicParams = dropStatic(isStatic, params)
    val dynamicParams = originalDynamicParams.map(Id.apply)

    // The worker is nested in a scope that can still bind the original
    // parameters. Its binders therefore have to be fresh, as all binders in
    // this IR are globally unique.
    val renaming = originalDynamicParams.zip(dynamicParams).map {
      case (from, to) => from -> Expr.Variable(to)
    }.toMap
    val rewrittenBody = substitute(
      ctx.withinBody(id) { rewrite(body) },
      renaming)

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
          substitute(r, Map(param -> Expr.Variable(argId)))
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
    Stmt.App(ctx.workers(id), dropStatic(isStatic, args.map(rewrite)))
  }

  private def rewriteCall(
    result: List[Id],
    returnedKs: Id,
    id: Id,
    args: List[Expr],
    ks: Expr,
    rest: Stmt
  )(using ctx: Context): Stmt = {
    val isStatic = ctx.statics(id)
    Stmt.Call(
      result,
      returnedKs,
      Callee.Function(ctx.workers(id)),
      dropStatic(isStatic, args.map(rewrite)),
      rewrite(ks),
      rewrite(rest))
  }

  /** Enter a specialized worker from its original calling convention.
   *
   * If the invariant is the CPS meta-continuation, close it into an entry
   * continuation. Recursive continuations can then uniformly omit `ks`, while
   * the entry continuation restores the original `(value, ks)` convention at
   * the boundary. The ordinary parameter-dropping pass removes its unused
   * `returnedKs` parameter afterwards.
   */
  private def enterWorker(
    id: Id,
    params: List[Id],
    worker: Worker
  )(using ctx: Context): Stmt = {
    val isStatic = ctx.statics(id)
    val dynamicArgs = dropStatic(isStatic, params).map(Expr.Variable.apply)
    val workerCall = Stmt.App(worker.id, dynamicArgs)

    if params.size >= 2 && isStatic(params.size - 2) && !isStatic.last then {
      val entry = Id("k")
      val result = Id("result")
      val returnedKs = Id("ks")
      val originalKs = params(params.size - 2)
      val originalK = params.last
      val adaptedArgs = dynamicArgs.dropRight(1) :+ Expr.Variable(entry)
      Stmt.Def(
        entry,
        List(result, returnedKs),
        Stmt.App(originalK, List(Expr.Variable(result), Expr.Variable(originalKs))),
        Stmt.App(worker.id, adaptedArgs))
    } else workerCall
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
    case Stmt.Call(_, _, _, _, _, rest) => List(rest)
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

    val wrappers = shared.toList.sortBy(_.id).flatMap { id =>
      ctx.pendingWorkers.remove(id).map(id -> _)
    }

    // The worker is local to the wrapper below. Calls in the surrounding
    // statement must therefore retain the wrapper's original convention.
    wrappers.foreach { case (id, _) => ctx.workers.remove(id) }
    val rewritten = rewrite(s)

    wrappers.foldRight(rewritten) { case ((id, Worker(workerId, staticParams, dynamicParams, workerBody)), rest) =>
      val isStatic = ctx.statics(id)
      val si = staticParams.iterator
      val di = dynamicParams.iterator
      val allParams = isStatic.map { s => if s then si.next() else Id(di.next()) }
      val dynamicWrapperArgs = isStatic.zip(allParams).collect { case (false, p) => Expr.Variable(p) }

      val wrappedBody = Stmt.Def(workerId, dynamicParams, workerBody,
        Stmt.App(workerId, dynamicWrapperArgs))

      Stmt.Def(id, allParams, wrappedBody, rest)
    }
  }

  // --- Rewrite ---

  def rewrite(s: Stmt)(using ctx: Context): Stmt = placeWorkers(s) {

    // When a recursive invariant differs between entry sites, retain the
    // original function as an entry wrapper. Its parameters bind the static
    // values for a fresh worker invocation; recursive calls bypass the
    // wrapper and use only the dynamic parameters.
    case Stmt.Def(id, params, body, rest) if ctx.wrapperSpecializations.contains(id) =>
      val worker = buildWorker(id, params, body)
      ctx.workers -= id
      val wrapperBody = Stmt.Def(
        worker.id,
        worker.dynamicParams,
        worker.body,
        enterWorker(id, params, worker))
      Stmt.Def(id, params, wrapperBody, rewrite(rest))

    case Stmt.Def(id, params, body, rest) if ctx.hasStatics(id) =>
      ctx.pendingWorkers(id) = buildWorker(id, params, body)
      rewrite(rest)

    case Stmt.Def(id, params, body, rest) =>
      val rewrittenBody = ctx.withinBody(id) { rewrite(body) }
      Stmt.Def(id, params, rewrittenBody, rewrite(rest))

    case Stmt.Call(result, returnedKs, Callee.Function(id), args, ks, rest) if ctx.hasStatics(id) && ctx.within(id) =>
      rewriteCall(result, returnedKs, id, args, ks, rest)

    case Stmt.Call(result, returnedKs, Callee.Function(id), args, ks, rest) if ctx.pendingWorkers.contains(id) =>
      placeWorkerHere(id, args) {
        rewriteCall(result, returnedKs, id, args, ks, rest)
      }

    case Stmt.Call(result, returnedKs, Callee.Function(id), args, ks, rest) if ctx.workers.contains(id) && !ctx.within(id) =>
      rewriteCall(result, returnedKs, id, args, ks, rest)

    case Stmt.Call(result, returnedKs, id, args, ks, rest) =>
      Stmt.Call(result, returnedKs, id, args.map(rewrite), rewrite(ks), rewrite(rest))

    // Recursive call: redirect to worker, drop static args
    case Stmt.App(id, args) if ctx.hasStatics(id) && ctx.within(id) =>
      rewriteCall(id, args)

    // External call: place pending worker here, then rewrite the call
    case Stmt.App(id, args) if ctx.pendingWorkers.contains(id) =>
      placeWorkerHere(id, args) {
        rewriteCall(id, args)
      }

    // Call to an already-placed worker: just rewrite the call
    case Stmt.App(id, args) if ctx.workers.contains(id) && !ctx.within(id) =>
      rewriteCall(id, args)

    case Stmt.App(id, args) =>
      Stmt.App(id, args.map(rewrite))

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(id, method, args.map(rewrite))

    case Stmt.Return(values) =>
      Stmt.Return(values.map(rewrite))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args.map(rewrite), purity, rewrite(rest))

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(rewrite), rewrite(rest))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, rewrite(binding), rewrite(rest))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(rewrite(cond), ctx.scoped { rewrite(thn) }, ctx.scoped { rewrite(els) })

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
      Stmt.Reset(p, ks, k, ctx.scoped { rewrite(body) }, rewrite(ks1), rewrite(k1))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(prompt, resume, ks, k, ctx.scoped { rewrite(body) }, rewrite(ks1), rewrite(k1))

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Stmt.Resume(r, ks, k, ctx.scoped { rewrite(body) }, rewrite(ks1), rewrite(k1))

    case h: Stmt.Hole => h
  }

  def rewrite(e: Expr)(using ctx: Context): Expr = e match {
    case Expr.Variable(_) => e
    case Expr.Literal(_, _) => e
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(rewrite))
    case Expr.Abort => e
    case Expr.Toplevel => e
  }

  def rewrite(op: Operation)(using ctx: Context): Operation =
    Operation(op.name, op.params, ctx.scoped { rewrite(op.body) })

  def rewrite(cl: Clause)(using ctx: Context): Clause =
    Clause(cl.params, ctx.scoped { rewrite(cl.body) })

  // --- Toplevel ---

  def rewrite(d: ToplevelDefinition)(using ctx: Context): Option[ToplevelDefinition] = d match {
    case ToplevelDefinition.Def(id, params, body) if ctx.hasStatics(id) =>
      val worker = buildWorker(id, params, body)

      // A toplevel caller may precede this definition in the module. Keeping
      // the original calling convention as a wrapper makes specialization
      // independent of module order. The inliner can still eliminate a
      // uniquely called wrapper afterwards.
      ctx.workers -= id
      val wrapperBody = Stmt.Def(
        worker.id,
        worker.dynamicParams,
        worker.body,
        enterWorker(id, params, worker))
      Some(ToplevelDefinition.Def(id, params, wrapperBody))

    case ToplevelDefinition.Def(id, params, body) =>
      val rewrittenBody = ctx.withinBody(id) { rewrite(body) }
      Some(ToplevelDefinition.Def(id, params, rewrittenBody))

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      Some(ToplevelDefinition.Val(id, ks, k, ctx.scoped { rewrite(binding) }))
  }

  // --- Entry point ---

  def transform(m: ModuleDecl): ModuleDecl =
    transform(m, Set.empty)

  /** Specialize recursive invariants without changing functions whose ABI has
   * already been fixed by a later calling-convention decision. */
  def transform(m: ModuleDecl, protectedDefinitions: Set[Id]): ModuleDecl = {
    transform(m, protectedDefinitions, cpsMetaContinuationsOnly = false)
  }

  /** Specialize only a recursively invariant CPS meta-continuation. This is
   * the focused cleanup needed after compositional calls have been lowered;
   * it deliberately does not rerun general static-argument specialization. */
  def specializeCpsMetaContinuations(
    m: ModuleDecl,
    protectedDefinitions: Set[Id]
  ): ModuleDecl = {
    transform(m, protectedDefinitions, cpsMetaContinuationsOnly = true)
  }

  private def transform(
    m: ModuleDecl,
    protectedDefinitions: Set[Id],
    cpsMetaContinuationsOnly: Boolean
  ): ModuleDecl = {
    val targetFlows = m.definitions.map(Targets.targets).toVector
    val analysis = CallAnalysis(m, targetFlows)
    // Opaque control can hide the path back to a recursive call from the
    // focused machine. In that case its must facts are vacuous; retain the
    // syntactic equations collected above instead.
    val pathStatics = StaticParameters.results(m)
    given ctx: Context = initializeContext(
      analysis,
      pathStatics,
      protectedDefinitions,
      cpsMetaContinuationsOnly)

    m.copy(definitions = m.definitions.flatMap(d => rewrite(d)))
  }

  private def initializeContext(
    analysis: CallAnalysis,
    pathStatics: Map[Id, StaticParameters.Result],
    protectedDefinitions: Set[Id],
    cpsMetaContinuationsOnly: Boolean
  ): Context = {
    val statics = mutable.Map.empty[Id, List[Boolean]]
    val wrapperSpecializations = mutable.Set.empty[Id]

    analysis.functions.foreach {
      case (id, info) if info.isRecursive && !protectedDefinitions.contains(id) =>
        val syntacticStatics = info.staticArguments
        val preciseStatics = pathStatics.get(id) match {
          case Some(result) if result.recursive => result.parameters.toList
          // Without a reached recursive call, positive must facts are
          // vacuous. Negative facts are still conservative, so intersect
          // the focused result with the syntactic recursive-call equations.
          case Some(result) =>
            result.parameters.zip(syntacticStatics).map(_ && _).toList
          case None => syntacticStatics
        }
        val isInternallyStatic = info.admissibleStatics(preciseStatics)

        if cpsMetaContinuationsOnly then {
          val metaContinuation = info.params.size - 2
          val specialize =
            info.hasCpsReturn &&
              metaContinuation >= 0 &&
              isInternallyStatic(metaContinuation)

          if specialize then {
            val mask = info.params.indices.map(_ == metaContinuation).toList
            statics(id) = mask

            val sameAtEveryEntry = info.externalCalls.size <= 1 || {
              val first = info.externalCalls.head(metaContinuation)
              info.externalCalls.tail.forall { args =>
                args.length > metaContinuation && args(metaContinuation) == first
              }
            }
            if !sameAtEveryEntry then wrapperSpecializations += id
          }
        } else if info.externalCalls.size <= 1 then
          statics(id) = isInternallyStatic
        else
          val firstExt = info.externalCalls.head
          val isStatic = isInternallyStatic.zipWithIndex.map { case (intStatic, idx) =>
            intStatic && info.externalCalls.tail.forall { args =>
              args.length > idx && firstExt(idx) == args(idx)
            }
          }
          if isStatic.exists(identity) then statics(id) = isStatic

          // A path-static meta-continuation admits the CPS analogue of a
          // loop-entry wrapper: bind it once at entry and let recursion use a
          // worker specialized to that value. Restricting this construction
          // to the conventional (ks, k) pair keeps it distinct from general
          // polyvariant specialization.
          val metaContinuation = info.params.size - 2
          val specializeEntryMetaContinuation =
            info.hasCpsReturn &&
              metaContinuation >= 0 &&
              isInternallyStatic(metaContinuation) &&
              !isStatic(metaContinuation)
          if specializeEntryMetaContinuation then {
            statics(id) = isStatic.updated(metaContinuation, true)
            wrapperSpecializations += id
          }

      case _ => ()
    }
    new Context(statics.toMap, wrapperSpecializations.toSet)
  }
}

/**
 * The relational, *must*-equality projection used by static-argument
 * specialization — the static (recursion-invariant) parameters — expressed on
 * [[AbstractMachine]].
 *
 * Two AAM ideas make this the same machine as [[PointsTo]], only re-knobbed:
 *
 *   - The value lattice is *symbolic* (Herbrand-flavoured), and the machine's
 *     `join` is used as the domain's *meet* (anti-unification): the store starts
 *     unwritten (`Nothing`, ⊥) and narrows toward `Anything` (⊤), keeping only
 *     agreements. That is a must analysis run on the may machine via the dual
 *     lattice.
 *   - The *focus* is the observed function: the analysis is run once per
 *     function, seeding its parameters with their own symbols. After the fixed
 *     point a parameter is static iff its address still holds that symbol — i.e.
 *     every reachable recursive call passed it unchanged.
 *
 * Calls to *known* functions resolve precisely through their closure values in
 * the store, so direct recursion is observed exactly; any other value used as a
 * callee is treated as opaque, and matches on symbolic scrutinees explore every
 * branch. Constructor values are reified to symbolic terms, so an argument that
 * rebuilds the same term counts as unchanged.
 */
class StaticParameters(module: ModuleDecl, observed: Id, parameters: List[Id])
    extends AbstractMachine(module) {

  enum Sym {
    case Nothing                 // ⊥: unwritten — identity for the dual join
    case Symbol(id: Id)          // provably equal to a parameter/variable
    case Term(expr: Expr)        // a specific pure term
    case Closure(value: StaticParameters.this.Value.Closure)
    case Anything                // ⊤: no equality known
  }

  type Property = Sym
  type Context = Unit

  def bottom: Sym = Sym.Nothing
  def external: Sym = Sym.Anything

  // The machine's `join` is the domain's meet: keep agreements, generalise
  // disagreements to ⊤.
  def join(left: Sym, right: Sym): Sym = (left, right) match {
    case (Sym.Nothing, other) => other
    case (other, Sym.Nothing) => other
    case (Sym.Anything, _) | (_, Sym.Anything) => Sym.Anything
    case (a, b) if a == b => a
    case _ => Sym.Anything
  }

  def literal(value: Any, annotatedType: core.ValueType): Sym =
    Sym.Term(Expr.Literal(value, annotatedType))

  def closure(value: Value.Closure, context: Unit): Sym = Sym.Closure(value)

  def instance(value: Value.Object, context: Unit): Sym = Sym.Anything

  def constructor(value: Value.Constructor, context: Unit): Sym = {
    val elements = value.fields.map(field => reify(valueAt(field).property))
    if elements.forall(_.isDefined) then
      Sym.Term(Expr.Make(value.tpe, value.tag, elements.map(_.get)))
    else Sym.Anything
  }

  private def reify(value: Sym): Option[Expr] = value match {
    case Sym.Symbol(id) => Some(Expr.Variable(id))
    case Sym.Term(expr) => Some(expr)
    case _ => None
  }

  def initialContext: Unit = ()
  def tick(
    call: Option[Stmt],
    callee: Value.Closure,
    arguments: List[Values],
    caller: Unit
  ): Unit = ()

  override protected def registerNestedDefinitions: Boolean = true

  // The focus: seed the observed function's parameters with their own symbols.
  override protected def entryPoints: List[(Id, List[Values])] =
    List(observed -> parameters.map { parameter =>
      // The symbol records identity, not runtime shape. In particular, a
      // symbolic scrutinee can be any constructor, so every match branch must
      // remain reachable.
      Values(Set.empty, open = true, Sym.Symbol(parameter))
    })

  private var recursive = false

  override protected def observeApply(
    statement: Stmt,
    callee: Values,
    targets: Set[Id],
    arguments: List[Values],
    context: Unit
  ): Unit =
    recursive ||= targets.contains(observed)

  private lazy val computed: Unit = run()

  /** For each parameter, whether every reachable recursive call passed it
   *  unchanged (its address still holds its own symbol). This is vacuously
   *  true when there is no recursive call; clients that distinguish that case
   *  can inspect [[result]]. */
  def staticParameters: Vector[Boolean] = result.parameters

  private[cps] def result: StaticParameters.Result = {
    computed
    StaticParameters.Result(
      parameters.map { parameter =>
        parameterValue(parameter, ()).property == Sym.Symbol(parameter)
      }.toVector,
      recursive)
  }
}

object StaticParameters {

  private[cps] final case class Result(parameters: Vector[Boolean], recursive: Boolean)

  /** The static parameters of every function, top-level and nested: each is
   *  focused in turn, seeding its own parameters.
   *
   *  A focused run only follows the observed function's own (mutually) recursive
   *  calls, so it cannot see the function being handed to unknown code. An
   *  escaping function may be re-entered with arbitrary arguments, so none of its
   *  parameters is recursion-invariant — gate those to all-varying. */
  def analyze(module: ModuleDecl): Map[Id, Vector[Boolean]] = {
    results(module).iterator.map { case (id, result) =>
      id -> result.parameters
    }.toMap
  }

  private[cps] def results(module: ModuleDecl): Map[Id, Result] = {
    val escaped = new PointsTo(module).escapedFunctions
    definitions(module).map { case (id, params) =>
      val result =
        if escaped.contains(id) then Result(Vector.fill(params.size)(false), recursive = false)
        else new StaticParameters(module, id, params).result
      id -> result
    }.toMap
  }

  /** Every definition — top-level and nested — with its parameter list. */
  private def definitions(module: ModuleDecl): List[(Id, List[Id])] = {
    val out = mutable.ListBuffer.empty[(Id, List[Id])]
    def go(stmt: Stmt): Unit = stmt match {
      case Stmt.Def(id, params, body, rest) => out += ((id, params)); go(body); go(rest)
      case Stmt.New(_, _, operations, rest) => operations.foreach(o => go(o.body)); go(rest)
      case Stmt.Let(_, _, rest) => go(rest)
      case Stmt.Call(_, _, _, _, _, rest) => go(rest)
      case Stmt.Run(_, _, _, _, rest) => go(rest)
      case Stmt.If(_, thn, els) => go(thn); go(els)
      case Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => go(clause.body) }; default.foreach(go)
      case Stmt.Region(_, _, rest) => go(rest)
      case Stmt.Alloc(_, _, _, rest) => go(rest)
      case Stmt.Var(_, _, _, rest) => go(rest)
      case Stmt.Dealloc(_, rest) => go(rest)
      case Stmt.Get(_, _, rest) => go(rest)
      case Stmt.Put(_, _, rest) => go(rest)
      case Stmt.Reset(_, _, _, body, _, _) => go(body)
      case Stmt.Shift(_, _, _, _, body, _, _) => go(body)
      case Stmt.Resume(_, _, _, body, _, _) => go(body)
      case _: Stmt.App | _: Stmt.Invoke | _: Stmt.Return | _: Stmt.Hole => ()
    }
    module.definitions.foreach {
      case ToplevelDefinition.Def(id, params, body) => out += ((id, params)); go(body)
      case ToplevelDefinition.Val(_, _, _, binding) => go(binding)
    }
    out.toList
  }
}
