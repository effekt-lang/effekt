package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{Annotation, Annotations, Context, ContextOps}
import effekt.context.assertions.*
import effekt.source.{ AnyPattern, Def, Effectful, IgnorePattern, MatchPattern, MatchGuard, ModuleDecl, Stmt, TagPattern, Term, Tree, resolve, symbol }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.symbols.kinds.*
import effekt.util.messages.*
import effekt.util.foreachAborting

import scala.language.implicitConversions

/**
 * Typechecking
 * ============
 *
 * Preconditions:
 * --------------
 * Typer assumes that all dependencies already have been type checked.
 * In particular, it assumes that all definitions / symbols (functions, parameters etc.)
 * have been annotated with a type: this models a (global) typing context.
 *
 * Postconditions:
 * ---------------
 * All trees will be annotated with intermediate types (and effects). This is useful for
 * IDE support.
 * Also, after type checking, all definitions of the file will be annotated with their type.
 *
 * Invariants:
 * -----------
 * All effects inferred by Typer are concrete and dealiased. This is established by
 * type [[ConcreteEffects]] and constructor [[Typer.asConcrete]].
 */
case class Result[+T](tpe: T, effects: ConcreteEffects)


object Typer extends Phase[NameResolved, Typechecked] {

  val phaseName = "typer"

  def run(input: NameResolved)(using Context) = Context.using(module = input.mod, focus = input.tree) {
    try {
      val NameResolved(source, tree, mod) = input

      Context.initTyperstate()
      Context.timed(phaseName, source.name) {
        Context in {
          Context.withUnificationScope {
            ???
          }
        }
      }

      if (Context.messaging.hasErrors) {
        None
      } else {
        Some(Typechecked(source, tree, mod))
      }
    } finally {
      // Store the backtrackable annotations into the global DB
      // This is done regardless of errors, since
      Context.commitTypeAnnotations()
    }
  }

  def checkExpr(expr: Term, expected: Option[ValueType])(using Context, Captures): Result[ValueType] =
    ???

  def checkStmt(stmt: Stmt, expected: Option[ValueType])(using Context, Captures): Result[ValueType] =
    ???

  def findFunctionTypeFor(sym: BlockSymbol)(using Context): (FunctionType, Captures) = sym match {
    // capture of effect operations is dealt with by type checking Do or MethodCall
    case b: Operation => (Context.lookupFunctionType(b), CaptureSet.empty)
    case b: BlockSymbol => (Context.lookupFunctionType(b), Context.lookupCapture(b))
  }

  def tryEach[K, R](inputs: List[K])(f: K => R)(using Context): (List[(K, R, TyperState)], List[(K, EffektMessages)]) = {
    val stateBefore = Context.backupTyperstate()
    val results = inputs.map {
      case input =>
        try { input ->
          Try {
            val result = f(input)
            val state = Context.backupTyperstate()
            (result, state)
          }
        } finally { Context.restoreTyperstate(stateBefore) }
    }
    val successes = results.collect { case (sym, Right((r, st))) => (sym, r, st) }
    val errors = results.collect { case (sym, Left(r)) => (sym, r) }
    (successes, errors)
  }

  /**
   * Returns Left(Messages) if there are any errors
   *
   * In the case of nested calls, currently only the errors of the innermost failing call
   * are reported
   */
  private def Try[T](block: => T)(using C: Context): Either[EffektMessages, T] = {
    import kiama.util.Severities.Error

    val (msgs, optRes) = Context withMessages {
      try { Some(block) } catch {
        case FatalPhaseError(msg) =>
          C.report(msg)
          None
      }
    }

    if (msgs.exists { m => m.severity == Error } || optRes.isEmpty) {
      Left(msgs)
    } else {
      Right(optRes.get)
    }
  }

  //</editor-fold>

  //<editor-fold desc="Helpers to register subcapturing constraints">

  /**
   * The current capture / region / scope for which we collect constraints.
   *
   * Functions [[usingCapture]] and [[usingCaptureWithout]] implicitly look
   * up this scope and have their arguments flow into the current Capture.
   *
   * Function [[flowingInto]] sets the current capture.
   *
   * Example illustrating the interaction:
   *
   * {{{
   *   val inferredCapture: Captures = ???
   *   flowingInto(inferredCapture) {
   *     // inferredCapture is now the current capture
   *     assert(currentCapture == inferredCapture)
   *
   *     // other flows into inferredCapture (i.e. other <: inferredCapture)
   *     usingCapture(other)
   *   }
   * }}}
   */
  def currentCapture(using current: Captures): Captures = current

  /**
   * Sets the [[currentCapture]] in a given scope.
   */
  def flowingInto[T](c: Captures)(prog: Captures ?=> T): T = prog(using c)

  /**
   * Requires that [[c]] is included in the [[currentCapture]] (i.e. [[c]] <: [[currentCapture]])
   */
  def usingCapture(c: Captures)(using C: Context, current: Captures): Unit =
    C.requireSubregion(c, current)

  /**
   * Requires that [[c]] - [[filter]] is included in the [[currentCapture]] (i.e. ([[c]] - [[filter]]) <: [[currentCapture]])
   */
  def usingCaptureWithout(c: Captures)(filter: List[Capture])(using C: Context, current: Captures): Unit =
    flowsIntoWithout(c, current)(filter)

  /**
   * Requires that [[from]] is included in [[to]] (i.e. [[from]] <: [[to]])
   */
  def flowsInto(from: Captures, to: Captures)(using C: Context): Unit =
    C.requireSubregion(from, to)

  /**
   * Requires that [[from]] - [[filter]] is included in the [[to]] (i.e. ([[from]] - [[filter]]) <: [[to]])
   */
  def flowsIntoWithout(from: Captures, to: Captures)(filter: List[Capture])(using C: Context): Unit =
    C.requireSubregionWithout(from, to, filter)

  //</editor-fold>

  //<editor-fold desc="Other helpers and Extension Methods">
  def matchDeclared(got: BlockType, declared: BlockType, param: source.Param)(using Context): Unit =
    Context.at(param) {
      Context.requireSubtype(got, declared,
        ErrorContext.Declaration(param, Context.unification(declared), Context.unification(got)))
    }
  def matchDeclared(got: ValueType, declared: ValueType, param: source.Param)(using Context): Unit =
    Context.at(param) {
      Context.requireSubtype(got, declared,
        ErrorContext.Declaration(param, Context.unification(declared), Context.unification(got)))
    }

  def matchPattern(scrutinee: ValueType, patternTpe: ValueType, pattern: source.MatchPattern)(using Context): Unit =
    Context.requireSubtype(scrutinee, patternTpe, ErrorContext.PatternMatch(pattern))

  def matchExpected(got: ValueType, expected: ValueType)(using Context): Unit =
    Context.requireSubtype(got, expected,
      ErrorContext.Expected(Context.unification(got), Context.unification(expected), Context.focus))

  def matchExpected(got: BlockType, expected: BlockType)(using Context): Unit =
    Context.requireSubtype(got, expected,
      ErrorContext.Expected(Context.unification(got), Context.unification(expected), Context.focus))

  extension (expr: Term) {
    def checkAgainst(tpe: ValueType)(using Context, Captures): Result[ValueType] =
      checkExpr(expr, Some(tpe))
  }

  extension (stmt: Stmt) {
    def checkAgainst(tpe: ValueType)(using Context, Captures): Result[ValueType] =
      checkStmt(stmt, Some(tpe))
  }

  /**
   * Combinators that also store the computed type for a tree in the TypesDB
   */
  def checkAgainst[T <: Tree](t: T, expected: Option[ValueType])(f: T => Result[ValueType])(using Context, Captures): Result[ValueType] =
    Context.at(t) {
      val Result(got, effs) = f(t)
      wellformed(got)
      wellformed(effs.toEffects)
      expected foreach { matchExpected(got, _) }
      Context.annotateInferredType(t, got)
      Context.annotateInferredEffects(t, effs.toEffects)
      Result(got, effs)
    }

  def checkBlockAgainst[T <: Tree](t: T, expected: Option[BlockType])(f: T => Result[BlockType])(using Context, Captures): Result[BlockType] =
    Context.at(t) {
      val Result(got, effs) = f(t)
      wellformed(got)
      wellformed(effs.toEffects)
      expected foreach { matchExpected(got, _) }
      Context.annotateInferredType(t, got)
      Context.annotateInferredEffects(t, effs.toEffects)
      Result(got, effs)
    }
  //</editor-fold>

}

/**
 * Instances of this class represent an immutable backup of the typer state
 */
private[typer] case class TyperState(annotations: Annotations, unification: UnificationState, capabilityScope: CapabilityScope)

trait TyperOps extends ContextOps { self: Context =>


  /**
   * Local annotations database, only used by Typer
   *
   * It is used to (1) model the typing context, (2) collect information
   * used for elaboration (i.e., capabilities), and (3) gather inferred
   * types for LSP support.
   *
   * (1) "Typing Context"
   * --------------------
   * Since symbols are unique, we can use mutable state instead of reader.
   * Typer uses local annotations that are immutable and can be backtracked.
   *
   * The "Typing Context" consists of:
   * - typing context for value types [[Annotations.ValueType]]
   * - typing context for block types [[Annotations.BlockType]]
   * - modalities on typing context for block symbol [[Annotations.Captures]]
   *
   * (2) Elaboration Info
   * --------------------
   * - [[Annotations.CapabilityReceiver]]
   * - [[Annotations.CapabilityArguments]]
   * - [[Annotations.BoundCapabilities]]
   * - [[Annotations.TypeArguments]]
   *
   * (3) Inferred Information for LSP
   * --------------------------------
   * We first store the inferred types here, before substituting and committing to the
   * global DB, later.
   * - [[Annotations.InferredValueType]]
   * - [[Annotations.InferredBlockType]]
   * - [[Annotations.InferredEffect]]
   */
  private [typer] var annotations: Annotations = Annotations.empty

  //<editor-fold desc="(1) Unification">

  /**
   * The unification engine, keeping track of constraints and the current unification scope
   *
   * Contains mutable variables. The methods [[unification.backup()]] and [[unification.restore()]]
   * allow to save a copy of the current state.
   */
  private[typer] val unification = new Unification(using this)
  export unification.{ requireSubtype, requireSubregion, join, instantiate, instantiateFresh, freshTypeVar, freshCaptVar, without, requireSubregionWithout }

  // opens a fresh unification scope
  private[typer] def withUnificationScope[T](additional: List[CaptUnificationVar])(block: => T): T = {
    unification.enterScope()
    val res = block
    unification.leaveScope(additional)
    res
  }
  private[typer] def withUnificationScope[T](block: => T): T = withUnificationScope(Nil)(block)

  //</editor-fold>

  //<editor-fold desc="(2) Capability Passing">

  private [typer] var capabilityScope: CapabilityScope = GlobalCapabilityScope

  private [typer] def bindingCapabilities[R](binder: source.Tree, caps: List[symbols.BlockParam])(f: => R): R = {
    bindCapabilities(binder, caps)
    capabilityScope = BindSome(binder, caps.map { c => c.tpe.asInterfaceType -> c }.toMap, capabilityScope)
    val result = f
    capabilityScope = capabilityScope.parent
    result
  }

  private [typer] def bindCapabilities[R](binder: source.Tree, caps: List[symbols.BlockParam]): Unit =
    val capabilities = caps map { cap =>
      assertConcrete(cap.tpe.asInterfaceType)
      positions.dupPos(binder, cap)
      cap
    }
    annotations.update(Annotations.BoundCapabilities, binder, capabilities)

  private [typer] def bindingAllCapabilities[R](binder: source.Tree)(f: => R): (R, Map[InterfaceType, symbols.BlockParam]) = {
    capabilityScope = BindAll(binder, Map.empty, capabilityScope)
    val result = f
    val caps = capabilityScope.asInstanceOf[BindAll].capabilities
    capabilityScope = capabilityScope.parent
    (result, caps)
  }

  /**
   * Has the potential side-effect of creating a fresh capability. Also see [[BindAll.capabilityFor()]]
   */
  private [typer] def capabilityFor(tpe: InterfaceType): symbols.BlockParam =
    assertConcrete(tpe)
    val cap = capabilityScope.capabilityFor(tpe)
    annotations.update(Annotations.Captures, cap, CaptureSet(cap.capture))
    cap

  private [typer] def freshCapabilityFor(tpe: InterfaceType): symbols.BlockParam =
    val capName = tpe.name.rename(_ + "$capability")
    val param: BlockParam = BlockParam(capName, tpe)
    // TODO FIXME -- generated capabilities need to be ignored in LSP!
//     {
//      override def synthetic = true
//    }
    bind(param, tpe)
    param

  private [typer] def freshCapabilityFor(tpe: InterfaceType, capture: CaptureSet): symbols.BlockParam =
    val param = freshCapabilityFor(tpe)
    bind(param, capture)
    param

  private [typer] def provideCapabilities(call: source.CallLike, effs: List[InterfaceType]): List[BlockParam] =
    val caps = effs.map(capabilityFor)
    annotations.update(Annotations.CapabilityArguments, call, caps)
    caps

  private [typer] def capabilityReceiver(call: source.Do, eff: InterfaceType): BlockParam =
    val cap = capabilityFor(eff)
    annotations.update(Annotations.CapabilityReceiver, call, cap)
    cap

  //</editor-fold>

  //<editor-fold desc="(3) Typing Context">

  // first tries to find the type in the local typing context
  // if not found, it tries the global DB, since it might be a symbol of an already checked dependency
  private[typer] def lookup(s: ValueSymbol) =
    annotations.getOrElse(Annotations.ValueType, s, valueTypeOf(s))

  private[typer] def lookup(s: BlockSymbol) = (lookupBlockType(s), lookupCapture(s))

  private[typer] def lookupFunctionType(s: BlockSymbol): FunctionType =
    annotations.get(Annotations.BlockType, s)
     .map {
       case f: FunctionType => unification(f) // here we apply the substitutions known so far.
       case tpe => abort(pretty"Expected function type, but got ${tpe}.")
     }
     .orElse(functionTypeOption(s))
     .getOrElse {
       if (s.name.name == "resume")
        abort(pretty"Cannot find `resume`. Maybe you are trying to resume inside of an object literal and not as part of `try { ... } with ...`?")
       else
        abort(pretty"Cannot find type for ${s.name} -- forward uses and recursive functions need annotated return types.")
     }

  private[typer] def lookupBlockType(s: BlockSymbol): BlockType =
    annotations.get(Annotations.BlockType, s).orElse(blockTypeOption(s)).getOrElse(abort(pretty"Cannot find type for ${s.name}."))

  private[typer] def lookupCapture(s: BlockSymbol) =
    annotations.get(Annotations.Captures, s).orElse(captureOfOption(s)).getOrElse {
      s match {
        case b: TrackedParam => CaptureSet(b.capture)
        case _ => panic(pretty"Shouldn't happen: we do not have a capture for ${s}, yet.")
      }
    }

  private[typer] def bind(s: ValueSymbol, tpe: ValueType): Unit =
    annotations.update(Annotations.ValueType, s, tpe)

  private[typer] def bind(s: BlockSymbol, tpe: BlockType, capt: Captures): Unit = { bind(s, tpe); bind(s, capt) }

  private[typer] def bind(s: BlockSymbol, tpe: BlockType): Unit =
    annotations.update(Annotations.BlockType, s, tpe)

  private[typer] def bind(s: BlockSymbol, capt: Captures): Unit =
    annotations.update(Annotations.Captures, s, capt)

  private[typer] def bind(bs: Map[Symbol, ValueType]): Unit =
    bs foreach {
      case (v: ValueSymbol, t: ValueType) => bind(v, t)
      case (sym, other) => panic(pretty"Internal Error: wrong combination of symbols and types: ${sym}:${other}")
    }

  private[typer] def bind(p: ValueParam): Unit = p match {
    case s @ ValueParam(name, Some(tpe)) => bind(s, tpe)
    case s => panic(pretty"Internal Error: Cannot add $s to typing context.")
  }

  private[typer] def bind(p: TrackedParam): Unit = p match {
    case s @ BlockParam(name, tpe) => bind(s, tpe, CaptureSet(p.capture))
    case s @ ExternResource(name, tpe) => bind(s, tpe, CaptureSet(p.capture))
    case s : VarBinder => bind(s, CaptureSet(s.capture))
    case r : ResumeParam => panic("Cannot bind resume")
  }

  //</editor-fold>


  //<editor-fold desc="(5) Inferred Information for LSP">

  private[typer] def annotateInferredType(t: Tree, e: ValueType) =
    annotations.update(Annotations.InferredValueType, t, e)

  private[typer] def annotateInferredType(t: Tree, e: BlockType) =
    annotations.update(Annotations.InferredBlockType, t, e)

  private[typer] def annotateInferredEffects(t: Tree, e: Effects) =
    annotations.update(Annotations.InferredEffect, t, e)

  private[typer] def annotateTypeArgs(call: source.CallLike, targs: List[symbols.ValueType]): Unit = {
    // apply what we know before saving
    annotations.update(Annotations.TypeArguments, call, targs map unification.apply)
  }

  private[typer] def annotatedTypeArgs(call: source.CallLike): List[symbols.ValueType] = {
    annotations.apply(Annotations.TypeArguments, call)
  }

  //</editor-fold>

  //<editor-fold desc="(6) Managing Typer State">

  private[typer] def initTyperstate(): Unit = {
    annotations = Annotations.empty
    capabilityScope = GlobalCapabilityScope
    unification.init()
  }

  private[typer] def backupTyperstate(): TyperState =
    TyperState(annotations.copy, unification.backup(), capabilityScope.copy)

  private[typer] def restoreTyperstate(st: TyperState): Unit = {
    annotations = st.annotations.copy
    unification.restore(st.unification)
    capabilityScope = st.capabilityScope.copy
  }

  private[typer] def commitTypeAnnotations(): Unit = {
    val subst = unification.substitution

    var capturesForLSP: List[(Tree, CaptureSet)] = Nil

    // Since (in comparison to System C) we now have type directed overload resolution again,
    // we need to make sure the typing context and all the annotations are backtrackable.
    // This can be achieved by going back to local `annotations` which are easily backtrackable.
    // In the end, we need to postprocess the annotations; see draft below...
    annotations.updateAndCommit(Annotations.ValueType) { case (t, tpe) => subst.substitute(tpe) }
    annotations.updateAndCommit(Annotations.BlockType) { case (t, tpe) => subst.substitute(tpe) }
    annotations.updateAndCommit(Annotations.Captures) { case (t, capt) => subst.substitute(capt) }

    // Update and write out all inferred types and captures for LSP support
    // This info is currently also used by Transformer!
    annotations.updateAndCommit(Annotations.InferredValueType) { case (t, tpe) => subst.substitute(tpe) }
    annotations.updateAndCommit(Annotations.InferredBlockType) { case (t, tpe) => subst.substitute(tpe) }
    annotations.updateAndCommit(Annotations.InferredEffect) { case (t, effs) => subst.substitute(effs) }

    annotations.updateAndCommit(Annotations.TypeArguments) { case (t, targs) => targs map subst.substitute }

    annotations.updateAndCommit(Annotations.BoundCapabilities) { case (t, caps) => caps }
    annotations.updateAndCommit(Annotations.CapabilityArguments) { case (t, caps) => caps }
    annotations.updateAndCommit(Annotations.CapabilityReceiver) { case (t, caps) => caps }
  }

  //</editor-fold>
}
