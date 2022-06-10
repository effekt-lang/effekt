package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotation, Annotations, Context, ContextOps }
import effekt.context.assertions.*
import effekt.source.{ AnyPattern, Def, IgnorePattern, MatchPattern, ModuleDecl, Stmt, TagPattern, Term, Tree }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.symbols.kinds.*
import effekt.util.messages.{ ErrorReporter, FatalPhaseError }
import kiama.util.Messaging.Messages

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

/**
 * All effects inferred by Typer are required to be concrete and dealiased.
 *
 * This way, we can easily compare them for equality.
 */
class ConcreteEffects private[typer] (protected val effects: List[Effect]) {

  def toList: List[Effect] = effects
  def toEffects: Effects = Effects(effects)

  // both are known to be concrete, no need to go through validation again
  def ++(other: ConcreteEffects): ConcreteEffects = ConcreteEffects.fromList(this.effects ++ other.effects)

  // we can use set difference since type constructors are assumed to be invariant and all unification variables
  // are substituted away.
  def --(other: ConcreteEffects): ConcreteEffects = ConcreteEffects.fromList(
    (this.effects.toSet -- other.effects.toSet).toList
  )

  def isEmpty: Boolean = effects.isEmpty
  def nonEmpty: Boolean = effects.nonEmpty

  def filterNot(p: Effect => Boolean): ConcreteEffects = ConcreteEffects.fromList(effects.filterNot(p))

  def controlEffects: List[InterfaceType] = effects.controlEffects

  def forall(p: Effect => Boolean): Boolean = effects.forall(p)
  def exists(p: Effect => Boolean): Boolean = effects.exists(p)

  override def toString = toEffects.toString
}
object ConcreteEffects {
  // unsafe, doesn't perform check
  private def fromList(eff: List[Effect]): ConcreteEffects = new ConcreteEffects(eff.distinct)

  /**
   * These smart constructors should not be used directly.
   * [[Typer.asConcrete]] should be used instead, since it performs substitution and dealiasing.
   */
  def apply(eff: List[Effect])(using Context): ConcreteEffects =
    eff foreach Typer.assertConcrete
    fromList(eff)

  def apply(effs: Effects)(using Context): ConcreteEffects = apply(effs.toList)

  def empty: ConcreteEffects = fromList(Nil)
}

val Pure = ConcreteEffects.empty
val Empty = CaptureSet.empty

case class StateCapability(binder: VarBinder, tpe: ValueType) {
  lazy val (effect, get, put) = {
    val eff = Interface(binder.name, Nil)
    val get = Operation(binder.name.rename(name => "get"), Nil, Nil, tpe, Effects.Pure, eff)
    val put = Operation(binder.name.rename(name => "put"), Nil, List(ValueParam(binder.name, Some(tpe))), builtins.TUnit, Effects.Pure, eff)
    eff.ops = List(get, put)
    (eff, get, put)
  }
  lazy val param = BlockParam(binder.name, effect)
}

/**
 * Invariant: Like the result effects of Typer, all types of bound capabilities need to be concrete!
 */
sealed trait CapabilityScope {
  def copy: CapabilityScope
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam
  def parent: CapabilityScope
}
case object GlobalCapabilityScope extends CapabilityScope {
  def copy: CapabilityScope = this
  def parent: CapabilityScope = sys error "No parent"
  // If we try to find a capability for an effect that is known to be unhandled (that is no outer scope could
  // potentially handle it, then we raise an error.
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    C.abort(s"Unhandled effect ${tpe}")
}
class BindSome(binder: source.Tree, capabilities: Map[symbols.InterfaceType, symbols.BlockParam],val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindSome(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.getOrElse(tpe, parent.capabilityFor(tpe))
  override def toString: String = s"BindSome(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}
class BindAll(binder: source.Tree, var capabilities: Map[symbols.InterfaceType, symbols.BlockParam], val parent: CapabilityScope) extends CapabilityScope {
  def copy: CapabilityScope = BindAll(binder, capabilities, parent.copy)
  def capabilityFor(tpe: symbols.InterfaceType)(using C: Context): symbols.BlockParam =
    capabilities.getOrElse(tpe, {
      val freshCapability = C.freshCapabilityFor(tpe)
      capabilities = capabilities.updated(tpe, freshCapability)
      freshCapability
    })
  override def toString: String = s"BindAll(${binder.getClass.getSimpleName}, ${capabilities}, ${parent})"
}


object Typer extends Phase[NameResolved, Typechecked] {

  val phaseName = "typer"

  def run(input: NameResolved)(using Context) = Context.using(module = input.mod, focus = input.tree) {
    try {
      val NameResolved(source, tree, mod) = input

      // Effects that are lexically in scope at the top level
      val toplevelEffects = mod.imports.foldLeft(asConcrete(mod.effects)) { case (effs, mod) =>
        effs ++ asConcrete(mod.effects)
      }
      Context.initTyperstate(toplevelEffects)

      Context in {
        Context.withUnificationScope {

          // No captures are allowed on the toplevel
          given Captures = CaptureSet()

          // We split the type-checking of definitions into "pre-check" and "check"
          // to allow mutually recursive defs
          tree.defs.foreach { d => precheckDef(d) }
          tree.defs.foreach { d =>
            val Result(_, effs) = synthDef(d)
            val controlEffects = effs.toEffects.controlEffects
            if (controlEffects.nonEmpty)
              Context.at(d) {
                Context.error("Unhandled effects: " + Effects(controlEffects))
              }
          }
        }
      }

      if (Context.buffer.hasErrors) {
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

  //<editor-fold desc="expressions">

  def currentCapture(using capts: Captures) = capts

  def usingCapture(c: Captures)(using C: Context, capts: Captures): Unit =
    C.requireSubregion(c, capts)

  def insertBoxing(expr: Term, expected: Option[ValueType])(using Context, Captures): Result[ValueType] =
    expr match {
      case source.Var(id) => checkExpr(source.Box(None, source.InterfaceArg(id).inheritPosition(id)).inheritPosition(expr), expected)
      case _              => Context.abort("Currently automatic boxing is only supported for variables, please bind the block first.")
    }

  def insertUnboxing(expr: Term, expected: Option[BlockType])(using Context, Captures): Result[BlockType] =
    checkExprAsBlock(source.Unbox(expr).inheritPosition(expr), expected)

  def checkExpr(expr: Term, expected: Option[ValueType])(using Context, Captures): Result[ValueType] =
    checkAgainst(expr, expected) {
      case source.IntLit(n)     => Result(TInt, Pure)
      case source.BooleanLit(n) => Result(TBoolean, Pure)
      case source.UnitLit()     => Result(TUnit, Pure)
      case source.DoubleLit(n)  => Result(TDouble, Pure)
      case source.StringLit(s)  => Result(TString, Pure)

      case source.If(cond, thn, els) =>
        val Result(cndTpe, cndEffs) = cond checkAgainst TBoolean
        val Result(thnTpe, thnEffs) = checkStmt(thn, expected)
        val Result(elsTpe, elsEffs) = checkStmt(els, expected)

        Result(Context.join(thnTpe, elsTpe), cndEffs ++ thnEffs ++ elsEffs)

      case source.While(cond, body) =>
        val Result(_, condEffs) = cond checkAgainst TBoolean
        val Result(_, bodyEffs) = body checkAgainst TUnit
        Result(TUnit, condEffs ++ bodyEffs)

      // TODO the variable now can also be a block variable
      case source.Var(id) => id.symbol match {
        case x: VarBinder => Context.lookup(x) match {
          case (BlockTypeApp(TState.interface, List(tpe)), capt) =>
            usingCapture(capt)
            Result(tpe, Pure)
          case _ => Context.panic(s"Builtin state cannot be typed.")
        }

        case b: BlockSymbol => insertBoxing(expr, expected)
        case x: ValueSymbol => Result(Context.lookup(x), Pure)
      }

      case e @ source.Assign(id, expr) => e.definition match {
        case x: VarBinder =>
          val stTpe = Context.lookup(x) match {
            case (BlockTypeApp(TState.interface, List(tpe)), capt) =>
              usingCapture(capt)
              tpe
            case _ => Context.panic(s"Builtin state cannot be typed.")
          }
          val Result(_, eff) = expr checkAgainst stTpe
          Result(TUnit, eff)
      }

      case l @ source.Box(annotatedCapture, block) =>
        val expectedTpe = expected.collect { case BoxedType(tpe, cap) => tpe }
        val inferredCap: Captures = annotatedCapture.map { _.resolve }.getOrElse {
          Context.freshCaptVar(CaptUnificationVar.InferredBox(l))
        }

        expected.map(Context.unification.apply) foreach {
          case BoxedType(tpe, expected) => Context.requireSubregion(inferredCap, expected)
          case x: UnificationVar => Context.abort(s"Cannot infer type.")
          case tpe => Context.abort(s"Required a value of type ${tpe}, but got a block.")
        }

        given Captures = inferredCap
        val Result(inferredTpe, inferredEff) = checkBlockArgument(block, expectedTpe)

        Result(BoxedType(inferredTpe, inferredCap), inferredEff)

      case source.Unbox(_) => insertBoxing(expr, expected)

      case c @ source.Call(t: source.IdTarget, targs, vargs, bargs) =>
        checkOverloadedCall(c, t, targs map { _.resolve }, vargs, bargs, expected)

      case c @ source.Call(source.ExprTarget(e), targs, vargs, bargs) =>
        val Result(tpe, funEffs) = checkExprAsBlock(e, None) match {
          case Result(b: FunctionType, capt) => Result(b, capt)
          case _ => Context.abort("Callee is required to have function type")
        }

        val Result(t, eff) = checkCallTo(c, None, "function", tpe, targs map { _.resolve }, vargs, bargs, expected)
        Result(t, eff ++ funEffs)

      case tree @ source.TryHandle(prog, handlers) =>

        // (1) extract all handled effects and capabilities
        var providedCapabilities: List[symbols.BlockParam] = Nil
        handlers foreach Context.withFocus { h =>
          val effect: InterfaceType = h.effect.resolve
          val capability = h.capability.map { _.symbol }.getOrElse { Context.freshCapabilityFor(effect) }

          if (providedCapabilities contains effect) {
            Context.error(s"Effect ${effect} is handled twice.")
          } else {
            // TODO also relate it to the lexical region
            Context.bind(capability, CaptureSet(capability.capture))
            providedCapabilities = providedCapabilities :+ capability
          }
        }

        var handlerEffs: ConcreteEffects = Pure

        // Create a fresh capture variable for the continuations ?Ck
        val continuationCapt = Context.freshCaptVar(CaptUnificationVar.HandlerRegion(tree))
        val continuationCaptHandled = Context.without(continuationCapt, providedCapabilities.map(_.capture))

        val Result(ret, effs) = {

          val Result(ret, effs) = Context.bindingCapabilities(tree, providedCapabilities) {
            // All used captures flow into the continuation capture, except the ones handled by this handler.
            given Captures = continuationCaptHandled
            checkStmt(prog, expected)
          }

          // Also all capabilities used by the handler flow into the capture of the continuation
          given Captures = continuationCaptHandled

          handlers foreach Context.withFocus { h =>
            val effectSymbol: Interface = h.definition

            val tparams = effectSymbol.tparams
            val targs = h.effect.tparams.map(_.resolve)
            val tsubst = (tparams zip targs).toMap

            // (3) check all operations are covered
            val covered = h.clauses.map { _.definition }
            val notCovered = effectSymbol.ops.toSet -- covered.toSet

            if (notCovered.nonEmpty) {
              val explanation = notCovered.map { op => s"${op.name} of effect ${op.effect.name}" }.mkString(", ")
              Context.error(s"Missing definitions for effect operations: ${explanation}")
            }

            if (covered.size > covered.distinct.size)
              Context.error(s"Duplicate definitions of effect operations")

            h.clauses foreach Context.withFocus {
              case d @ source.OpClause(op, params, body, resume) =>
                val declaration = d.definition

                val declaredType = Context.lookupFunctionType(declaration)

                // Create fresh type parameters for existentials.
                // TODO they could be annotated!
                //     effect E[A, B, ...] { def op[C, D, ...]() = ... }  !--> op[A, B, ..., C, D, ...]
                // The parameters C, D, ... are existentials
                val existentials: List[TypeVar] = declaredType.tparams.drop(targs.size).map { r => TypeVar(r.name) }

                // (1) Instantiate block type of effect operation
                val (rigids, crigids, canonical, FunctionType(tps, cps, vps, Nil, tpe, effs)) =
                  Context.instantiate(Context.lookupFunctionType(declaration), targs ++ existentials)

                // (3) check parameters
                if (vps.size != params.size)
                  Context.abort(s"Wrong number of value arguments, given ${params.size}, but ${op.name} expects ${vps.size}.")

                (params zip vps).foreach {
                  case (param, decl) =>
                    val sym = param.symbol
                    val annotType = sym.tpe
                    annotType.foreach { t => Context.at(param) {
                      // Here we are contravariant: declared types have to be subtypes of the actual types
                      Context.requireSubtype(decl, t)
                    }}
                    Context.bind(sym, annotType.getOrElse(decl))
                }

                // (5) synthesize type of continuation
                val resumeType = if (declaration.isBidirectional) {
                  // resume { e }
                  val other = declaration.otherEffects
                  val captureParam = CaptureParam(Name.local("resumeBlock"))
                  FunctionType(Nil, List(captureParam), Nil, List(FunctionType(Nil, Nil, Nil, Nil, tpe, other)), ret, Effects.Pure)
                } else {
                  // resume(v)
                  FunctionType(Nil, Nil, List(tpe), Nil, ret, Effects.Pure)
                }

                Context.bind(Context.symbolOf(resume).asBlockSymbol, resumeType, continuationCapt)
                Context in {
                  val Result(_, heffs) = body checkAgainst ret
                  handlerEffs = handlerEffs ++ heffs

                  val typesInEffects = freeTypeVars(heffs)
                  existentials.foreach { t =>
                    if (typesInEffects.contains(t)) {
                      Context.error(s"Type variable ${t} escapes its scope as part of the effect types: $heffs")
                    }
                  }
                }
            }
          }

          Result(ret, effs)
        }

        val handled = asConcrete(Effects(providedCapabilities.map { cap => cap.tpe.asEffect }))

        val unusedEffects = handled -- effs

        if (unusedEffects.nonEmpty)
          Context.warning("Handling effects that are not used: " + unusedEffects)

        // The captures of the handler continue flowing into the outer scope
        usingCapture(continuationCapt)

        Result(ret, (effs -- handled) ++ handlerEffs)

      case tree @ source.Match(sc, clauses) =>

        // (1) Check scrutinee
        // for example. tpe = List[Int]
        val Result(tpe, effs) = checkExpr(sc, None)

        // (2) check exhaustivity
        checkExhaustivity(tpe, clauses.map { _.pattern })

        var resEff = effs

        val tpes = clauses.map {
          case source.MatchClause(p, body) =>
            // (3) infer types for all clauses
            Context.bind(checkPattern(tpe, p))
            val Result(clTpe, clEff) = Context in { checkStmt(body, expected) }
            resEff = resEff ++ clEff
            clTpe
        }

        // Clauses could in general be empty if there are no constructors
        // In that case the scrutinee couldn't have been constructed and
        // we can unify with everything.
        Result(Context.join(tpes: _*), resEff)

      case source.Select(_, _) =>
        insertBoxing(expr, expected)

      case source.Hole(stmt) =>
        val Result(tpe, effs) = checkStmt(stmt, None)
        Result(expected.getOrElse(TBottom), Pure)
    }

  /**
   * We defer checking whether something is first-class or second-class to Typer now.
   */
  def checkExprAsBlock(expr: Term, expected: Option[BlockType])(using Context, Captures): Result[BlockType] =
    checkBlockAgainst(expr, expected) {
      case u @ source.Unbox(expr) =>
        val expectedTpe = expected map {
          tpe =>
            val captVar = Context.freshCaptVar(CaptUnificationVar.InferredUnbox(u))
            BoxedType(tpe, captVar)
        }
        val Result(vtpe, eff1) = checkExpr(expr, expectedTpe)
        // TODO here we also need unification variables for block types!
        // C.unify(tpe, BoxedType())
        vtpe match {
          case BoxedType(btpe, capt2) =>
            usingCapture(capt2)
            Result(btpe, eff1)
          case _ => Context.abort(s"Unbox requires a boxed type, but got $vtpe")
        }

      case source.Var(id) => ???
//      id.symbol match {
//        case b: BlockSymbol =>
//          val (tpe, capt) = Context.lookup(b)
//          tpe / capt
//        case e: ValueSymbol => insertUnboxing(expr)
//      }

      case s @ source.Select(expr, selector) => ???
//        checkExprAsBlock(expr) match {
//          case ((i @ InterfaceType(interface, targs)) / capt) =>
//            // (1) find the operation
//            // try to find an operation with name "selector"
//            val op = interface.ops.collect {
//              case op if op.name.name == selector.name => op
//            } match {
//              case Nil      => Context.at(s) { Context.abort(s"Cannot select ${selector.name} in type ${i}") }
//              case List(op) => op
//              case _        => Context.at(s) { Context.abort(s"Multiple operations match ${selector.name} in type ${i}") }
//            }
//            // assign the resolved operation to the identifier
//            Context.assignSymbol(selector, op)
//
//            // (2) substitute type arguments
//            val tsubst = (interface.tparams zip targs).toMap
//            tsubst.substitute(op.toType) / capt
//
//          case _ => Context.abort(s"Selection requires an interface type.")
//        }
//
      case other => insertUnboxing(other, expected)
    }

  //</editor-fold>

  //<editor-fold desc="pattern matching">

  /**
   * This is a quick and dirty implementation of coverage checking. Both performance, and error reporting
   * can be improved a lot.
   *
   * TODO Maybe move exhaustivity check to a separate phase AFTER typer? This way all types are inferred.
   */
  def checkExhaustivity(sc: ValueType, cls: List[MatchPattern])(using Context): Unit = ()
  //  {
  //    val catchall = cls.exists { p => p.isInstanceOf[AnyPattern] || p.isInstanceOf[IgnorePattern] }
  //
  //    if (catchall)
  //      return ;
  //
  //    sc match {
  //      case TypeConstructor(t: DataType) =>
  //        t.variants.foreach { variant =>
  //          checkExhaustivity(variant, cls)
  //        }
  //
  //      case TypeConstructor(t: Record) =>
  //        val (related, unrelated) = cls.collect { case p: TagPattern => p }.partitionMap {
  //          case p if p.definition == t => Left(p.patterns)
  //          case p => Right(p)
  //        }
  //
  //        if (related.isEmpty) {
  //          Context.error(s"Non exhaustive pattern matching, missing case for ${sc}")
  //        }
  //
  //        (t.fields.map { f => f.tpe } zip related.transpose) foreach {
  //          case (t, ps) => checkExhaustivity(t, ps)
  //        }
  //      case other =>
  //        ()
  //    }
  //  }

  def checkPattern(sc: ValueType, pattern: MatchPattern)(using Context, Captures): Map[Symbol, ValueType] = Context.focusing(pattern) {
    case source.IgnorePattern()    => Map.empty
    case p @ source.AnyPattern(id) => Map(p.symbol -> sc)
    case p @ source.LiteralPattern(lit) =>
      lit.checkAgainst(sc)
      Map.empty
    case p @ source.TagPattern(id, patterns) =>

      // symbol of the constructor we match against
      val sym: Record = Context.symbolOf(id) match {
        case c: Record => c
        case _         => Context.abort("Can only match on constructors")
      }

      // (4) Compute blocktype of this constructor with rigid type vars
      // i.e. Cons : `(?t1, List[?t1]) => List[?t1]`
      val (rigids, crigids, canonical, FunctionType(_, _, vps, _, ret, _)) = Context.instantiate(sym.toType, Nil)

      // (5) given a scrutinee of `List[Int]`, we learn `?t1 -> Int`
      Context.requireSubtype(sc, ret)

      // (6) check for existential type variables
      // at the moment we do not allow existential type parameters on constructors.
      //      val skolems = Context.skolems(rigids)
      //      if (skolems.nonEmpty) {
      //        Context.error(s"Unbound type variables in constructor ${id}: ${skolems.map(_.underlying).mkString(", ")}")
      //      }

      // (8) check nested patterns
      var bindings = Map.empty[Symbol, ValueType]

      if (patterns.size != vps.size)
          Context.error(s"Wrong number of pattern arguments, given ${patterns.size}, expected ${vps.size}.")

      (patterns zip vps) foreach {
        case (pat, par: ValueType) =>
          bindings ++= checkPattern(par, pat)
      }

      bindings
  }

  //</editor-fold>

  //<editor-fold desc="statements and definitions">

  def checkStmt(stmt: Stmt, expected: Option[ValueType])(using Context, Captures): Result[ValueType] =
    checkAgainst(stmt, expected) {
      case source.DefStmt(b, rest) =>
        val Result(t, effBinding) = Context in { precheckDef(b); synthDef(b) }
        val Result(r, effStmt) = checkStmt(rest, expected)
        Result(r, effBinding ++ effStmt)

      // <expr> ; <stmt>
      case source.ExprStmt(e, rest) =>
        val Result(_, eff1) = checkExpr(e, None)
        val Result(r, eff2) = checkStmt(rest, expected)
        Result(r, eff1 ++ eff2)

      case source.Return(e)        => checkExpr(e, expected)

      case source.BlockStmt(stmts) => Context in { checkStmt(stmts, expected) }
    }

  // not really checking, only if defs are fully annotated, we add them to the typeDB
  // this is necessary for mutually recursive definitions
  def precheckDef(d: Def)(using Context): Unit = Context.focusing(d) {
    case d @ source.FunDef(id, tps, vps, bps, ret, body) =>
      val fun = d.symbol
      val cap = Context.freshCaptVar(CaptUnificationVar.FunctionRegion(d))
      // TODO here it would be better to just maintain the invariant that it is a single unification variable
      Context.bind(fun, cap)
      fun.annotatedType.foreach { tpe => Context.bind(d.symbol, tpe) }

    case d @ source.ExternFun(pure, id, tps, vps, bps, tpe, body) =>
      val fun = d.symbol
      Context.bind(fun, fun.toType, Empty)
      if (fun.effects.controlEffects.nonEmpty) {
        Context.abort("Unhandled control effects on extern defs not allowed")
      }

    case d @ source.InterfaceDef(id, tparams, ops, isEffect) =>
      d.symbol.ops.foreach { op =>
        val tpe = op.toType
        wellformed(tpe)
        Context.bind(op, tpe)
      }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.symbol
        Context.bind(sym, sym.toType, CaptureSet())

        sym.fields.foreach { field =>
          val tpe = field.toType
          wellformed(tpe)
          Context.bind(field, tpe, CaptureSet())
        }
      }

    case d @ source.RecordDef(id, tparams, fields) =>
      val rec = d.symbol
      Context.bind(rec, rec.toType, CaptureSet())
      rec.fields.foreach { field =>
        val tpe = field.toType
        wellformed(tpe)
        Context.bind(field, tpe, CaptureSet())
      }

    case d: source.TypeDef   => wellformed(d.symbol.tpe)
    case d: source.EffectDef => wellformed(d.symbol.effs)
    case _                   => ()
  }

  def synthDef(d: Def)(using Context, Captures): Result[Unit] = Context.at(d) {
    d match {
      case d @ source.FunDef(id, tps, vps, bps, ret, body) =>
        val sym = d.symbol
        // was assigned by precheck
        val functionCapture = Context.lookupCapture(sym)
        val selfRegion = Context.getSelfRegion(d)

        // We can also try to solve for the function capture, after checking the function.
        // Hence we provide it to `withUnificationScope`.
        val captVars = functionCapture match {
          case x: CaptUnificationVar => List(x)
          case _ => Nil
        }
        Context.withUnificationScope(captVars) {

          sym.vparams foreach Context.bind
          sym.bparams foreach Context.bind

          val inferredCapture = Context.freshCaptVar(CaptUnificationVar.FunctionRegion(d))

          Context.withRegion(selfRegion) {
            (sym.annotatedType: @unchecked) match {
              case Some(annotated) =>
                // the declared effects are considered as bound
                val bound: ConcreteEffects = annotated.effects
                val capabilities = bound.controlEffects.map { tpe => Context.freshCapabilityFor(tpe) }
                val captures = capabilities.map { _.capture }

                // block parameters and capabilities for effects are assumed bound
                given Captures = inferredCapture

                val Result(tpe, effs) = Context.bindingCapabilities(d, capabilities) {
                   Context in { body checkAgainst annotated.result }
                }
                Context.wellscoped(effs)
                Context.annotateInferredType(d, tpe)
                Context.annotateInferredEffects(d, effs.toEffects)

                println(s">>>> ${d.id.name}: Inferred ${inferredCapture}, functionCapture ${functionCapture}. Captures ${captures}")
                Context.unification.dumpConstraints()

                Context.requireSubregionWithout(inferredCapture, functionCapture, annotated.cparams ++ captures ++ List(selfRegion))

                Context.unification.dumpConstraints()

                Result((), effs -- bound)
              case None =>

                // to subtract the capabilities, which are only inferred bottom up, we need a **second** unification variable
                given Captures = inferredCapture

                // all effects are handled by the function itself (since they are inferred)
                val (Result(tpe, effs), caps) = Context.bindingAllCapabilities(d) {
                  Context in { checkStmt(body, None) }
                }
                Context.wellscoped(effs) // check they are in scope

                // TODO also add capture parameters for inferred capabilities
                val funType = sym.toType(tpe, effs.toEffects)

                // The order of effects annotated to the function is the canonical ordering for capabilities
                val capabilities = effs.controlEffects.map { caps.apply }

                Context.bindCapabilities(d, capabilities)

                Context.bind(sym, funType)
                Context.annotateInferredType(d, tpe)
                Context.annotateInferredEffects(d, effs.toEffects)

                // we subtract all capabilities introduced by this function to compute its capture
                Context.requireSubregionWithout(inferredCapture, functionCapture, (sym.bparams ++ capabilities).map(_.capture) ++ List(selfRegion))

                Result((), Pure)
            }
          }
        }
      case d @ source.InterfaceDef(id, tparams, ops, isEffect) =>
        val effectDecl = d.symbol
        effectDecl.ops foreach { op =>
          if (op.otherEffects.toList contains op.appliedEffect) {
            Context.error("Bidirectional effects that mention the same effect recursively are not (yet) supported.")
          }
        }
        Context.withEffect(effectDecl)
        Result((), Pure)

      case d @ source.ValDef(id, annot, binding) =>
        val Result(t, effBinding) = d.symbol.tpe match {
          case Some(t) =>
            binding checkAgainst t
          case None => checkStmt(binding, None)
        }
        Context.bind(d.symbol, t)
        Result((), effBinding)

      case d @ source.VarDef(id, annot, reg, binding) =>
        val sym = d.symbol

        // we use the current region as an approximation for the state
        val stCapt = reg map Context.symbolOf map {
          case b: BlockSymbol =>
            Context.lookup(b) match {
              case (TRegion, capt) => capt
              case _               => Context.at(reg.get) { Context.abort(s"Expected a region.") }
            }
          case _ => Context.at(reg.get) { Context.abort(s"Expected a region.") }
        } getOrElse { CaptureSet(Context.region) }

        val Result(tpeBind, effBind) = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding, None)
        }
        val stTpe = BlockTypeApp(TState.interface, List(tpeBind))

        Context.bind(sym, stTpe, stCapt)
        // TODO continue. Do we need to still make up a blockparam for state now that VarDef is a block symbol?
        //Context.stateFor(sym)

        Result((), effBind)

      case d @ source.ExternFun(pure, id, tps, vps, bps, tpe, body) =>
        d.symbol.vparams foreach Context.bind
        d.symbol.bparams foreach Context.bind
        Result((), Pure)

      // all other definitions have already been prechecked
      case d =>
        Result((), Pure)
    }
  }

  //</editor-fold>

  //<editor-fold desc="Function calls, arguments, and parameters">

  def checkBlockArgument(arg: source.BlockArg, expected: Option[BlockType])(using Context, Captures): Result[BlockType] =
    (arg, expected) match {
      // Use expected type, if present
      case (arg: source.FunctionArg, Some(tpe: FunctionType)) =>
        checkFunctionArgument(arg, tpe)
      case (arg@source.FunctionArg(tparams, vparams, bparams, body), None) => Context in {
        val tps = tparams.map { p => p.symbol.asTypeVar }
        val vps = vparams.map { p =>
          val param = p.symbol
          val tpe = p.symbol.tpe.getOrElse {
            Context.abort("Expected type needs to be known for function arguments at the moment.")
          }
          Context.bind(param, tpe)
          tpe
        }
        val bps = bparams.map { p =>
          val param = p.symbol
          val tpe = param.tpe
          Context.bind(param, tpe)
          tpe
        }

        // like with non-annotated function definitions, we need to use a separate unification variable to
        // subtract bound (but inferred) capabilities later.
        val inferredCapture = Context.freshCaptVar(CaptUnificationVar.AnonymousFunctionRegion(arg))
        val (Result(tpe, effs), caps) = Context.bindingAllCapabilities(arg) {
          given Captures = inferredCapture
          Context in { checkStmt(body, None) }
        }
        Context.wellscoped(effs) // check effects are in scope

        val funType = FunctionType(tps, Nil, vps, bps, tpe, effs.toEffects)

        // The order of effects annotated to the function is the canonical ordering for capabilities
        val capabilities = effs.controlEffects.map { caps.apply }
        Context.bindCapabilities(arg, capabilities)

        // Like with functions, bound parameters and capabilities are not closed over
        Context.requireSubregionWithout(inferredCapture, currentCapture, (bparams.map(_.symbol) ++ capabilities).map(_.capture))

        Result(funType, Pure)
      }
      case (rg@source.InterfaceArg(id), None) =>
        val (btpe, capt) = Context.lookup(id.symbol.asBlockSymbol)
        usingCapture(capt)
        Result(btpe, Pure)
      case (rg@source.InterfaceArg(id), Some(expected)) =>
        if (!id.symbol.isInstanceOf[BlockSymbol]) Context.at(rg) {
          Context.abort(s"Expected a block variable, but ${id.name} is a value. Maybe use explicit syntax: { () => ${id.name} }")
        }
        val (btpe, capt) = Context.lookup(id.symbol.asBlockSymbol)
        Context.requireSubtype(btpe, expected)
        usingCapture(capt)
        Result(btpe, Pure)
      case _ =>
        Context.abort("Can only type check function arguments, right now. Not capability arguments.")
    }

  // Example.
  //   BlockParam: def foo { f: Int => String / Print }
  //   BlockArg: foo { n => println("hello" + n) }
  //     or
  //   BlockArg: foo { (n: Int) => println("hello" + n) }
  def checkFunctionArgument(arg: source.FunctionArg, expected: FunctionType)(using Context, Captures): Result[BlockType] = Context.focusing(arg) {

    case decl @ source.FunctionArg(tparams, vparams, bparams, body) =>

      // (1) Apply what we already know.
      val bt @ FunctionType(tps, cps, vps, bps, tpe1, handled) = expected

      // (2) Check wellformedness
      if (tps.size != tparams.size)
        Context.abort(s"Wrong number of type arguments, given ${tparams.size}, but function expects ${tps.size}.")

      if (vps.size != vparams.size)
        Context.abort(s"Wrong number of value arguments, given ${vparams.size}, but function expects ${vps.size}.")

      if (bps.size != bparams.size)
        Context.abort(s"Wrong number of block arguments, given ${bparams.size}, but function expects ${bps.size}.")

      // (3) Substitute type parameters
      val typeParams = tparams.map { p => p.symbol.asTypeVar }
      val typeSubst = (tps zip typeParams).toMap

      // (4) Check type annotations against declaration
      val valueTypes = (vparams zip vps) map {
        case (param, expected) =>
          val adjusted = typeSubst substitute expected
          val tpe = param.symbol.tpe.map { got =>
              Context.at(param) { Context.requireSubtype(adjusted, got) }
              got
          } getOrElse { adjusted }
          // bind types to check body
          Context.bind(param.symbol, tpe)
          tpe
      }
      val blockTypes = (bparams zip bps) map {
        case (param, exp) =>
          val adjusted = typeSubst substitute exp
          val got = param.symbol.tpe
          Context.at(param) { Context.requireSubtype(adjusted, got) }
          // bind types to check body
          Context.bind(param.symbol, got)
          got
      }
      val captureParams = bparams.map { p => p.symbol.capture }
      val adjustedReturn = typeSubst substitute tpe1

      // Bind additional capabilities for all control effects "handled" by the function argument.
      val adjustedHandled = typeSubst substitute handled
      val bound: ConcreteEffects = adjustedHandled
      // TODO share code with FunDef and BindAll
      val capabilities = bound.controlEffects.map { tpe => Context.freshCapabilityFor(tpe) }

      val bodyRegion = Context.freshCaptVar(CaptUnificationVar.AnonymousFunctionRegion(arg))

      val Result(bodyType, bodyEffs) = Context.bindingCapabilities(decl, capabilities) {
         given Captures = bodyRegion
         body checkAgainst adjustedReturn
      }
      Context.requireSubregionWithout(bodyRegion, currentCapture, captureParams ++ capabilities.map(_.capture))

      val tpe = FunctionType(typeParams, captureParams, valueTypes, blockTypes, bodyType, adjustedHandled)

      Result(tpe, bodyEffs -- bound)
  }

  def findFunctionTypeFor(sym: TermSymbol)(using Context): FunctionType = sym match {
    case b: BlockSymbol => Context.lookupFunctionType(b)
    case v: ValueSymbol =>
      Context.unification(Context.lookup(v)) match {
      case BoxedType(b: FunctionType, _) => b
      case b => Context.abort(s"Required function type, but got ${b}")
    }
  }

  /**
   * Attempts to check a potentially overladed call, not reporting any errors but returning them instead.
   *
   * This is necessary for overload resolution by trying all alternatives.
   *   - if there is multiple without errors: Report ambiguity
   *   - if there is no without errors: report all possible solutions with corresponding errors
   */
  def checkOverloadedCall(
    call: source.Call,
    target: source.IdTarget,
    targs: List[ValueType],
    vargs: List[source.Term],
    bargs: List[source.BlockArg],
    expected: Option[ValueType]
  )(using Context, Captures): Result[ValueType] = {

    val scopes = target.definition match {
      // an overloaded call target
      case CallTarget(name, syms) => syms
      // already resolved by a previous attempt to typecheck
      case sym                    => List(Set(sym))
    }

    val stateBefore = Context.backupTyperstate()

    // TODO right now unhandled effects (via capability search) influences overload resolution.
    //  examples/neg/see existential_effect_leaks.effekt
    //
    //  We should establish proper shadowing here!
    //
    // Potential Design
    // ----------------
    // We can get rid of the complexity of backtracking by
    // 1) filter out overloads that do not match arity / kind wise
    // 2) check whether there are multiple functions that overlap on their value arguments (raise ambiguity if that is the case)
    // 3) infer the value arguments *once* without expected type.
    //
    // For each (non-empty) scope,
    //   For each candidate in that scope (might require backtracking on the unifier!):
    //   + see whether the value arguments *could* unify with the expected value parameter types
    // - If there are multiple possible candidates -> Ambiguity Error
    // - If there is none: proceed to outer scope
    // - If there is exactly one match, fully typecheck the call with this.
    val results = scopes map { scope =>
      scope.toList.map {
        case receiver =>
          receiver -> Try {
            Context.restoreTyperstate(stateBefore)
            val funTpe = findFunctionTypeFor(receiver)

            val receiverAsBlockSymbol = receiver match {
              case b: BlockSymbol => Some(b)
              case other => None
            }

            val Result(tpe, effs) = checkCallTo(call, receiverAsBlockSymbol, receiver.name.name, funTpe, targs, vargs, bargs, expected)
            (Result(tpe, effs), Context.backupTyperstate())
          }
      }
    }

    val successes = results.map { scope => scope.collect { case (sym, Right(r)) => sym -> r } }
    val errors = results.flatMap { scope => scope.collect { case (sym, Left(r)) => sym -> r } }

    successes foreach {
      // continue in outer scope
      case Nil => ()

      // Exactly one successful result in the current scope
      case List((sym, (tpe, st))) =>
        // use the typer state after this checking pass
        Context.restoreTyperstate(st)
        // reassign symbol of fun to resolved calltarget symbol
        Context.assignSymbol(target.id, sym)

        return tpe

      // Ambiguous reference
      case results =>
        val sucMsgs = results.map {
          case (sym, tpe) =>
            s"- ${sym.name} of type ${findFunctionTypeFor(sym)}"
        }.mkString("\n")

        val explanation =
          s"""| Ambiguous reference to ${target.id}. The following blocks would typecheck:
              |
              |${sucMsgs}
              |""".stripMargin

        Context.abort(explanation)
    }

    errors match {
      case Nil =>
        Context.abort("Cannot typecheck call, no function found")

      // exactly one error
      case List((sym, errs)) =>
        val msg = errs.head
        val msgs = errs.tail
        Context.buffer.append(msgs)
        // reraise and abort
        // TODO clean this up
        Context.at(msg.value.asInstanceOf[Tree]) { Context.abort(msg.label) }

      case failed =>
        // reraise all and abort
        val msgs = failed.flatMap {
          // TODO also print signature!
          case (block, msgs) =>
            val fullname = block.name match {
              case q: QualifiedName => q.qualifiedName
              case n                => n.name
            }
            msgs.map { m => m.copy(label = s"Possible overload ${fullname}: ${m.label}") }
        }.toVector

        Context.reraise(msgs)

        Context.abort(s"Cannot typecheck call. There are multiple overloads, which all fail to check.")
    }
  }

  def checkCallTo(
    call: source.Call,
    receiver: Option[BlockSymbol],
    name: String,
    funTpe: FunctionType,
    targs: List[ValueType],
    vargs: List[source.Term],
    bargs: List[source.BlockArg],
    expected: Option[ValueType]
  )(using Context, Captures): Result[ValueType] = {

    if (targs.nonEmpty && targs.size != funTpe.tparams.size)
      Context.abort(s"Wrong number of type arguments ${targs.size}")

    if (vargs.size != funTpe.vparams.size)
      Context.error(s"Wrong number of value arguments, given ${vargs.size}, but ${name} expects ${funTpe.vparams.size}.")

    if (bargs.size != funTpe.bparams.size)
      Context.error(s"Wrong number of block arguments, given ${bargs.size}, but ${name} expects ${funTpe.bparams.size}.")

    val functionCapture = currentCapture

    // (1) Instantiate blocktype
    // e.g. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
    val (typeArgs, captArgs, canonicalEffs, bt @ FunctionType(_, _, vps, bps, ret, retEffs)) = Context.instantiate(funTpe, targs)

    // (2) check return type
    expected.foreach { expectedReturn => Context.requireSubtype(ret, expectedReturn) }

    var effs: ConcreteEffects = Pure

    (vps zip vargs) foreach { case (tpe, expr) =>
      val Result(t, eff) = checkExpr(expr, Some(tpe))
      effs = effs ++ eff
    }

    (bps zip bargs zip captArgs) foreach { case ((tpe, expr), capt) =>
      Context.requireSubregion(capt, functionCapture)
      given Captures = capt
      val Result(t, eff) = checkBlockArgument(expr, Some(tpe))
      effs = effs ++ eff
    }

    // We add return effects last to have more information at this point to
    // concretize the effect.
    effs = effs ++ retEffs

    // annotate call node with inferred type arguments
    Context.annotateTypeArgs(call, typeArgs)

    // Annotate the call target tree with the additional capabilities
    // We need to establish the canonical ordering of capabilities.
    // 1) we have to use the capabilities, which are annotated on the original function type
    // 2) we need to dealias
    // 3) we need to compute distinct effects on the dealiased list
    // 3) and only then substitute
    //
    // This is important since
    //   [A, B](): Unit / { State[A], State[B] }
    // with A := Int and B := Int requires us to pass two capabilities.
    val capabilities = Context.provideCapabilities(call, canonicalEffs.map(Context.unification.apply).controlEffects)

    // TODO it probably would be better to go back to explicitly represent effect calls, for instance,
    //   with `do raise()`.
    receiver foreach {
      case op: Operation =>
        // the first capability provided above is the one for the effect.
        val capability = capabilities.head
        usingCapture(CaptureSet(capability.capture))
      case recv: BlockSymbol => usingCapture(Context.lookupCapture(recv))
    }

    Result(ret, effs)
  }

  /**
   * Returns Left(Messages) if there are any errors
   *
   * In the case of nested calls, currently only the errors of the innermost failing call
   * are reported
   */
  private def Try[T](block: => T)(using C: Context): Either[Messages, T] = {
    import kiama.util.Severities.Error

    val (msgs, optRes) = Context withMessages {
      try { Some(block) } catch {
        case FatalPhaseError(msg) =>
          C.error(msg)
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

  //<editor-fold desc="Helpers and Extension Methods">

  // TODO first substitute and dealias, then check concrete, then convert.
  implicit def asConcrete(effs: Effects)(using Context): ConcreteEffects =
    ConcreteEffects(Context.unification(effs))

  /**
   * Asserts that all effects in the list are _concrete_, that is,
   * no unification variables (neither type, nor region) are allowed.
   *
   * If all effects are concrete (and we assume effect type constructors are INVARIANT):
   *   - we can use structural equality to compare them
   *   - we can use sets and hash maps
   *
   * Consequences:
   *   - If we try to add an effect that is not concrete, we should raise an "Could not infer..." error.
   *   - We need to substitute early in order to have more concrete effects.
   *   - Requiring effects to be concrete also simplifies effect-set comparison in [[TypeComparer]].
   *
   * TODO Question: should we ALWAYS require effects to be concrete, also when compared with [[TypeUnifier]]?
   */
  private[typer] def assertConcrete(effs: Effects)(using C: Context): Unit =
    if (!isConcreteEffects(effs)) C.abort(s"Effects need to be fully known: ${effs}")

  private[typer] def assertConcrete(eff: Effect)(using C: Context): Unit =
    if (!isConcreteEffect(eff)) {
      C.abort(s"Effects need to be fully known: ${eff}")
    }

  private[typer] def isConcreteValueType(tpe: ValueType): Boolean = tpe match {
    case x: UnificationVar => false
    case x: TypeVar => true
    case t: TypeConstructor => true
    case t : BuiltinType => true
    case ValueTypeApp(tpe, args) => isConcreteValueType(tpe) && args.forall(isConcreteValueType)
    case BoxedType(tpe, capture) => isConcreteBlockType(tpe) && isConcreteCaptureSet(capture)
    // aliases should have been resolved by now
    case TypeAlias(name, tparams, tpe) => false
  }

  private def isConcreteBlockType(tpe: BlockType): Boolean = tpe match {
    case FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
      vparams.forall(isConcreteValueType) && bparams.forall(isConcreteBlockType) && isConcreteValueType(result) && isConcreteEffects(effects)
    case BlockTypeApp(tpe, args) => isConcreteBlockType(tpe) && args.forall(isConcreteValueType)
    case t: Interface => true
    case b: BuiltinEffect => true
  }
  private def isConcreteCaptureSet(capt: Captures): Boolean = capt.isInstanceOf[CaptureSet]

  private def isConcreteEffect(eff: Effect): Boolean = eff match {
    case t: Interface => true
    case t: BuiltinEffect => true
    case BlockTypeApp(tpe, args) => isConcreteBlockType(tpe) && args.forall(isConcreteValueType)
    // aliases should have been resolved by now
    case EffectAlias(name, tparams, effs) => false // isConcreteEffects(effs)
  }
  private def isConcreteEffects(effs: Effects): Boolean = effs.toList.forall(isConcreteEffect)

  private def freeTypeVars(o: Any): Set[TypeVar] = o match {
    case t: symbols.TypeVar => Set(t)
    case FunctionType(tps, cps, vps, bps, ret, effs) =>
      freeTypeVars(vps) ++ freeTypeVars(bps) ++ freeTypeVars(ret) ++ freeTypeVars(effs) -- tps.toSet
    case e: Effects            => freeTypeVars(e.toList)
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case _ =>
      Set.empty
  }

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
      expected foreach { Context.requireSubtype(got, _) }
      Context.annotateInferredType(t, got)
      Context.annotateInferredEffects(t, effs.toEffects)
      Result(got, effs)
    }

  def checkBlockAgainst[T <: Tree](t: T, expected: Option[BlockType])(f: T => Result[BlockType])(using Context, Captures): Result[BlockType] =
    Context.at(t) {
      val Result(got, effs) = f(t)
      wellformed(got)
      wellformed(effs.toEffects)
      expected foreach { Context.requireSubtype(got, _) }
      Context.annotateInferredType(t, got)
      Context.annotateInferredEffects(t, effs.toEffects)
      Result(got, effs)
    }

  /**
   * Helper methods on function symbols to retreive its type
   * either from being annotated or by looking it up (if already typechecked...)
   */
  extension (fun: Fun)(using Context) {
    // invariant: only works if ret is defined!
    def toType: FunctionType =
      annotatedType.get
    def toType(result: ValueType, effects: Effects): FunctionType =
      FunctionType(fun.tparams, fun.bparams.map { _.capture }, fun.vparams.map { p => p.tpe.get }, fun.bparams.map { p => p.tpe }, result, effects)
    def annotatedType: Option[FunctionType] =
      for { result <- fun.annotatedResult; effects <- fun.annotatedEffects } yield toType(result, effects)

    def effects: Effects =
      annotatedType
        .map { tpe => tpe.effects }
        .getOrElse { Context.lookupFunctionType(fun).effects }
  }

  //</editor-fold>

}

/**
 * Instances of this class represent an immutable backup of the typer state
 */
private[typer] case class TyperState(lexicalEffects: List[Interface], annotations: Annotations, unification: UnificationState, capabilityScope: CapabilityScope)

trait TyperOps extends ContextOps { self: Context =>

  /**
   * The unification engine, keeping track of constraints and the current unification scope
   *
   * Contains mutable variables. The methods [[unification.backup()]] and [[unification.restore()]]
   * allow to save a copy of the current state.
   */
  private[typer] val unification = new Unification(using self)
  export unification.{ requireSubtype, requireSubregion, join, instantiate, dealias, freshCaptVar, without, requireSubregionWithout }

  // opens a fresh unification scope
  private[typer] def withUnificationScope[T](additional: List[CaptUnificationVar])(block: => T): T = {
    unification.enterScope()
    val res = block
    unification.leaveScope(additional)
    res
  }
  private[typer] def withUnificationScope[T](block: => T): T = withUnificationScope(Nil)(block)


  /**
   * The current lexical region used for mutable variables.
   *
   * None on the toplevel
   */
  private var lexicalRegion: Option[CaptureParam] = None

  /**
   * The effects, whose declarations are _lexically_ in scope
   */
  private var lexicalEffects: List[Interface] = Nil

  private [typer] var capabilityScope: CapabilityScope = GlobalCapabilityScope


  private [typer] def bindingCapabilities[R](binder: source.Tree, caps: List[symbols.BlockParam])(f: => R): R = {
    bindCapabilities(binder, caps)
    capabilityScope = BindSome(binder, caps.map { c => c.tpe.asEffect -> c }.toMap, capabilityScope)
    val result = f
    capabilityScope = capabilityScope.parent
    result
  }

  private [typer] def bindCapabilities[R](binder: source.Tree, caps: List[symbols.BlockParam]): Unit =
    val capabilities = caps map { cap =>
      Typer.assertConcrete(cap.tpe.asEffect)
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
    Typer.assertConcrete(tpe)
    capabilityScope.capabilityFor(tpe)

  private [typer] def freshCapabilityFor(tpe: InterfaceType): symbols.BlockParam =
    val capName = tpe.name.rename(_ + "$capability")
    val param = BlockParam(capName, tpe)
    bind(param, tpe)
    param

  private [typer] def provideCapabilities(call: source.Call, effs: List[InterfaceType]): List[BlockParam] =
    val caps = effs.map(capabilityFor)
    annotations.update(Annotations.CapabilityArguments, call, caps)
    caps


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
   * - ...
   *
   * (2) Elaboration Info
   * --------------------
   * - [[Annotations.BoundCapabilities]]
   * - [[Annotations.CapabilityArguments]]
   *
   * (3) Inferred Information for LSP
   * --------------------------------
   * We first store the inferred types here, before substituting and committing to the
   * global DB, later.
   * - [[Annotations.InferredValueType]]
   * - [[Annotations.InferredBlockType]]
   * - [[Annotations.InferredEffect]]
   * - [[Annotations.BlockArgumentType]]
   * - [[Annotations.TypeArguments]]
   */
  private [typer] var annotations: Annotations = Annotations.empty

  //<editor-fold desc="(1) Typing Context">

  // first tries to find the type in the local typing context
  // if not found, it tries the global DB, since it might be a symbol of an already checked dependency
  private[typer] def lookup(s: ValueSymbol) =
    annotations.getOrElse(Annotations.ValueType, s, valueTypeOf(s))

  private[typer] def lookup(s: BlockSymbol) = (lookupBlockType(s), lookupCapture(s))

  private[typer] def lookupFunctionType(s: BlockSymbol): FunctionType =
    annotations.get(Annotations.BlockType, s)
     .map {
       case f: FunctionType => f
       case tpe => abort(s"Expected function type, but got ${tpe}.")
     }
     .orElse(functionTypeOption(s))
     .getOrElse(abort(s"Cannot find type for ${s.name.name} -- (mutually) recursive functions need to have an annotated return type."))

  private[typer] def lookupBlockType(s: BlockSymbol): BlockType =
    annotations.get(Annotations.BlockType, s).orElse(functionTypeOption(s)).getOrElse(abort(s"Cannot find type for ${s.name.name}."))

  private[typer] def lookupCapture(s: BlockSymbol) =
    annotations.get(Annotations.Captures, s).orElse(captureOfOption(s)).getOrElse {
      s match {
        case b: BlockParam => CaptureSet(b.capture)
        case _ => panic(s"Shouldn't happen: we do not have a capture for ${s}, yet.")
      }
    }

  private[typer] def getSelfRegion(tree: source.Tree): CaptureParam =
    annotation(Annotations.SelfRegion, tree)

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
      //        case (v: BlockSymbol, t: FunctionType) => bind(v, t)
      case other => panic(s"Internal Error: wrong combination of symbols and types: ${other}")
    }

  private[typer] def bind(p: ValueParam): Unit = p match {
    case s @ ValueParam(name, Some(tpe)) => bind(s, tpe)
    case s => panic(s"Internal Error: Cannot add $s to typing context.")
  }

  private[typer] def bind(p: BlockParam): Unit = p match {
    case s @ BlockParam(name, tpe) => bind(s, tpe, CaptureSet(p.capture))
  }

  //</editor-fold>

  //<editor-fold desc="(3) Inferred Information for LSP">

  private[typer] def annotateInferredType(t: Tree, e: ValueType) =
    annotations.update(Annotations.InferredValueType, t, e)

  private[typer] def annotateInferredType(t: Tree, e: BlockType) =
    annotations.update(Annotations.InferredBlockType, t, e)

  private[typer] def annotateInferredEffects(t: Tree, e: Effects) =
    annotations.update(Annotations.InferredEffect, t, e)

  // TODO also add InferredRegion
  //private[typer] def annotateInferredCapt(t: Tree, e: CaptureSet) = inferredCaptures = (t -> e) :: inferredCaptures

  private[typer] def annotateTypeArgs(call: source.Call, targs: List[symbols.ValueType]): Unit = {
    annotations.update(Annotations.TypeArguments, call, targs)
  }

//  private[typer] def annotateTarget(t: source.CallTarget, tpe: FunctionType): Unit = {
//    annotations.annotate(Annotations.TargetType, t, tpe)
//  }

  //</editor-fold>

  /**
   * Override the dynamically scoped `in` to also reset typer state
   */
  override def in[T](block: => T): T = {
    val effectsBefore = lexicalEffects
    val result = super.in(block)


    // TyperState has two kinds of components:
    // - reader-like (like lexicalEffects that are in scope)
    // - state-like (like annotations and unification constraints)
    //
    // The dynamic scoping of `in` should only affect the "reader" components of `typerState`, but
    // not the "state" components. For those, we manually perform backup and restore in typer.
    lexicalEffects = effectsBefore
    result
  }

  private[typer] def initTyperstate(effects: ConcreteEffects): Unit = {
    lexicalEffects = effects.toList.collect {
      case i: Interface => i
      case BlockTypeApp(i, _) => i
    }
    annotations = Annotations.empty
    capabilityScope = GlobalCapabilityScope
    unification.init()
  }

  private[typer] def backupTyperstate(): TyperState =
    TyperState(lexicalEffects, annotations.copy, unification.backup(), capabilityScope.copy)

  private[typer] def restoreTyperstate(st: TyperState): Unit = {
    lexicalEffects = st.lexicalEffects
    annotations = st.annotations.copy
    unification.restore(st.unification)
    capabilityScope = st.capabilityScope.copy
  }

  private[typer] def commitTypeAnnotations(): Unit = {
    val subst = unification.substitution

    // TODO since (in comparison to System C) we now have type directed overload resolution again,
    //   we need to make sure the typing context and all the annotations are backtrackable.
    //   This can be achieved by going back to local `annotations` which are easily backtrackable.
    //   In the end, we need to postprocess the annotations; see draft below...
    annotations.updateAndCommit(Annotations.ValueType) { case (t, tpe) => subst.substitute(tpe) }
    annotations.updateAndCommit(Annotations.BlockType) { case (t, tpe) =>
      val substituted = subst.substitute(tpe)
      println(s"${t.name.name}: $tpe ----> $substituted")
      substituted
    }
    annotations.updateAndCommit(Annotations.Captures) { case (t, capt) =>
      val substituted = subst.substitute(capt)
      println(s"${t.name.name} @ $substituted")
      substituted
    }

    // Update and write out all inferred types and captures for LSP support
    // This info is currently also used by Transformer!
    annotations.updateAndCommit(Annotations.InferredValueType) { case (t, tpe) => subst.substitute(tpe) }
    annotations.updateAndCommit(Annotations.InferredBlockType) { case (t, tpe) => subst.substitute(tpe) }
    annotations.updateAndCommit(Annotations.InferredEffect) { case (t, effs) => subst.substitute(effs) }

    annotations.updateAndCommit(Annotations.TypeArguments) { case (t, targs) => targs map subst.substitute }

    annotations.updateAndCommit(Annotations.BoundCapabilities) { case (t, caps) => caps }
    annotations.updateAndCommit(Annotations.CapabilityArguments) { case (t, caps) => caps }

    // TODO the state capability might still contain unification variables in its effect operations get and set
    //  however, creating a new state capability would also create a fresh block symbol, which might prove problematic
    annotations.updateAndCommit(Annotations.StateCapability) { case (t, state) => state }
  }


  // Lexical Regions
  // ===============
  def region: CaptureParam = lexicalRegion.getOrElse(abort("Mutable variables are not allowed outside of a function definition"))
  def withRegion[T](c: CaptureParam)(prog: => T): T = {
    val before = lexicalRegion
    lexicalRegion = Some(c)
    val res = prog
    lexicalRegion = before
    res
  }

  // Effects that are in the lexical scope
  // =====================================
  private[typer] def effectsInScope: List[Interface] = lexicalEffects

  private[typer] def withEffect(e: Interface): Context = {
    lexicalEffects = (e :: lexicalEffects).distinct
    this
  }

  // TODO extend check to also check in value types
  //   (now that we have first class functions, they could mention effects).
  private[typer] def wellscoped(effects: ConcreteEffects): Unit = {
    def checkInterface(eff: Interface): Unit =
      if (!(lexicalEffects contains eff)) error(s"Effect ${eff} leaves its defining scope.")

    def checkEffect(eff: Effect): Unit = eff match {
      case e: Interface => checkInterface(e)
      case BlockTypeApp(e, args) => checkInterface(e)
      case EffectAlias(n, params, effs) => effs.toList foreach checkEffect
      case b: BuiltinEffect => ()
    }

    effects.toList foreach checkEffect
  }
}
