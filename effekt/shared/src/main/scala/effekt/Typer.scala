package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{Annotation, Annotations, Context, ContextOps}
import effekt.context.assertions.*
import effekt.source.{AnyPattern, Def, Effectful, IgnorePattern, MatchGuard, MatchPattern, ModuleDecl, OpClause, Stmt, TagPattern, Term, Tree, resolve, symbol}
import effekt.source.Term.BlockLiteral
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
            flowingInto(builtins.toplevelCaptures) {
              // bring builtins into scope
              builtins.rootTerms.values.foreach {
                case term: BlockParam =>
                  Context.bind(term, term.tpe.getOrElse {
                    INTERNAL_ERROR("Builtins should always be annotated with their types.")
                  })
                  Context.bind(term, CaptureSet(term.capture))
                case term: ExternResource =>
                  Context.bind(term, term.tpe)
                  Context.bind(term, CaptureSet(term.capture))
                case term: Callable =>
                  Context.bind(term, term.toType)
                case term => Context.panic(s"Cannot bind builtin term: ${term}")
              }

              // We split the type-checking of definitions into "pre-check" and "check"
              // to allow mutually recursive defs
              tree.defs.foreach { d => precheckDef(d) }
              tree.defs.foreach { d =>
                val Result(_, effs) = synthDef(d)
                val unhandled = effs.toEffects
                if (unhandled.nonEmpty)
                  Context.at(d) {
                    Context.error(pretty"Unhandled effects ${unhandled}")
                  }
              }
            }
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


  //<editor-fold desc="expressions">

  def checkExpr(expr: Term, expected: Option[ValueType])(using Context, Captures): Result[ValueType] =
    checkAgainst(expr, expected) {
      case source.Literal(_, tpe)     => Result(tpe, Pure)

      case source.If(guards, thn, els) =>
        val Result((), guardEffs) = checkGuards(guards)
        val Result(thnTpe, thnEffs) = checkStmt(thn, expected)
        val Result(elsTpe, elsEffs) = checkStmt(els, expected)

        Result(Context.join(thnTpe, elsTpe), guardEffs ++ thnEffs ++ elsEffs)

      case source.While(guards, body, default) =>
        val Result((), guardEffs) = checkGuards(guards)
        val expectedType = if default.isDefined then expected else Some(TUnit)
        val Result(bodyTpe, bodyEffs) = checkStmt(body, expectedType)
        val Result(defaultTpe, defaultEffs) = default.map { s =>
          checkStmt(s, expectedType)
        }.getOrElse(Result(TUnit, ConcreteEffects.empty))

        Result(Context.join(bodyTpe, defaultTpe), defaultEffs ++ bodyEffs)

      case source.Var(id) => id.symbol match {
        case x: RefBinder => Context.lookup(x) match {
          case (btpe, capt) =>
            val vtpe = TState.extractType(btpe)
            usingCapture(capt)
            Result(vtpe, Pure)
        }
        case b: BlockSymbol => Context.abort("Expected an expression, but got a block.")
        case x: ValueSymbol => Result(Context.lookup(x), Pure)
      }

      case e @ source.Assign(id, expr) => e.definition match {
        case x: RefBinder =>
          val stTpe = Context.lookup(x) match {
            case (btpe, capt) =>
              usingCapture(capt)
              TState.extractType(btpe)
          }
          val Result(_, eff) = expr checkAgainst stTpe
          Result(TUnit, eff)
      }

      case l @ source.Box(annotatedCapture, block) =>

        val expectedTpe = expected.collect { case BoxedType(tpe, cap) => tpe }
        val inferredCap: Captures = annotatedCapture.map { _.resolve }.getOrElse {
          Context.freshCaptVar(CaptUnificationVar.InferredBox(l))
        }

        flowingInto(inferredCap) {
          val Result(inferredTpe, inferredEff) = checkExprAsBlock(block, expectedTpe)
          val tpe = Context.unification(BoxedType(inferredTpe, inferredCap))
          expected.map(Context.unification.apply) foreach { matchExpected(tpe, _) }
          Result(tpe, inferredEff)
        }

      case source.Unbox(_) =>
        Context.abort("Expected an expression, but got an unbox (which is a block).")

      case c @ source.Select(receiver, field) =>
        checkOverloadedFunctionCall(c, field, Nil, List(receiver), Nil, expected)

      case c @ source.Do(effect, op, targs, vargs, bargs) =>
        // (1) first check the call
        val Result(tpe, effs) = checkOverloadedFunctionCall(c, op, targs map { _.resolve }, vargs, bargs, expected)
        // (2) now we need to find a capability as the receiver of this effect operation
        // (2a) compute substitution for inferred type arguments
        val typeArgs = Context.annotatedTypeArgs(c)
        val operation = c.definition
        val subst = Substitutions.types(operation.tparams, typeArgs)

        // (2b) substitute into effect type of operation
        val effect = subst.substitute(operation.appliedInterface)
        // (2c) search capability
        val capability = Context.capabilityReceiver(c, effect)
        // (2d) register capability as being used
        usingCapture(CaptureSet(capability.capture))

        // (3) add effect to used effects
        Result(tpe, effs ++ ConcreteEffects(List(effect)))

      case c @ source.Call(t: source.IdTarget, targs, vargs, bargs) =>
        checkOverloadedFunctionCall(c, t.id, targs map { _.resolve }, vargs, bargs, expected)

      case c @ source.Call(source.ExprTarget(e), targs, vargs, bargs) =>
        val Result(tpe, funEffs) = checkExprAsBlock(e, None) match {
          case Result(b: FunctionType, capt) => Result(b, capt)
          case _ => Context.abort("Cannot infer function type for callee.")
        }

        val Result(t, eff) = checkCallTo(c, "function", tpe, targs map { _.resolve }, vargs, bargs, expected)
        Result(t, eff ++ funEffs)

      // precondition: PreTyper translates all uniform-function calls to `Call`.
      //   so the method calls here are actually methods calls on blocks as receivers.
      case c @ source.MethodCall(receiver, id, targs, vargs, bargs) =>
        checkOverloadedMethodCall(c, receiver, id, targs map { _.resolve }, vargs, bargs, expected)

      case tree @ source.Region(name, body) =>
        val reg = tree.symbol
        Context.bind(reg)

        val inferredCapture = Context.freshCaptVar(CaptUnificationVar.RegionRegion(tree))
        usingCapture(inferredCapture)

        flowingInto(Context.without(inferredCapture, List(reg.capture))) {
          checkStmt(body, expected)
        }

      case tree @ source.TryHandle(prog, handlers) =>

        // (1) extract all handled effects and capabilities
        val providedCapabilities: List[symbols.BlockParam] = handlers map Context.withFocus { h =>
          val effect: InterfaceType = h.effect.resolve
          val capability = h.capability.map { _.symbol }.getOrElse { Context.freshCapabilityFor(effect) }
          val tpe = capability.tpe.getOrElse { INTERNAL_ERROR("Block type annotation required") }
          Context.bind(capability, tpe, CaptureSet(capability.capture))
          capability
        }

        // Create a fresh capture variable for the continuations ?Ck
        val continuationCapt = Context.freshCaptVar(CaptUnificationVar.HandlerRegion(tree))

        // All used captures flow into the continuation capture, except the ones handled by this handler.
        val continuationCaptHandled = Context.without(continuationCapt, providedCapabilities.map(_.capture))

        // (2) Check the handled program
        val Result(ret, effs) = Context.bindingCapabilities(tree, providedCapabilities) {
          flowingInto(continuationCaptHandled) {
            checkStmt(prog, expected)
          }
        }

        // (3) Check the handlers
        // Also all capabilities used by the handler flow into the capture of the continuation

        var handlerEffs: ConcreteEffects = Pure

        handlers foreach Context.withFocus { h =>
          flowingInto(continuationCaptHandled) {
            val Result(_, usedEffects) = checkImplementation(h.impl, Some((ret, continuationCapt)))
            handlerEffs = handlerEffs ++ usedEffects
          }
        }

        // (4) Wellformedness checks
        val handlerFor = providedCapabilities.map { cap =>
          // all effects have to be concrete at this point in time
          // safety: (1) ensures there's a type annotation
          val concreteEffect = Context.unification(cap.tpe.getOrElse { INTERNAL_ERROR("Block type annotation required") }.asInterfaceType)
          (concreteEffect, cap)
        }

        // Helper definitions:
        // - capabilities that are bound explicitly by the user
        val explicitCapabilities = handlers.flatMap { _.capability.map(_.symbol) }.toSet
        // - all effects that are handled
        val handled = ConcreteEffects(handlerFor.map(_._1))
        // - capabilities grouped by effect
        val capabilityGroups = handlerFor.groupBy(_._1).view.mapValues(_.map(_._2)).toList

        // Compute groups of capabilities that handle the same effects, then
        //  1) check whether all are bound explicitly (which would be fine)
        //  2) it is only a singleton set (which is fine)
        capabilityGroups.foreach {
          case (effect, capabilities) if capabilities.size > 1 =>
            val allExplicit = capabilities.forall { c => explicitCapabilities contains c }

            if (!allExplicit)
              Context.warning(pp"There are multiple handlers for effect ${effect}; this might not be intended.\nMaybe bind capabilities explicitly (e.g. `try { ... } with c: MyEffect { ... }`)?")
          case _ => ()
        }
        capabilityGroups.foreach {
          case (effect, capabilities) =>
            val allImplicit = capabilities.forall { c => !(explicitCapabilities contains c) }
            val used = effs.exists(e => e == effect)

            if (allImplicit && !used)
              Context.warning(pp"Handling effect ${effect}, which seems not to be used by the program.")
        }

        // The captures of the handler continue flowing into the outer scope
        usingCapture(continuationCapt)

        Result(ret, (effs -- handled) ++ handlerEffs)

      case tree @ source.Match(sc, clauses, default) =>

        // (1) Check scrutinee
        // for example. tpe = List[Int]
        val Result(tpe, effs) = checkExpr(sc, None)

        var resEff = effs

        val tpes = clauses.map {
          case source.MatchClause(p, guards, body) =>
            // (3) infer types for pattern
            Context.bind(checkPattern(tpe, p))
            // infer types for guards
            val Result((), guardEffs) = checkGuards(guards)
            // check body of the clause
            val Result(clTpe, clEff) = Context in { checkStmt(body, expected) }

            resEff = resEff ++ clEff ++ guardEffs
            clTpe
        } ++ default.map { body =>
          val Result(defaultTpe, defaultEff) = Context in { checkStmt(body, expected) }
          resEff = resEff ++ defaultEff
          defaultTpe
        }

        // Clauses could in general be empty if there are no constructors
        // In that case the scrutinee couldn't have been constructed and
        // we can unify with everything.
        Result(Context.join(tpes: _*), resEff)

      case source.Hole(stmt) =>
        val Result(tpe, effs) = checkStmt(stmt, None)
        Result(expected.getOrElse(TBottom), Pure)

      case tree : source.New => Context.abort("Expected an expression, but got an object implementation (which is a block).")
      case tree : source.BlockLiteral => Context.abort("Expected an expression, but got a block literal.")
    }

  // Sideeffect: binds names in the current scope
  def checkGuards(guards: List[MatchGuard])(using Context, Captures): Result[Unit] =
    var effs = ConcreteEffects.empty
    guards foreach { g =>
      val Result(bindings, guardEffs) = checkGuard(g)
      Context.bind(bindings)
      effs = effs ++ guardEffs
    }
    Result((), effs)


  /**
   * The [[continuationDetails]] are only provided, if a continuation is captured (that is for implementations as part of effect handlers).
   */
  def checkImplementation(impl: source.Implementation, continuationDetails: Option[(ValueType, CaptUnificationVar)])(using Context, Captures): Result[InterfaceType] = Context.focusing(impl) {
    case source.Implementation(sig, clauses) =>

      var handlerEffects: ConcreteEffects = Pure

      // Extract interface and type arguments from annotated effect
      val tpe @ InterfaceType(constructor, targs) = sig.resolve
      val interface = constructor.asInterface // can only implement concrete interfaces

      // (3) check all operations are covered
      val covered = clauses.map { _.definition }
      val notCovered = interface.operations.toSet -- covered.toSet

      if (notCovered.nonEmpty) {
        val explanation = notCovered.map { op => pp"${op.name} of interface ${op.interface.name}" }.mkString(", ")
        Context.error(pretty"Missing definitions for operations: ${explanation}")
      }

      if (covered.size > covered.distinct.size)
        Context.error("Duplicate definitions of operations")

      clauses foreach Context.withFocus {
        case d @ source.OpClause(op, tparams, vparams, bparams, retAnnotation, body, resume) =>

          retAnnotation.foreach {
            case Effectful(otherTpe, otherEffs2) =>
              // if there is a return type annotation from the user, report an error
              // see PR #148 for more details
              // TODO: Can we somehow use the return type provided by the user?
              Context.abort(pretty"Unexpected type annotation on operation ${op}.")
          }

          val declaredOperation = interface.operations.find(o => o.name.name == op.name).getOrElse {
            Context.abort(pretty"Operation ${op.name} not defined in ${interface.name}.")
          }
          val declaredType = Context.lookupFunctionType(d.definition)

          def assertArity(kind: String, got: Int, expected: Int): Unit =
            if (got != expected)
              Context.abort(pretty"Number of ${kind} (${got}) does not match declaration of '${op.name}', which expects ${expected}.")

          // if we have zero given type parameters, we synthesize them -- no need to check then
          if (tparams.size != 0) assertArity("type parameters", tparams.size, declaredType.tparams.size - targs.size)
          assertArity("value parameters", vparams.size, declaredType.vparams.size)

          //     effect E[A, B, ...] { def op[C, D, ...]() = ... }  !--> op[A, B, ..., C, D, ...]
          // The parameters C, D, ... are existentials
          val existentialParams: List[TypeVar] = if (tparams.size == declaredType.tparams.size - targs.size) {
            tparams.map { tparam => tparam.symbol.asTypeParam }
          } else {
            // using the invariant that the universals are prepended to type parameters of the operation
            declaredType.tparams.drop(targs.size).map { tp =>
              // recreate "fresh" type variables
              val name = tp.name
              TypeVar.TypeParam(name)
            }
          }
          val existentials = existentialParams.map(ValueTypeRef.apply)

          Context.annotate(Annotations.TypeParameters, d, existentialParams)

          val canonicalEffects = declaredType.effects.canonical

          // distinguish between handler operation or object operation (which does not capture a cont.)
          val Result(_, effs) = continuationDetails match {
            // normal object: no continuation there
            case None =>
              // block parameters are to be bound by the definition itself instead of by resume when using handlers
              assertArity("block parameters", bparams.size, declaredType.bparams.size)

              val cparamsForBlocks = bparams.map { p => p.symbol.capture }
              // will be introduced as capabilities in a later phase
              val cparamsForEffects = canonicalEffects.map { tpe => CaptureParam(tpe.name) }
              val cparams = cparamsForBlocks ++ cparamsForEffects

              // substitute actual type parameter and capture parameters for declared ones
              val FunctionType(Nil, Nil, vps, bps, tpe, effs) =
                Context.instantiate(declaredType, targs ++ existentials, cparams.map(cap => CaptureSet(cap))) : @unchecked

              (vparams zip vps).foreach {
                case (param, declaredType) =>
                  val sym = param.symbol
                  val annotatedType = sym.tpe
                  annotatedType.foreach(matchDeclared(_, declaredType, param))
                  Context.bind(sym, annotatedType.getOrElse(declaredType))
              }

              (bparams zip bps zip cparamsForBlocks).foreach {
                case ((param, declaredType), capture) =>
                  val sym = param.symbol
                  val annotatedType = sym.tpe
                  annotatedType.foreach { matchDeclared(_, declaredType, param) }
                  Context.bind(sym, annotatedType.getOrElse(declaredType), CaptureSet(capture))
              }

              // these capabilities are later introduced as parameters in capability passing
              val capabilities = (effs.canonical zip cparamsForEffects).map {
                case (tpe, capt) => Context.freshCapabilityFor(tpe, CaptureSet(capt))
              }

              val Result(bodyType, bodyEffs) = Context.bindingCapabilities(d, capabilities) {
                body checkAgainst tpe
              }
              Result(bodyType, bodyEffs -- effs)

            // handler implementation: we have a continuation
            case Some(ret, continuationCapt) =>

              if (bparams.nonEmpty)
                Context.error("Block parameters are bound by resume and not the effect operation itself")

              def isBidirectional = canonicalEffects.nonEmpty || declaredType.bparams.nonEmpty

              val cparamsForBlocks = declaredOperation.bparams.map { p => CaptureParam(p.name) } // use the original name
              val cparamsForEffects = canonicalEffects.map { tpe => CaptureParam(tpe.name) } // use the type name
              val cparams = cparamsForBlocks ++ cparamsForEffects

              val FunctionType(Nil, Nil, vps, bps, tpe, effs) =
                Context.instantiate(declaredType, targs ++ existentials, cparams.map(cap => CaptureSet(cap))) : @unchecked

              (vparams zip vps).foreach {
                case (param, declaredType) =>
                  val sym = param.symbol
                  val annotatedType = sym.tpe
                  annotatedType.foreach(matchDeclared(_, declaredType, param))
                  Context.bind(sym, annotatedType.getOrElse(declaredType))
              }

              // (4) synthesize type of continuation
              val resumeType = if (isBidirectional) {
                // resume { {f} => e }
                val resumeType = FunctionType(Nil, cparams, Nil, bps, tpe, effs)
                val resumeCapt = CaptureParam(Name.local("resumeBlock"))
                FunctionType(Nil, List(resumeCapt), Nil, List(resumeType), ret, Effects.Pure)
              } else {
                // resume(v)
                FunctionType(Nil, Nil, List(tpe), Nil, ret, Effects.Pure)
              }
              Context.bind(Context.symbolOf(resume).asBlockSymbol, resumeType, continuationCapt)

              body checkAgainst ret
          }

          handlerEffects = handlerEffects ++ effs
      }

      // The implementation has the annotated block type
      Context.annotateInferredType(impl, tpe)

      Result(tpe, handlerEffects)
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
        Context.unification(vtpe) match {
          case BoxedType(btpe, capt2) =>
            usingCapture(capt2)
            Result(btpe, eff1)
          case _ =>
            Context.annotationOption(Annotations.UnboxParentDef, u) match {
              case Some(source.DefDef(id, annot, block)) =>
                // Since this `unbox` was synthesized by the compiler from `def foo = E`,
                // it's possible that the user simply doesn't know that they should have used the `val` keyword to specify a value
                // instead of using `def`; see [issue #130](https://github.com/effekt-lang/effekt/issues/130) for more details
                Context.abort(pretty"Expected the right-hand side of a `def` binding to be a block, but got a value of type $vtpe.\nMaybe try `val` if you're defining a value.")
              case _ =>
                Context.abort(pretty"Unbox requires a boxed type, but got $vtpe.")
            }
        }

      case source.Var(id) => id.symbol match {
        case b: BlockSymbol =>
          val (tpe, capt) = Context.lookup(b)
          expected.foreach(exp => matchExpected(tpe, exp))
          usingCapture(capt)
          Result(tpe, Pure)
        case e: ValueSymbol =>
          Context.abort(pretty"Expected a block variable, but ${id} is a value. Maybe use explicit syntax: { () => ${id} }")
      }

      case source.New(impl) => checkImplementation(impl, None)

      case s : source.MethodCall => sys error "Nested capability selection not yet supported"

      case arg: source.BlockLiteral => expected match {
        case Some(tpe: FunctionType) => checkFunctionArgument(arg, tpe)
        case Some(tpe) => Context.abort(pretty"Block literal has a function type, but expected type is: ${tpe}")
        case None => inferFunctionArgument(arg)
      }

      case other => Context.abort("Expected block, but got an expression.")
    }

  //</editor-fold>

  //<editor-fold desc="pattern matching">
  def checkPattern(sc: ValueType, pattern: MatchPattern)(using Context, Captures): Map[Symbol, ValueType] = Context.focusing(pattern) {
    case source.IgnorePattern()    => Map.empty
    case p @ source.AnyPattern(id) => Map(p.symbol -> sc)
    case p @ source.LiteralPattern(lit) =>
      Context.requireSubtype(sc, lit.tpe, ErrorContext.PatternMatch(p))
      Map.empty
    case p @ source.TagPattern(id, patterns) =>

      // symbol of the constructor we match against
      val sym: Constructor = p.definition

      val universals   = sym.tparams.take(sym.tpe.tparams.size)
      val existentials = sym.tparams.drop(sym.tpe.tparams.size)

      // create fresh unification variables
      val freshUniversals   = universals.map { t => Context.freshTypeVar(t, pattern) }
      // create fresh **bound** variables
      val freshExistentials = existentials.map { t => TypeVar.TypeParam(t.name) }

      Context.annotate(Annotations.TypeParameters, p, freshExistentials)

      val targs = (freshUniversals ++ freshExistentials).map { t => ValueTypeRef(t) }

      // (4) Compute blocktype of this constructor with rigid type vars
      // i.e. Cons : `(?t1, List[?t1]) => List[?t1]`
      val FunctionType(_, _, vps, _, ret, _) = Context.instantiate(sym.toType, targs, Nil)

      // (5) given a scrutinee of `List[Int]`, we learn `?t1 -> Int`
      matchPattern(sc, ret, p)

      // (6) check nested patterns
      var bindings = Map.empty[Symbol, ValueType]

      if (patterns.size != vps.size)
          Context.abort(s"Wrong number of pattern arguments, given ${patterns.size}, expected ${vps.size}.")

      (patterns zip vps) foreach {
        case (pat, par: ValueType) =>
          bindings ++= checkPattern(par, pat)
      }

      bindings
  } match { case res => Context.annotateInferredType(pattern, sc); res }

  def checkGuard(guard: MatchGuard)(using Context, Captures): Result[Map[Symbol, ValueType]] = guard match {
    case MatchGuard.BooleanGuard(condition) =>
      val Result(tpe, effs) = checkExpr(condition, Some(TBoolean))
      Result(Map.empty, effs)
    case MatchGuard.PatternGuard(scrutinee, pattern) =>
      val Result(tpe, effs) = checkExpr(scrutinee, None)
      Result(checkPattern(tpe, pattern), effs)
  }

  //</editor-fold>

  //<editor-fold desc="statements and definitions">

  def checkStmt(stmt: Stmt, expected: Option[ValueType])(using Context, Captures): Result[ValueType] =
    checkAgainst(stmt, expected) {
      // local mutable state
      case source.DefStmt(d @ source.VarDef(id, annot, binding), rest) =>
        val sym = d.symbol
        val stCapt = CaptureSet(sym.capture)

        val Result(tpeBind, effBind) = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding, None)
        }
        val stTpe = TState(tpeBind)

        Context in {
          Context.bind(sym, stTpe, stCapt)
          val inferredCapture = Context.freshCaptVar(CaptUnificationVar.VarRegion(d))
          usingCapture(inferredCapture)

          flowingInto(Context.without(inferredCapture, List(sym.capture))) {
            val Result(res, eff) = checkStmt(rest, expected)
            Result(res, eff ++ effBind)
          }
        }

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

      // (1) make up a fresh capture unification variable and annotate on function symbol
      val cap = Context.freshCaptVar(CaptUnificationVar.FunctionRegion(d))
      Context.bind(fun, cap)

      // (2) Store the annotated type (important for (mutually) recursive and out-of-order definitions)
      fun.annotatedType.foreach { tpe => Context.bind(fun, tpe) }

    case d @ source.DefDef(id, annot, source.New(source.Implementation(tpe, clauses))) =>
      val obj = d.symbol

      // (1) make up a fresh capture unification variable
      val cap = Context.freshCaptVar(CaptUnificationVar.BlockRegion(d))

      // (2) annotate capture variable and implemented blocktype
      Context.bind(obj, Context.resolvedType(tpe).asInterfaceType, cap)

    case d @ source.ExternDef(cap, id, tps, vps, bps, tpe, body) =>
      val fun = d.symbol

      Context.bind(fun, fun.toType, fun.capture)
      if (fun.effects.canonical.nonEmpty) {
        Context.abort("Unhandled control effects on extern defs not allowed")
      }

    case d @ source.ExternResource(id, tpe) =>
      Context.bind(d.symbol)

    case d @ source.InterfaceDef(id, tparams, ops) =>
      d.symbol.operations.foreach { op =>
        if (op.effects.toList contains op.appliedInterface) {
          Context.error("Bidirectional effects that mention the same effect recursively are not (yet) supported.")
        }

        val tpe = op.toType
        wellformed(tpe)
        Context.bind(op, tpe)
      }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { c =>
        val constructor = c.symbol
        Context.bind(constructor, constructor.toType, CaptureSet())
        constructor.fields.foreach { field =>
          val tpe = field.toType
          wellformed(tpe)
        }
      }

    case d @ source.RecordDef(id, tparams, fields) =>
      val constructor = d.symbol.constructor
      Context.bind(constructor, constructor.toType, CaptureSet())
      constructor.fields.foreach { field =>
        val tpe = field.toType
        wellformed(tpe)
        Context.bind(field, tpe, CaptureSet())
      }

    case d: source.TypeDef => wellformed(d.symbol.tpe)
    case d: source.EffectDef => wellformed(d.symbol.effs)

    case _ => ()
  }

  def synthDef(d: Def)(using Context, Captures): Result[Unit] = Context.at(d) {
    d match {
      case d @ source.FunDef(id, tps, vps, bps, ret, body) =>
        val sym = d.symbol
        // was assigned by precheck
        val functionCapture = Context.lookupCapture(sym)

        // We can also try to solve for the function capture, after checking the function.
        // Hence we provide it to `withUnificationScope`.
        val captVars = functionCapture match {
          case x: CaptUnificationVar => List(x)
          case _ => Nil
        }
        val Result(funTpe, unhandledEffects) = Context.withUnificationScope(captVars) {

          sym.vparams foreach Context.bind
          sym.bparams foreach Context.bind

          // to subtract the capabilities, which are only inferred bottom up, we need a **second** unification variable
          val inferredCapture = Context.freshCaptVar(CaptUnificationVar.FunctionRegion(d))

          flowingInto(inferredCapture) {

            (sym.annotatedType: @unchecked) match {
              case Some(annotated) =>
                // the declared effects are considered as bound
                val bound: ConcreteEffects = annotated.effects
                val capabilities = bound.canonical.map { tpe => Context.freshCapabilityFor(tpe) }
                val captures = capabilities.map { _.capture }

                // block parameters and capabilities for effects are assumed bound
                val Result(tpe, effs) = Context.bindingCapabilities(d, capabilities) {
                   Context in { body checkAgainst annotated.result }
                }
                Context.annotateInferredType(d, tpe)
                Context.annotateInferredEffects(d, effs.toEffects)

                // TODO also annotate the capabilities
                flowsIntoWithout(inferredCapture, functionCapture) {
                  annotated.cparams ++ captures
                }

                Result(annotated, effs -- bound)

              case None =>
                // all effects are handled by the function itself (since they are inferred)
                val (Result(tpe, effs), caps) = Context.bindingAllCapabilities(d) {
                  Context in { checkStmt(body, None) }
                }

                // We do no longer use the order annotated on the function, but always the canonical ordering.
                val capabilities = effs.canonical.map { caps.apply }
                val captures = capabilities.map(_.capture)

                Context.bindCapabilities(d, capabilities)
                Context.annotateInferredType(d, tpe)
                Context.annotateInferredEffects(d, effs.toEffects)

                // we subtract all capabilities introduced by this function to compute its capture
                flowsIntoWithout(inferredCapture, functionCapture) {
                  (sym.bparams ++ capabilities).map(_.capture)
                }

                // TODO also add capture parameters for inferred capabilities
                val funType = sym.toType(tpe, effs.toEffects, captures)
                Result(funType, Pure)
            }
          }
        }
        // we bind the function type outside of the unification scope to solve for variables.
        val substituted = Context.unification(funTpe)
        if (!isConcreteBlockType(substituted)) {
          Context.abort(pretty"Cannot fully infer type for ${id}: ${substituted}")
        }
        Context.bind(sym, substituted)

        Result((), unhandledEffects)

      case d @ source.ValDef(id, annot, binding) =>
        val Result(t, effBinding) = d.symbol.tpe match {
          case Some(t) =>
            val Result(_, eff) = binding checkAgainst t
            // use annotated, not inferred type
            Result(t, eff)
          case None => checkStmt(binding, None)
        }

        Context.bind(d.symbol, t)

        Result((), effBinding)

      // regions
      case d @ source.RegDef(id, annot, reg, binding) =>
        val sym = d.symbol
        // we use the current region as an approximation for the state
        val stCapt = Context.symbolOf(reg) match {
          case b: BlockSymbol =>
            Context.lookup(b) match {
              case (TRegion, capt) => capt
              case _               => Context.at(reg) { Context.abort("Expected a region.") }
            }
          case _ => Context.at(reg) { Context.abort("Expected a region.") }
        }

        // bind region as capture for the variable
        Context.bind(sym, stCapt)

        val Result(tpeBind, effBind) = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding, None)
        }
        val stTpe = TState(tpeBind)

        // to allocate into the region, it needs to be live...
        usingCapture(stCapt)

        Context.bind(sym, stTpe, stCapt)

        Result((), effBind)

      case d @ source.DefDef(id, annot, binding) =>
        given inferredCapture: CaptUnificationVar = Context.freshCaptVar(CaptUnificationVar.BlockRegion(d))

        // we require inferred Capture to be solved after checking this block.
        Context.withUnificationScope(List(inferredCapture)) {
          val Result(t, effBinding) = checkExprAsBlock(binding, d.symbol.tpe)
          Context.bind(d.symbol, t, inferredCapture)
          Result((), effBinding)
        }

      case d @ source.ExternDef(captures, id, tps, vps, bps, tpe, bodies) => Context.withUnificationScope {
        val sym = d.symbol
        sym.vparams foreach Context.bind
        sym.bparams foreach Context.bind

        flowingInto(Context.lookupCapture(sym)) {

          // Note: Externs are always annotated with a type
          val expectedReturnType = d.symbol.annotatedType.get.result
          bodies.foreach {
            case source.ExternBody.StringExternBody(ff, body) =>
              body.args.foreach { arg => checkExpr(arg, None) }
            case source.ExternBody.EffektExternBody(ff, body) =>
              checkStmt(body, Some(expectedReturnType))
            case u: source.ExternBody.Unsupported => u
          }

        }

        Result((), Pure)
      }

      // all other definitions have already been prechecked
      case d =>
        Result((), Pure)
    }
  }

  //</editor-fold>

  //<editor-fold desc="Function calls, arguments, and parameters">

  // Example.
  //   BlockParam: def foo { f: Int => String / Print }
  //   BlockArg: foo { n => println("hello" + n) }
  //     or
  //   BlockArg: foo { (n: Int) => println("hello" + n) }
  //
  // TODO For now we assume that handled effects can not show up in the return type of the block argument.
  def checkFunctionArgument(arg: source.BlockLiteral, expected: FunctionType)(using Context, Captures): Result[BlockType] = Context.focusing(arg) {

    case decl @ source.BlockLiteral(tparams, vparams, bparams, body) =>

      // (1) Apply what we already know.
      val bt @ FunctionType(tps, cps, vps, bps, tpe1, effs) = expected

      // (2) Check wellformedness
      if (tps.size != tparams.size)
        Context.abort(s"Wrong number of type arguments, given ${tparams.size}, but function expects ${tps.size}.")

      if (vps.size != vparams.size)
        Context.abort(s"Wrong number of value arguments, given ${vparams.size}, but function expects ${vps.size}.")

      if (bps.size != bparams.size)
        Context.abort(s"Wrong number of block arguments, given ${bparams.size}, but function expects ${bps.size}.")

      // (3) Substitute type parameters
      val typeParams = tparams.map { p => p.symbol.asTypeParam }
      val typeSubst = Substitutions.types(tps, typeParams.map { p => ValueTypeRef(p) })

      // (4) Check type annotations against declaration
      val valueTypes = (vparams zip vps) map {
        case (param, expected) =>
          val adjusted = typeSubst substitute expected
          // check given matches the expected, if given at all
          val tpe = param.symbol.tpe.map { got =>
            matchDeclared(got, adjusted, param);
            got
          } getOrElse { adjusted }
          // bind types to check body
          Context.bind(param.symbol, tpe)
          tpe
      }

      val blockTypes = (bparams zip bps) map {
        case (param, expTpe) =>
          val adjusted = typeSubst substitute expTpe
          val sym = param.symbol
          // check given matches the expected, if given at all
          val got = sym.tpe.map { got =>
            matchDeclared(got, adjusted, param)
            got
          } getOrElse { adjusted }
          // bind types to check body
          Context.bind(param.symbol, got, CaptureSet(sym.capture))
          got
      }

      // (4) Bind capabilities for all effects "handled" by this function
      val effects: ConcreteEffects = typeSubst substitute effs
      val capabilities = effects.canonical.map { tpe => Context.freshCapabilityFor(tpe) }

      // (5) Substitute capture params
      val captParams = (bparams.map(_.symbol) ++ capabilities).map { p => p.capture }
      val captSubst = Substitutions.captures(cps, captParams.map { p => CaptureSet(p) })

      // (6) Substitute both types and captures into expected return type
      val subst = typeSubst ++ captSubst

      val expectedReturn = subst substitute tpe1

      // (7) Check function body
      val bodyRegion = Context.freshCaptVar(CaptUnificationVar.AnonymousFunctionRegion(arg))

      val Result(bodyType, bodyEffs) = Context.bindingCapabilities(decl, capabilities) {
         flowingInto(bodyRegion) { body checkAgainst expectedReturn }
      }

      usingCaptureWithout(bodyRegion) { captParams }

      val tpe = FunctionType(typeParams, captParams, valueTypes, blockTypes, bodyType, effects.toEffects)

      Result(tpe, bodyEffs -- effects)
  }

  def inferFunctionArgument(arg: source.BlockLiteral)(using Context, Captures): Result[BlockType] = Context.focusing(arg) {
    case arg @ source.BlockLiteral(tparams, vparams, bparams, body) => Context in {
      val tps = tparams.map { p => p.symbol.asTypeParam }
      val vps = vparams.map { p =>
        val param = p.symbol
        val tpe = p.symbol.tpe.getOrElse {
          INTERNAL_ERROR("Expected type needs to be known for function arguments at the moment.")
        }
        Context.bind(param, tpe)
        tpe
      }
      val bps = bparams.map { p =>
        val param = p.symbol
        val tpe = param.tpe.getOrElse {
          INTERNAL_ERROR("Expected type need to be know for function arguments at the moment.")
        }
        Context.bind(param, tpe)
        tpe
      }

      // like with non-annotated function definitions, we need to use a separate unification variable to
      // subtract bound (but inferred) capabilities later.
      val inferredCapture = Context.freshCaptVar(CaptUnificationVar.AnonymousFunctionRegion(arg))
      val (Result(tpe, effs), caps) = Context.bindingAllCapabilities(arg) {
        flowingInto(inferredCapture) {
          Context in { checkStmt(body, None) }
        }
      }

      // The order of effects annotated to the function is the canonical ordering for capabilities
      val capabilities = effs.canonical.map { caps.apply }
      Context.bindCapabilities(arg, capabilities)

      val cps = (bparams.map(_.symbol) ++ capabilities).map(_.capture)

      val funType = FunctionType(tps, cps, vps, bps, tpe, effs.toEffects)

      // Like with functions, bound parameters and capabilities are not closed over
      usingCaptureWithout(inferredCapture) {
        (bparams.map(_.symbol) ++ capabilities).map(_.capture)
      }

      Result(funType, Pure)
    }
  }

  def findFunctionTypeFor(sym: BlockSymbol)(using Context): (FunctionType, Captures) = sym match {
    // capture of effect operations is dealt with by type checking Do or MethodCall
    case b: Operation => (Context.lookupFunctionType(b), CaptureSet.empty)
    case b: BlockSymbol => (Context.lookupFunctionType(b), Context.lookupCapture(b))
  }

  def attempt[T](f: => T)(using Context): Either[EffektMessages, (T, TyperState)] =
     val stateBefore = Context.backupTyperstate()
     try {
      Try {
        val result = f
        (result, Context.backupTyperstate())
      }
     } finally {
       Context.restoreTyperstate(stateBefore)
     }

  /**
   * We do not respect nested scoping on overload resolution for methods right now.
   *
   * Uniform function syntax has been desugared by [[PreTyper]]
   */
  def checkOverloadedMethodCall(
    call: source.CallLike,
    receiver: source.Term,
    id: source.IdRef,
    targs: List[ValueType],
    vargs: List[source.Term],
    bargs: List[source.Term],
    expected: Option[ValueType]
  )(using Context, Captures): Result[ValueType] = {
    val sym = id.symbol

    val methods = sym match {
      // an overloaded call target
      case CallTarget(syms) => syms.flatten.collect { case op: Operation => op }
      // already resolved by a previous attempt to typecheck
      case sym: Operation => List(sym)
      case s => Context.panic(s"Not a valid method: ${s} : ${s.getClass.getSimpleName}")
    }

    val Result(recvTpe, recvEffs) = checkExprAsBlock(receiver, None)

    val interface = recvTpe.asInterfaceType
    // filter out operations that do not fit the receiver
    val candidates = methods.filter(op => op.interface == interface.typeConstructor)

    val (successes, errors) = tryEach(candidates) { op =>
      val (funTpe, capture) = findFunctionTypeFor(op)

      // 1) check arity of explicitly provided type arguments

      if (targs.nonEmpty && targs.size != funTpe.tparams.size)
        Context.abort(s"Wrong number of type arguments, given ${targs.size} but expected ${funTpe.tparams.size}")

      // 2)
      // args present: check prefix against receiver
      (targs zip interface.args).foreach { case (manual, inferred) =>
        matchExpected(inferred, manual)
      }

      // args missing: synthesize args from receiver and unification variables (e.g. [Int, String, ?A, ?B])
      def fillInTypeArguments(tparams: List[TypeParam], interfaceArgs: List[ValueType]): List[ValueType] =
        (tparams, interfaceArgs) match {
          // we have an argument, provided by the interface: use it
          case (param :: params, arg :: args) => arg :: fillInTypeArguments(params, args)
          // we don't have an argument, create fresh unification variable
          case (param :: params, Nil) => ValueTypeRef(Context.freshTypeVar(param, call)) :: fillInTypeArguments(params, Nil)
          case (Nil, _) => Nil
        }

      val synthTargs = if targs.nonEmpty then targs else fillInTypeArguments(funTpe.tparams, interface.args)

      // TODO maybe type checking all calls should have a similar structure like above:
      //   1. make up and annotate unification variables, if no type arguments are there
      //   2. type check call with either existing or made up type arguments

      checkCallTo(call, op.name.name, funTpe, synthTargs, vargs, bargs, expected)
    }
    resolveOverload(id, List(successes), errors)
  }

  /**
   * Attempts to check a potentially overladed call, not reporting any errors but returning them instead.
   *
   * This is necessary for overload resolution by trying all alternatives.
   *   - if there is multiple without errors: Report ambiguity
   *   - if there is no without errors: report all possible solutions with corresponding errors
   */
  def checkOverloadedFunctionCall(
    call: source.CallLike,
    id: source.IdRef,
    targs: List[ValueType],
    vargs: List[source.Term],
    bargs: List[source.Term],
    expected: Option[ValueType]
  )(using Context, Captures): Result[ValueType] = {

    val scopes = id.symbol match {
      // an overloaded call target
      case CallTarget(syms) => syms
      // already resolved by a previous attempt to typecheck
      case sym: BlockSymbol => List(Set(sym))
      case id: ValueSymbol => Context.abort(pp"Cannot call value ${id}")
    }

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
    val results = scopes map { scope => tryEach(scope.toList) { receiver =>
      val (funTpe, capture) = findFunctionTypeFor(receiver)
      val Result(tpe, effs) = checkCallTo(call, receiver.name.name, funTpe, targs, vargs, bargs, expected)
      // This is different, compared to method calls:
      usingCapture(capture)
      Result(tpe, effs)
    }}

    val successes = results.map { scope => scope._1 }
    val errors = results.flatMap { scope => scope._2 }

    resolveOverload(id, successes, errors)
  }

  private def resolveOverload(
    id: source.IdRef,
    successes: List[List[(BlockSymbol, Result[ValueType], TyperState)]],
    failures: List[(BlockSymbol, EffektMessages)]
  )(using Context): Result[ValueType] = {

    successes foreachAborting {
      // continue in outer scope
      case Nil => ()

      // Exactly one successful result in the current scope
      case List((sym, tpe, st)) =>
        // use the typer state after this checking pass
        Context.restoreTyperstate(st)
        // reassign symbol of fun to resolved calltarget symbol
        Context.assignSymbol(id, sym)

        return tpe

      // Ambiguous reference
      case results =>
        val successfulOverloads = results.map { (sym, res, st) => (sym, findFunctionTypeFor(sym)._1) }
        Context.abort(AmbiguousOverloadError(successfulOverloads, Context.rangeOf(id)))
    }

    failures match {
      case Nil =>
        Context.abort("Cannot typecheck call.")

      // exactly one error
      case List((sym, errs)) =>
        Context.abortWith(errs)

      case failed =>
        // reraise all and abort
        val failures = failed.map { case (block, msgs) => (block, findFunctionTypeFor(block)._1, msgs) }
        Context.abort(FailedOverloadError(failures, Context.currentRange))
    }
  }

  def checkCallTo(
    call: source.CallLike,
    name: String,
    funTpe: FunctionType,
    targs: List[ValueType],
    vargs: List[source.Term],
    bargs: List[source.Term],
    expected: Option[ValueType]
  )(using Context, Captures): Result[ValueType] = {

    if (targs.nonEmpty && targs.size != funTpe.tparams.size)
      Context.abort(s"Wrong number of type arguments, given ${targs.size}, but ${name} expects ${funTpe.tparams.size}.")

    if (vargs.size != funTpe.vparams.size)
      Context.abort(s"Wrong number of value arguments, given ${vargs.size}, but ${name} expects ${funTpe.vparams.size}.")

    if (bargs.size != funTpe.bparams.size)
      Context.abort(s"Wrong number of block arguments, given ${bargs.size}, but ${name} expects ${funTpe.bparams.size}.")

    val callsite = currentCapture

    // (1) Instantiate blocktype
    // e.g. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
    val (typeArgs, captArgs, bt @ FunctionType(_, _, vps, bps, ret, retEffs)) = Context.instantiateFresh(funTpe)

    // provided type arguments flow into the fresh unification variables (i.e., Int <: ?A)
    if (targs.nonEmpty) (targs zip typeArgs).foreach { case (targ, tvar) => matchExpected(tvar, targ) }

    // (2) check return type
    expected.foreach { expected => matchExpected(ret, expected) }

    var effs: ConcreteEffects = Pure

    (vps zip vargs) foreach { case (tpe, expr) =>
      val Result(t, eff) = checkExpr(expr, Some(tpe))
      effs = effs ++ eff
    }

    // To improve inference, we first type check block arguments that DO NOT subtract effects,
    // since those need to be fully known.

    val (withoutEffects, withEffects) = (bps zip (bargs zip captArgs)).partitionMap {
      // TODO refine and check that eff.args refers to (inferred) type arguments of this application (`typeArgs`)
      case (tpe : FunctionType, rest) if tpe.effects.exists { eff => eff.args.nonEmpty } => Right((tpe, rest))
      case (tpe, rest) => Left((tpe, rest))
    }

    (withoutEffects ++ withEffects) foreach { case (tpe, (expr, capt)) =>
      flowsInto(capt, callsite)
      // capture of block <: ?C
      flowingInto(capt) {
        val Result(t, eff) = checkExprAsBlock(expr, Some(tpe))
        effs = effs ++ eff
      }
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
    val capabilities = Context.provideCapabilities(call, retEffs.canonical.map(Context.unification.apply))

    val captParams = captArgs.drop(bargs.size)
    (captParams zip capabilities) foreach { case (param, cap) =>
      flowsInto(CaptureSet(cap.capture), param)
    }
    usingCapture(CaptureSet(capabilities.map(_.capture)))

    Result(ret, effs)
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
    capabilityScope = BindSome(
      binder,
      caps.map { c => c.tpe.getOrElse { INTERNAL_ERROR("Capability type needs to be know.") }.asInterfaceType -> c }.toMap,
      capabilityScope
    )
    val result = f
    capabilityScope = capabilityScope.parent
    result
  }

  private [typer] def bindCapabilities[R](binder: source.Tree, caps: List[symbols.BlockParam]): Unit =
    val capabilities = caps map { cap =>
      assertConcrete(cap.tpe.getOrElse { INTERNAL_ERROR("Capability type needs to be know.") }.asInterfaceType)
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
    val param: BlockParam = BlockParam(tpe.name, Some(tpe))
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
    case s @ BlockParam(name, tpe) => bind(s, tpe.get, CaptureSet(p.capture))
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
