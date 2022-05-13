package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.context.assertions._
import effekt.regions.Region
import effekt.source.{ AnyPattern, Def, Term, IgnorePattern, MatchPattern, ModuleDecl, Stmt, TagPattern, Tree }
import effekt.substitutions._
import effekt.symbols._
import effekt.symbols.builtins._
import effekt.symbols.kinds._
import effekt.util.messages.FatalPhaseError
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
 */
case class Result[+T](tpe: T, effects: Effects)

object Typer extends Phase[NameResolved, Typechecked] {

  val phaseName = "typer"

  def run(input: NameResolved)(using Context) = Context.using(module = input.mod, focus = input.tree) {
    try {
      val NameResolved(source, tree, mod) = input

      // Effects that are lexically in scope at the top level
      val toplevelEffects = mod.imports.foldLeft(mod.effects) { _ ++ _.effects }
      Context.initTyperstate(toplevelEffects)

      Context in {
        // We split the type-checking of definitions into "pre-check" and "check"
        // to allow mutually recursive defs
        tree.defs.foreach { d => precheckDef(d) }
        tree.defs.foreach { d =>
          val Result(_, effs) = synthDef(d)
          if (effs.nonEmpty)
            Context.at(d) {
              Context.error("Unhandled effects: " + effs)
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

  def checkExpr(expr: Term, expected: Option[ValueType])(using Context): Result[ValueType] =
    checkAgainst(expr, expected) {
      case source.IntLit(n)     => Result(TInt, Pure)
      case source.BooleanLit(n) => Result(TBoolean, Pure)
      case source.UnitLit()     => Result(TUnit, Pure)
      case source.DoubleLit(n)  => Result(TDouble, Pure)
      case source.StringLit(s)  => Result(TString, Pure)

      case source.If(cond, thn, els) =>
        val Result(cndTpe, cndEffs) = cond checkAgainst TBoolean
        val Result(thnTpe, thnEffs) = checkStmt(thn, expected)
        val Result(elsTpe, elsEffs) = els checkAgainst thnTpe

        Result(thnTpe, cndEffs ++ thnEffs ++ elsEffs)

      case source.While(cond, block) =>
        val Result(_, condEffs) = cond checkAgainst TBoolean
        val Result(_, blockEffs) = block checkAgainst TUnit
        Result(TUnit, condEffs ++ blockEffs)

      // the variable now can also be a block variable
      case source.Var(id) => id.symbol match {
        case b: BlockSymbol => Context.abort(s"Blocks cannot be used as expressions.")
        case e: ValueSymbol => Result(Context.lookup(e), Pure)
      }

      case e @ source.Assign(id, expr) =>
        // assert that it is a mutable variable
        val sym = e.definition.asVarBinder
        val Result(_, eff) = expr checkAgainst Context.lookup(sym)
        Result(TUnit, eff)

      // TODO share code with FunDef
      case l @ source.Box(block) =>
        val blockType = expected.map {
          case BoxedType(b, _) => b
          case b => Context.abort(s"Expected ${b} but got a first-class function")
        }
        val Result(inferredTpe, inferredEff) = checkBlockArgument(block, blockType)
        Result(BoxedType(inferredTpe, Region.empty), inferredEff)

      case c @ source.Call(t: source.IdTarget, targs, vargs, bargs) => {
        checkOverloadedCall(c, t, targs map { _.resolve }, vargs, bargs, expected)
      }

      case c @ source.Call(source.ExprTarget(e), targs, vargs, bargs) =>
        val Result(funTpe, funEffs) = checkExpr(e, None)

        val tpe: FunctionType = funTpe.dealias match {
          case BoxedType(f: FunctionType, _) => f
          case _          => Context.abort(s"Expected function type, but got ${funTpe}")
        }
        val Result(t, eff) = checkCallTo(c, "function", tpe, targs map { _.resolve }, vargs, bargs, expected)
        Result(t, eff ++ funEffs)

      case c @ source.Call(source.MemberTarget(receiver, id), targs, vargs, bargs) =>
        Context.panic("Method call syntax not allowed in source programs.")

      case source.TryHandle(prog, handlers) =>

        val Result(ret, effs) = checkStmt(prog, expected)

        var effects: List[symbols.Effect] = Nil

        var handlerEffs = Pure

        handlers foreach Context.withFocus { h =>
          val effect: Effect = h.effect.resolve

          if (effects contains effect) {
            Context.error(s"Effect ${effect} is handled twice.")
          } else {
            effects = effects :+ effect
          }

          val effectSymbol: ControlEffect = h.definition

          val tparams = effectSymbol.tparams
          val targs = h.effect.tparams.map(_.resolve)

          val covered = h.clauses.map { _.definition }
          val notCovered = effectSymbol.ops.toSet -- covered.toSet

          if (notCovered.nonEmpty) {
            val explanation = notCovered.map { op => s"${op.name} of effect ${op.effect.name}" }.mkString(", ")
            Context.error(s"Missing definitions for effect operations: ${explanation}")
          }

          if (covered.size > covered.distinct.size) {
            Context.error(s"Duplicate definitions of effect operations")
          }

          h.clauses foreach Context.withFocus {
            case d @ source.OpClause(op, params, body, resume) =>
              val effectOp = d.definition

              // (1) Instantiate block type of effect operation
              val (rigids, FunctionType(tparams, vps, Nil, tpe, effs)) = Unification.instantiate(Context.lookupFunctionType(effectOp))

              // (2) unify with given type arguments for effect (i.e., A, B, ...):
              //     effect E[A, B, ...] { def op[C, D, ...]() = ... }  !--> op[A, B, ..., C, D, ...]
              //     The parameters C, D, ... are existentials
              val existentials: List[TypeVar] = rigids.drop(targs.size).map { r => TypeVar(r.name) }
              Context.addToUnifier(((rigids: List[TypeVar]) zip (targs ++ existentials)).toMap)

              // (3) substitute what we know so far
              val substVps = vps map Context.unifier.substitute
              val substTpe = Context.unifier substitute tpe
              val substEffs = Context.unifier substitute effectOp.otherEffects

              // (4) check parameters
              if (substVps.size != params.size)
                Context.abort(s"Wrong number of value arguments, given ${params.size}, but ${op.name} expects ${substVps.size}.")

              (params zip substVps).foreach {
                case (param, decl) =>
                  val sym = param.symbol
                  val annotType = sym.tpe
                  annotType.foreach { t => Context.at(param) { Context.unify(decl, t) }}
                  Context.bind(sym, annotType.getOrElse(decl))
              }

              // (5) synthesize type of continuation
              val resumeType = if (effectOp.isBidirectional) {
                // resume { e }
                FunctionType(Nil, Nil, List(FunctionType(Nil, Nil, Nil, substTpe, substEffs)), ret, Pure)
              } else {
                // resume(v)
                FunctionType(Nil, List(substTpe), Nil, ret, Pure)
              }

              Context.bind(Context.symbolOf(resume), resumeType)
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

        val unusedEffects = Effects(effects) -- effs

        if (unusedEffects.nonEmpty)
          Context.warning("Handling effects that are not used: " + unusedEffects)

        Result(ret, (effs -- Effects(effects)) ++ handlerEffs)

      case source.Match(sc, clauses) =>

        // (1) Check scrutinee
        // for example. tpe = List[Int]
        val Result(tpe, effs) = checkExpr(sc, None)

        // (2) check exhaustivity
        checkExhaustivity(tpe, clauses.map { _.pattern })

        // Clauses could in general be empty if there are no constructors
        // In that case the scrutinee couldn't have been constructed and
        // we can unify with everything.
        var resTpe: ValueType = THole
        var resEff = effs

        clauses.foreach {
          case source.MatchClause(p, body) =>
            // (3) infer types for all clauses
            Context.bind(checkPattern(tpe, p))
            val Result(clTpe, clEff) = Context in { checkStmt(body, expected) }

            // (4) unify clauses and collect effects
            Context.at(body) { Context.unify(resTpe, clTpe) }
            resEff = resEff ++ clEff

            // replace if type is more specific
            if (resTpe == THole) { resTpe = clTpe }
        }
        Result(resTpe, resEff)

      case source.Hole(stmt) =>
        val Result(tpe, effs) = checkStmt(stmt, None)
        Result(expected.getOrElse(THole), Pure)
    }

  //</editor-fold>

  //<editor-fold desc="pattern matching">

  /**
   * This is a quick and dirty implementation of coverage checking. Both performance, and error reporting
   * can be improved a lot.
   */
  def checkExhaustivity(sc: ValueType, cls: List[MatchPattern])(using Context): Unit = {
    val catchall = cls.exists { p => p.isInstanceOf[AnyPattern] || p.isInstanceOf[IgnorePattern] }

    if (catchall)
      return ;

    sc match {
      case TypeConstructor(t: DataType) =>
        t.variants.foreach { variant =>
          checkExhaustivity(variant, cls)
        }

      case TypeConstructor(t: Record) =>
        val (related, unrelated) = cls.collect { case p: TagPattern => p }.partitionMap {
          case p if p.definition == t => Left(p.patterns)
          case p => Right(p)
        }

        if (related.isEmpty) {
          Context.error(s"Non exhaustive pattern matching, missing case for ${sc}")
        }

        (t.fields.map { f => f.tpe } zip related.transpose) foreach {
          case (t, ps) => checkExhaustivity(t, ps)
        }
      case other =>
        ()
    }
  }

  def checkPattern(sc: ValueType, pattern: MatchPattern)(using Context): Map[Symbol, ValueType] = Context.focusing(pattern) {
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
      val (rigids, FunctionType(_, vps, _, ret, _)) = Unification.instantiate(sym.toType)

      // (5) given a scrutinee of `List[Int]`, we learn `?t1 -> Int`
      Context.unify(ret, sc)

      // (6) check for existential type variables
      // at the moment we do not allow existential type parameters on constructors.
      val skolems = Context.skolems(rigids)
      if (skolems.nonEmpty) {
        Context.error(s"Unbound type variables in constructor ${id}: ${skolems.map(_.underlying).mkString(", ")}")
      }

      // (7) refine parameter types of constructor
      // i.e. `(Int, List[Int])`
      val constructorParams = vps map Context.unifier.substitute

      // (8) check nested patterns
      var bindings = Map.empty[Symbol, ValueType]

      if (patterns.size != constructorParams.size)
          Context.error(s"Wrong number of pattern arguments, given ${patterns.size}, expected ${constructorParams.size}.")

      (patterns zip constructorParams) foreach {
        case (pat, par: ValueType) =>
          bindings ++= checkPattern(par, pat)
      }

      bindings
  }

  //</editor-fold>

  //<editor-fold desc="statements and definitions">

  def checkStmt(stmt: Stmt, expected: Option[ValueType])(using Context): Result[ValueType] =
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

      case source.BlockStmt(stmts) => checkStmt(stmts, expected)
    }

  // not really checking, only if defs are fully annotated, we add them to the typeDB
  // this is necessary for mutually recursive definitions
  def precheckDef(d: Def)(using Context): Unit = Context.focusing(d) {
    case d @ source.FunDef(id, tps, vps, bps, ret, body) =>
      d.symbol.annotatedType.foreach { tpe => Context.assignType(d.symbol, tpe) }

    case d @ source.ExternFun(pure, id, tps, vps, bps, tpe, body) =>
      Context.assignType(d.symbol, d.symbol.toType)
      if (d.symbol.effects.controlEffects.nonEmpty) {
        Context.abort("Unhandled control effects on extern defs not allowed")
      }

    case d @ source.EffDef(id, tparams, ops) =>
      d.symbol.ops.foreach { op =>
        val tpe = op.toType
        wellformed(tpe)
        Context.assignType(op, tpe)
      }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.symbol
        Context.assignType(sym, sym.toType)

        sym.fields.foreach { field =>
          val tpe = field.toType
          wellformed(tpe)
          Context.assignType(field, tpe)
        }
      }

    case d @ source.RecordDef(id, tparams, fields) =>
      val rec = d.symbol
      Context.assignType(rec, rec.toType)
      rec.fields.foreach { field =>
        val tpe = field.toType
        wellformed(tpe)
        Context.assignType(field, tpe)
      }

    case d: source.TypeDef   => wellformed(d.symbol.tpe)
    case d: source.EffectDef => wellformed(d.symbol.effs)
    case _                   => ()
  }

  def synthDef(d: Def)(using Context): Result[Unit] = Context.at(d) {
    d match {
      case d @ source.FunDef(id, tps, vps, bps, ret, body) =>
        val sym = d.symbol
        sym.vparams foreach Context.bind
        sym.bparams foreach Context.bind
        (sym.annotatedType: @unchecked) match {
          case Some(annotated) =>
            val Result(tpe, effs) = body checkAgainst annotated.result
            Context.wellscoped(effs)
            Context.annotateInferredType(d, tpe)
            Context.annotateInferredEffects(d, effs)

            Result((), effs -- annotated.effects) // the declared effects are considered as bound
          case None =>
            val Result(tpe, effs) = checkStmt(body, None)
            Context.wellscoped(effs) // check they are in scope

            val funType = sym.toType(tpe, effs)
            Context.assignType(sym, funType)
            Context.annotateInferredType(d, tpe)
            Context.annotateInferredEffects(d, effs)

            Result((), Pure) // all effects are handled by the function itself (since they are inferred)
        }

      case d @ source.EffDef(id, tparams, ops) =>
        Context.withEffect(d.symbol)
        Result((), Pure)

      case d @ source.ValDef(id, annot, binding) =>
        val Result(t, effBinding) = d.symbol.tpe match {
          case Some(t) =>
            binding checkAgainst t
          case None => checkStmt(binding, None)
        }
        Context.bind(d.symbol, t)
        Result((), effBinding)

      case d @ source.VarDef(id, annot, binding) =>
        val Result(t, effBinding) = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding, None)
        }
        Context.bind(d.symbol, t)
        Result((), effBinding)

      case d @ source.ExternFun(pure, id, tps, vps, bps, tpe, body) =>
        d.symbol.vparams foreach Context.bind
        d.symbol.bparams foreach Context.bind
        Result((), Pure)

      // all other defintions have already been prechecked
      case d =>
        Result((), Pure)
    }
  }

  //</editor-fold>

  //<editor-fold desc="Function calls, arguments, and parameters">

  def checkBlockArgument(arg: source.BlockArg, expected: Option[BlockType])(implicit C: Context): Result[BlockType] =
    (arg, expected) match {
      case (arg: source.FunctionArg, Some(tpe: FunctionType)) =>
        checkFunctionArgument(arg, tpe)
      // if all parameters are annotated, that is good enough...
      case (arg@source.FunctionArg(tparams, vparams, bparams, body), _) =>
        val tps = tparams.map { p => p.symbol.asTypeVar }
        val vps = vparams.map { p => p.symbol.tpe }.map {
          case Some(tpe) => tpe
          case None => Context.abort("Expected type needs to be known for function arguments at the moment.")
        }
        val bps = bparams.map { p => p.symbol.tpe }
        val ret = RigidVar(TypeVar(Name.local("ReturnType")))
        val tpe = FunctionType(tps, vps, bps, ret, Pure)
        checkFunctionArgument(arg, tpe)
      case _ =>
        Context.abort("Can only type check function arguments, right now. Not capability arguments.")
    }

  // Example.
  //   BlockParam: def foo { f: Int => String / Print }
  //   BlockArg: foo { n => println("hello" + n) }
  //     or
  //   BlockArg: foo { (n: Int) => println("hello" + n) }
  def checkFunctionArgument(arg: source.FunctionArg, expected: FunctionType)(implicit C: Context): Result[FunctionType] = Context.focusing(arg) {
    case decl @ source.FunctionArg(tparams, vparams, bparams, body) =>

      // (1) Apply what we already know.
      val bt @ FunctionType(tps, vps, bps, tpe1, handled) = Context.unifier substitute expected

      // (2) Check wellformedness
      if (tps.size != tparams.size)
        Context.abort(s"Wrong number of type arguments, given ${tparams.size}, but function expects ${tps.size}.")

      if (vps.size != vparams.size)
        Context.abort(s"Wrong number of value arguments, given ${vparams.size}, but function expects ${vps.size}.")

      if (bps.size != bparams.size)
        Context.abort(s"Wrong number of block arguments, given ${bparams.size}, but function expects ${bps.size}.")

      // (3) Substitute type parameters
      val typeParams = tparams.map { p => p.symbol.asTypeVar }
      if (tps.nonEmpty) { Context.addToUnifier((tps zip typeParams).toMap) }

      // (4) Check type annotations against declaration
      val valueTypes = (vparams zip vps) map {
        case (param, exp) =>
          val adjusted = Context.unifier substitute exp
          val tpe = param.symbol.tpe.map { got =>
              Context.at(param) { Context.unify(adjusted, got) }
              got
          } getOrElse { adjusted }
          // bind types to check body
          Context.bind(param.symbol, tpe)
          tpe
      }
      val blockTypes = (bparams zip bps) map {
        case (param, exp) =>
          val adjusted = Context.unifier substitute exp
          val got = param.symbol.tpe
          Context.at(param) { Context.unify(adjusted, got) }
          // bind types to check body
          Context.bind(param.symbol, got)
          got
      }
      val adjustedReturn = Context.unifier substitute tpe1
      val Result(bodyType, bodyEffs) = body checkAgainst adjustedReturn

      val adjustedHandled = Context.unifier substitute handled
      val effs = bodyEffs -- adjustedHandled

      val tpe = FunctionType(typeParams, valueTypes, blockTypes, bodyType, adjustedHandled)

      // Annotate the block argument with the substituted type, so we can use it later to introduce capabilities
      Context.annotateBlockArgument(arg, tpe)

      Result(tpe, effs)
  }

  def findFunctionTypeFor(sym: TermSymbol)(using Context): FunctionType = sym match {
    case b: BlockSymbol => Context.lookupFunctionType(b)
    case v: ValueSymbol => Context.lookup(v) match {
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
    expected: Option[Type]
  )(using Context): Result[ValueType] = {

    val scopes = target.definition match {
      // an overloaded call target
      case CallTarget(name, syms) => syms
      // already resolved by a previous attempt to typecheck
      case sym                    => List(Set(sym))
    }

    // TODO improve: stop typechecking if one scope was successful

    val stateBefore = Context.backupTyperstate()

    // TODO try to avoid duplicate error messages
    val results = scopes map { scope =>
      scope.toList.map { sym =>
        sym -> Try {
          Context.restoreTyperstate(stateBefore)
          val tpe = findFunctionTypeFor(sym)
          val r = checkCallTo(call, sym.name.name, tpe, targs, vargs, bargs, expected)
          (r, Context.backupTyperstate())
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
    name: String,
    funTpe: FunctionType,
    targs: List[ValueType],
    vargs: List[source.Term],
    bargs: List[source.BlockArg],
    expected: Option[Type]
  )(using Context): Result[ValueType] = {

    // (1) Instantiate blocktype
    // e.g. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
    val (rigids, bt @ FunctionType(_, vps, bps, ret, retEffs)) = Unification.instantiate(funTpe)

    if (targs.nonEmpty && targs.size != rigids.size)
      Context.abort(s"Wrong number of type arguments ${targs.size}")

    if (vps.size != vargs.size)
      Context.error(s"Wrong number of value arguments, given ${vargs.size}, but ${name} expects ${vps.size}.")

    if (bps.size != bargs.size)
      Context.error(s"Wrong number of block arguments, given ${bargs.size}, but ${name} expects ${bps.size}.")

    // (2) Compute substitutions from provided type arguments (if any)
    if (targs.nonEmpty) {
      Context.addToUnifier(((rigids: List[TypeVar]) zip targs).toMap)
    }

    // (3) refine substitutions by matching return type against expected type
    expected.foreach { expectedReturn =>
      val refinedReturn = Context.unifier substitute ret
      Context.unify(expectedReturn, refinedReturn)
    }

    var effs = retEffs

    (vps zip vargs) foreach { case (tpe, expr) =>
      val tpe1 = Context.unifier substitute tpe // apply what we already know.
      val Result(_, eff) = checkExpr(expr, Some(tpe1))
      effs = effs ++ eff
    }

    (bps zip bargs) foreach { case (tpe, expr) =>
      val Result(_, eff) = checkBlockArgument(expr, Some(tpe))
      effs = effs ++ eff
    }

    //    println(
    //      s"""|Results of checking application of ${sym.name}
    //                  |    to args ${args}
    //                  |Substitution before checking arguments: $substBefore
    //                  |Substitution after checking arguments: $subst
    //                  |Rigids: $rigids
    //                  |Return type before substitution: $ret
    //                  |Return type after substitution: ${subst substitute ret}
    //                  |""".stripMargin
    //    )

    Context.checkFullyDefined(rigids)

    // annotate call node with inferred type arguments
    val inferredTypeArgs = rigids.map(Context.unifier.substitute)
    Context.annotateTypeArgs(call, inferredTypeArgs)

    // annotate the calltarget tree with the resolved blocktype
    Context.annotateTarget(call.target, Context.unifier.substitute(bt))

    val substRet = Context.unifier.substitute(ret)
    val substEff = Context.unifier.substitute(effs)
    Result(substRet, substEff)
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

  private def freeTypeVars(o: Any): Set[TypeVar] = o match {
    case t: symbols.TypeVar => Set(t)
    case FunctionType(tps, vps, bps, ret, effs) =>
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
    def checkAgainst(tpe: ValueType)(using Context): Result[ValueType] =
      checkExpr(expr, Some(tpe))
  }

  extension (stmt: Stmt) {
    def checkAgainst(tpe: ValueType)(using Context): Result[ValueType] =
      checkStmt(stmt, Some(tpe))
  }

  /**
   * Combinators that also store the computed type for a tree in the TypesDB
   */
  def checkAgainst[T <: Tree](t: T, expected: Option[Type])(f: T => Result[ValueType])(using Context): Result[ValueType] =
    Context.at(t) {
      val Result(got, effs) = f(t)
      wellformed(got)
      wellformed(effs)
      expected foreach { Context.unify(_, got) }
      Context.annotateInferredType(t, got)
      Context.annotateInferredEffects(t, effs)
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
      FunctionType(fun.tparams, fun.vparams.map { p => p.tpe.get }, fun.bparams.map { p => p.tpe }, result, effects)
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
private[typer] case class TyperState(effects: Effects, annotations: Annotations, unifier: Unifier)

trait TyperOps extends ContextOps { self: Context =>

  /**
   * The substitutions learnt so far
   */
  private var substitutions: Substitutions = Map.empty

  /**
   * The current lexical region used for mutable variables.
   *
   * None on the toplevel
   */
  private var lexicalRegion: Option[Region] = None

  /**
   * The effects, whose declarations are _lexically_ in scope
   */
  private var lexicalEffects: Effects = Pure


  // The "Typing Context"
  // ====================
  // since symbols are unique, we can use mutable state instead of reader

  //<editor-fold desc="Typing Context">

  private var valueTypingContext: Map[Symbol, ValueType] = Map.empty
  private var blockTypingContext: Map[Symbol, BlockType] = Map.empty
  private var regionContext: Map[Symbol, Region] = Map.empty

  // first tries to find the type in the local typing context
  // if not found, it tries the global DB, since it might be a symbol of an already checked dependency
  private[typer] def lookup(s: ValueSymbol) =
    valueTypingContext.getOrElse(s, valueTypeOf(s))

  private[typer] def lookup(s: BlockSymbol) = (lookupBlockType(s), lookupRegion(s))

  private[typer] def lookupFunctionType(s: BlockSymbol): FunctionType =
    blockTypingContext.get(s)
     .map {
       case f: FunctionType => f
       case tpe => abort(s"Expected function type, but got ${tpe}.")
     }
     .orElse(functionTypeOption(s))
     .getOrElse(abort(s"Cannot find type for ${s.name.name} -- (mutually) recursive functions need to have an annotated return type."))

  private[typer] def lookupBlockType(s: BlockSymbol): BlockType =
    blockTypingContext.get(s).orElse(functionTypeOption(s)).getOrElse(abort(s"Cannot find type for ${s.name.name}."))

  private[typer] def lookupRegion(s: BlockSymbol) =
    regionContext.getOrElse(s, regionOf(s))

  private[typer] def bind(s: Symbol, tpe: ValueType): Unit = valueTypingContext += (s -> tpe)

  private[typer] def bind(s: Symbol, tpe: BlockType, capt: Region): Unit = { bind(s, tpe); bind(s, capt) }

  private[typer] def bind(s: Symbol, tpe: BlockType): Unit = blockTypingContext += (s -> tpe)

  private[typer] def bind(s: Symbol, capt: Region): Unit = regionContext += (s -> capt)

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
    case s @ BlockParam(name, tpe) => bind(s, tpe, Region(s)) // bind(s, tpe, CaptureSet(CaptureOf(s)))
  }
  //</editor-fold>


  /**
   * Annotations added by typer
   *
   * The annotations are immutable and can be backtracked.
   */
  private var annotations: Annotations = Annotations.empty

  /**
   * Computed _unifier for type variables in this module
   */
  private var currentUnifier: Unifier = Unifier.empty

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

  private[typer] def initTyperstate(effects: Effects): Unit = {
    lexicalEffects = effects
    annotations = Annotations.empty
    currentUnifier = Unifier.empty
  }

  private[typer] def backupTyperstate(): TyperState =
    TyperState(lexicalEffects, annotations.copy, currentUnifier)

  private[typer] def restoreTyperstate(st: TyperState): Unit = {
    lexicalEffects = st.effects
    annotations = st.annotations.copy
    currentUnifier = st.unifier
  }

  private[typer] def commitTypeAnnotations(): Unit = {
    val subst = unifier.substitutions

    // now also store the typing context in the global database:
    valueTypingContext foreach { case (s, tpe) => assignType(s, subst.substitute(tpe)) }
    blockTypingContext foreach { case (s, tpe) => assignType(s, subst.substitute(tpe)) }
    //regionContext foreach { case (s, c) => assignCaptureSet(s, c) }

    // Update and write out all inferred types and captures for LSP support
    // This info is currently also used by Transformer!
    inferredValueTypes foreach { case (t, tpe) => annotate(Annotations.InferredValueType, t, subst.substitute(tpe)) }
    inferredBlockTypes foreach { case (t, tpe) => annotate(Annotations.InferredBlockType, t, subst.substitute(tpe)) }
    inferredEffects foreach { case (t, eff) => annotate(Annotations.InferredEffect, t, subst.substitute(eff)) }

//    val substitutedRegions = inferredRegions map { case (t, capt) => (t, capt.asRegionSet) }//(t, subst.substitute(capt)) }
    //inferredRegions foreach { case (t, capt) => annotate(Annotations.InferredRegion, t, capt.asInstanceOf[RegionSet]) }

    //annotate(Annotations.CaptureForFile, module, substitutedRegions)
    annotate(Annotations.Unifier, module, currentUnifier)
    annotations.commit()
  }

  // Effects that are in the lexical scope
  // =====================================
  private[typer] def effects: Effects = lexicalEffects

  private[typer] def withEffect(e: ControlEffect): Context = {
    lexicalEffects += e
    this
  }

  private[typer] def wellscoped(a: Effects): Unit = {
    // here we only care for the effect itself, not its type arguments
    val forbidden = Effects(a.controlEffects.toList.collect {
      case e: ControlEffect      => e
      case EffectApp(e, args) => e
      case e: EffectAlias     => e
    }) -- effects
    if (forbidden.nonEmpty) {
      error(s"Effects ${forbidden} leave their defining scope.")
    }
  }

  // Inferred types
  // ==============
  // We first store the inferred types here, before substituting and committing to DB, later.

  //<editor-fold desc="Inferred Types">

  private var inferredValueTypes: List[(Tree, ValueType)] = Nil
  private var inferredBlockTypes: List[(Tree, BlockType)] = Nil
  private var inferredEffects: List[(Tree, Effects)] = Nil
  private var inferredRegions: List[(Tree, Region)] = Nil


  private[typer] def annotateInferredType(t: Tree, e: ValueType) = inferredValueTypes = (t -> e) :: inferredValueTypes
  private[typer] def annotateInferredType(t: Tree, e: BlockType) = inferredBlockTypes = (t -> e) :: inferredBlockTypes
  private[typer] def annotateInferredEffects(t: Tree, e: Effects) = inferredEffects = (t -> e) :: inferredEffects
  //private[typer] def annotateInferredCapt(t: Tree, e: CaptureSet) = inferredCaptures = (t -> e) :: inferredCaptures


  // TODO also first store those annotations locally in typer, before substituting and committing to
  //  annotations DB.

  // this also needs to be backtrackable to interact correctly with overload resolution
  private[typer] def annotateBlockArgument(t: source.FunctionArg, tpe: FunctionType): Context = {
    annotations.annotate(Annotations.BlockArgumentType, t, tpe)
    this
  }

  private[typer] def annotateTypeArgs(call: source.Call, targs: List[symbols.ValueType]): Context = {
    annotations.annotate(Annotations.TypeArguments, call, targs)
    this
  }

  private[typer] def annotateTarget(t: source.CallTarget, tpe: FunctionType): Unit = {
    annotations.annotate(Annotations.TargetType, t, tpe)
  }

  //</editor-fold>

  // Unification
  // ===========
  private[typer] def unifier: Unifier = currentUnifier

  private[typer] def addToUnifier(map: Map[TypeVar, ValueType]): Unit =
    currentUnifier = currentUnifier.addAll(map)

  private[typer] def unify(tpe1: Type, tpe2: Type): Unit =
    currentUnifier = (currentUnifier union Unification.unify(tpe1, tpe2)).getUnifier

  private[typer] def skolems(rigids: List[RigidVar]): List[RigidVar] =
    currentUnifier.skolems(rigids)

  private[typer] def checkFullyDefined(rigids: List[RigidVar]): Unit =
    currentUnifier.checkFullyDefined(rigids).getUnifier
}
