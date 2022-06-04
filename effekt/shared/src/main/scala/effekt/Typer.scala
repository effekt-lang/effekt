package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.context.assertions._
import effekt.regions.Region
import effekt.source.{ AnyPattern, Def, Expr, IgnorePattern, MatchPattern, ModuleDecl, Stmt, TagPattern, Tree }
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
case class TyperResult[+T](tpe: T, effects: Effects)
object / {
  def unapply[T](t: TyperResult[T]): Option[(T, Effects)] = Some((t.tpe, t.effects))
}
object TyperResult {
  extension [T](tpe: T) {
    def /(effects: Effects): TyperResult[T] = TyperResult(tpe, effects)
  }
}
import TyperResult._


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
          val (_ / effs) = synthDef(d)
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

  def checkExpr(expr: Expr, expected: Option[ValueType])(using Context): TyperResult[ValueType] =
    checkAgainst(expr, expected) {
      case source.IntLit(n)     => TInt / Pure
      case source.BooleanLit(n) => TBoolean / Pure
      case source.UnitLit()     => TUnit / Pure
      case source.DoubleLit(n)  => TDouble / Pure
      case source.StringLit(s)  => TString / Pure

      case source.If(cond, thn, els) =>
        val (cndTpe / cndEffs) = cond checkAgainst TBoolean
        val (thnTpe / thnEffs) = checkStmt(thn, expected)
        val (elsTpe / elsEffs) = els checkAgainst thnTpe

        thnTpe / (cndEffs ++ thnEffs ++ elsEffs)

      case source.While(cond, block) =>
        val (_ / condEffs) = cond checkAgainst TBoolean
        val (_ / blockEffs) = block checkAgainst TUnit
        TUnit / (condEffs ++ blockEffs)

      // the variable now can also be a block variable
      case source.Var(id) => id.symbol match {
        case b: BlockSymbol => Context.abort(s"Blocks cannot be used as expressions.")
        case e: ValueSymbol => Context.valueTypeOf(e) / Pure
      }

      case e @ source.Assign(id, expr) =>
        // assert that it is a mutable variable
        val sym = e.definition.asVarBinder
        val (_ / eff) = expr checkAgainst Context.valueTypeOf(sym)
        TUnit / eff

      // TODO share code with FunDef
      case l @ source.Lambda(id, params, body) =>
        val sym = l.symbol
        // currently expects params to be fully annotated
        Context.define(sym.params)

        expected match {
          case Some(exp @ FunType(BlockType(_, ps, ret, effs), reg)) =>
            checkAgainstDeclaration("lambda", ps, params)
            val (retGot / effsGot) = body checkAgainst ret
            Context.unify(ret, retGot)

            val diff = effsGot -- effs

            val reg = Region.fresh(l)
            val got = FunType(BlockType(Nil, ps, retGot, effs), reg)

            Context.unify(exp, got)

            Context.assignType(sym, sym.toType(retGot, effs))
            Context.assignType(l, ret)
            Context.assignEffect(l, effs)
            Context.annotateRegions(sym, reg)

            got / diff

          case _ =>
            val (ret / effs) = checkStmt(body, None)
            Context.wellscoped(effs)
            val ps = extractAllTypes(sym.params)
            val tpe = BlockType(Nil, ps, ret, effs)

            // we make up a fresh region variable that will be checked later by the region checker
            val reg = Region.fresh(l)
            val funTpe = FunType(tpe, reg)

            Context.assignType(sym, sym.toType(ret, effs))
            Context.assignType(l, ret)
            Context.assignEffect(l, effs)
            Context.annotateRegions(sym, reg)

            expected.foreach { exp =>
              Context.unify(exp, funTpe)
            }

            funTpe / Pure // all effects are handled by the function itself (since they are inferred)
        }

      case c @ source.Call(t: source.IdTarget, targs, args) => {
        checkOverloadedCall(c, t, targs map { _.resolve }, args, expected)
      }

      case c @ source.Call(source.ExprTarget(e), targs, args) =>
        val (funTpe / funEffs) = checkExpr(e, None)

        val tpe: BlockType = funTpe.dealias match {
          case f: FunType => f.tpe
          case _          => Context.abort(s"Expected function type, but got ${funTpe}")
        }
        val (t / eff) = checkCallTo(c, "function", tpe, targs map { _.resolve }, args, expected)
        t / (eff ++ funEffs)

      case c @ source.Call(source.MemberTarget(receiver, id), targs, args) =>
        Context.panic("Method call syntax not allowed in source programs.")

      case source.TryHandle(prog, handlers, suspend, resume, retrn) =>
        // effect EffOp(x: B): C
        // try { s1: T }
        // with EffOp { (x: B, resume: C => R) => s2: R }
        // on suspend { s3: A }
        // on resume { x: A => _ }
        // on return { x: T => s4: R }

        val (result / effs) = checkStmt(prog, expected)

        var effects: List[symbols.Effect] = Nil

        var handlerEffs = Pure

        // Check suspend, resume and return clause
        val (suspendTpe / suspendEffs) = suspend map {
          case source.OnSuspend(s) => checkStmt(s, None) 
        } getOrElse (TUnit / Pure)
        val (_ / resumeEffs) = resume map { 
          case source.OnResume(source.BlockArg(List(source.ValueParams(List(param @ source.ValueParam(paramId, paramTpe)))), body)) =>
            param.symbol.tpe foreach { Context.unify(_, suspendTpe) }
            Context.define(param.symbol, suspendTpe)
            checkStmt(body, Some(TUnit))
          case _ => 
            Context.panic(
              "Cannot occur. The parser should have already verified that there is only one parameter for 'on resume'."
            )
        } getOrElse (TUnit / Pure)
        val (ret / retEffs) = retrn map {
          case source.OnReturn(source.BlockArg(List(source.ValueParams(List(param @ source.ValueParam(paramId, paramTpe)))), body)) =>
            param.symbol.tpe foreach { Context.unify(_, result) }
            Context.define(param.symbol, result)
            checkStmt(body, None)
          case _ => 
            Context.panic(
              "Cannot occur. The parser should have already verified that there is only one parameter for 'on return'."
            )
        } getOrElse (result / Pure)
        handlerEffs = handlerEffs ++ suspendEffs ++ resumeEffs ++ retEffs

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
              val (rigids, BlockType(tparams, pms, tpe, effs)) = Unification.instantiate(Context.blockTypeOf(effectOp))

              // (2) unify with given type arguments for effect (i.e., A, B, ...):
              //     effect E[A, B, ...] { def op[C, D, ...]() = ... }  !--> op[A, B, ..., C, D, ...]
              //     The parameters C, D, ... are existentials
              val existentials: List[TypeVar] = rigids.drop(targs.size).map { r => TypeVar(r.name) }
              Context.addToUnifier(((rigids: List[TypeVar]) zip (targs ++ existentials)).toMap)

              // (3) substitute what we know so far
              val substPms = Context.unifier substitute pms
              val substTpe = Context.unifier substitute tpe
              val substEffs = Context.unifier substitute effectOp.otherEffects

              // (4) check parameters
              val ps = checkAgainstDeclaration(op.name, substPms, params)

              // (5) synthesize type of continuation
              val resumeType = if (effectOp.isBidirectional) {
                // resume { e }
                BlockType(Nil, List(List(BlockType(Nil, List(Nil), substTpe, substEffs))), ret, Pure)
              } else {
                // resume(v)
                BlockType(Nil, List(List(substTpe)), ret, Pure)
              }

              Context.define(ps).define(Context.symbolOf(resume), resumeType) in {
                val (_ / heffs) = body checkAgainst ret
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

        /*
        

        resulting type is either T or R if an on return clause is given
        
        pseudocode:

        (tryType, tryEffs) <- check(try.body)
        (_, handlerEffs) <- for each handler in handlers do check(handler)
        (suspendType, suspendEffs) <- check(suspend.body)
        annotate(resume.param, suspendType)
        (_, resumeEffs) <- check(resume.body)
        annotate(return.param, tryType)
        (returnType, returnEffs) <- check(return.body)
        tpe <-
          if exists on return then
            returnType
          else
            tryType
        (tpe, tryEffs ++ handlerEffs ++ suspendEffs ++ resumeEffs ++ returnEffs)
        */ 
        ret / ((effs -- Effects(effects)) ++ handlerEffs)

      case source.MatchExpr(sc, clauses) =>

        // (1) Check scrutinee
        // for example. tpe = List[Int]
        val (tpe / effs) = checkExpr(sc, None)

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
            val (clTpe / clEff) = Context.define(checkPattern(tpe, p)) in { checkStmt(body, expected) }

            // (4) unify clauses and collect effects
            Context.at(body) { Context.unify(resTpe, clTpe) }
            resEff = resEff ++ clEff

            // replace if type is more specific
            if (resTpe == THole) { resTpe = clTpe }
        }
        resTpe / resEff

      case source.Hole(stmt) =>
        val tpe / effs = checkStmt(stmt, None)
        expected.getOrElse(THole) / Pure
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
      val (rigids, BlockType(_, pms, ret, _)) = Unification.instantiate(sym.toType)

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
      val constructorParams = Context.unifier substitute pms

      // (8) check nested patterns
      var bindings = Map.empty[Symbol, ValueType]

      (List(patterns) zip constructorParams) foreach {
        case (pats, pars) =>
          if (pats.size != pars.size)
            Context.error(s"Wrong number of pattern arguments, given ${pats.size}, expected ${pars.size}.")

          (pats zip pars) foreach {
            case (pat, par: ValueType) =>
              bindings ++= checkPattern(par, pat)
            case _ =>
              Context.panic("Should not happen, since constructors can only take value parameters")
          }
      }
      bindings
  }

  //</editor-fold>

  //<editor-fold desc="statements and definitions">

  def checkStmt(stmt: Stmt, expected: Option[ValueType])(using Context): TyperResult[ValueType] =
    checkAgainst(stmt, expected) {
      case source.DefStmt(b, rest) =>
        val (t / effBinding) = Context in { precheckDef(b); synthDef(b) }
        val (r / effStmt) = checkStmt(rest, expected)
        r / (effBinding ++ effStmt)

      // <expr> ; <stmt>
      case source.ExprStmt(e, rest) =>
        val (_ / eff1) = checkExpr(e, None)
        val (r / eff2) = checkStmt(rest, expected)
        r / (eff1 ++ eff2)

      case source.Return(e)        => checkExpr(e, expected)

      case source.BlockStmt(stmts) => checkStmt(stmts, expected)
    }

  // not really checking, only if defs are fully annotated, we add them to the typeDB
  // this is necessary for mutually recursive definitions
  def precheckDef(d: Def)(using Context): Unit = Context.focusing(d) {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      d.symbol.annotatedType.foreach { tpe => Context.assignType(d.symbol, tpe) }

    case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
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

  def synthDef(d: Def)(using Context): TyperResult[Unit] = Context.at(d) {
    d match {
      case d @ source.FunDef(id, tparams, params, ret, body) =>
        val sym = d.symbol
        Context.define(sym.params)
        (sym.annotatedType: @unchecked) match {
          case Some(annotated) =>
            val (tpe / effs) = body checkAgainst annotated.result
            Context.wellscoped(effs)
            Context.assignType(d, tpe)
            Context.assignEffect(d, effs)

            () / (effs -- annotated.effects) // the declared effects are considered as bound
          case None =>
            val (tpe / effs) = checkStmt(body, None)
            Context.wellscoped(effs) // check they are in scope
            Context.assignType(sym, sym.toType(tpe, effs))
            Context.assignType(d, tpe)
            Context.assignEffect(d, effs)

            () / Pure // all effects are handled by the function itself (since they are inferred)
        }

      case d @ source.EffDef(id, tparams, ops) =>
        Context.withEffect(d.symbol)
        () / Pure

      case d @ source.ValDef(id, annot, binding) =>
        val (t / effBinding) = d.symbol.tpe match {
          case Some(t) =>
            binding checkAgainst t
          case None => checkStmt(binding, None)
        }
        Context.define(d.symbol, t)
        () / effBinding

      case d @ source.VarDef(id, annot, binding) =>
        val (t / effBinding) = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding, None)
        }
        Context.define(d.symbol, t)
        () / effBinding

      case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
        Context.define(d.symbol.params)
        () / Pure

      // all other defintions have already been prechecked
      case d =>
        () / Pure
    }
  }

  //</editor-fold>

  //<editor-fold desc="arguments and parameters">

  /**
   * Invariant: Only call this on declarations that are fully annotated
   */
  def extractAllTypes(params: Params)(using Context): Sections = params map extractTypes

  def extractTypes(params: List[Param])(using Context): List[Type] = params map {
    case BlockParam(_, tpe) => tpe
    case ValueParam(_, Some(tpe)) => tpe
    case _ => Context.panic("Cannot extract type")
  }

  /**
   * Returns the binders that will be introduced to check the corresponding body
   */
  def checkAgainstDeclaration(
    name: String,
    atCallee: List[List[Type]],
    // we ask for the source Params here, since it might not be annotated
    atCaller: List[source.ParamSection]
  )(using Context): Map[Symbol, Type] = {

    if (atCallee.size != atCaller.size)
      Context.error(s"Wrong number of argument sections, given ${atCaller.size}, but ${name} expects ${atCallee.size}.")

    (atCallee zip atCaller).flatMap[(Symbol, Type)] {
      case (List(b1: BlockType), b2: source.BlockParam) =>
        Context.at(b2) { Context.panic("Internal Compiler Error: Not yet supported") }

      case (ps1: List[ValueType @unchecked], source.ValueParams(ps2)) =>
        if (ps1.size != ps2.size)
          Context.error(s"Wrong number of arguments, given ${ps2.size}, but ${name} expects ${ps1.size}.")
        (ps1 zip ps2).map[(Symbol, Type)] {
          case (decl, p @ source.ValueParam(id, annot)) =>
            val annotType = annot.map(_.resolve)
            annotType.foreach { t =>
              Context.at(p) { Context.unify(decl, t) }
            }
            (p.symbol, annotType.getOrElse(decl)) // use the annotation, if present.
        }.toMap
      case (_, _) => Context.panic("Internal Compiler Error: cannot match arguments with the expected types")
    }.toMap
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
    args: List[source.ArgSection],
    expected: Option[Type]
  )(using Context): TyperResult[ValueType] = {

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
          val tpe = Context.blockTypeOption(sym).getOrElse {
            if (sym.isInstanceOf[ValueSymbol]) {
              Context.abort(s"Expected a function type.")
            } else {
              Context.abort(s"Cannot find type for ${sym.name} -- if it is a recursive definition try to annotate the return type.")
            }
          }
          val r = checkCallTo(call, sym.name.name, tpe, targs, args, expected)
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
            s"- ${sym.name} of type ${Context.blockTypeOf(sym)}"
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
    funTpe: BlockType,
    targs: List[ValueType],
    args: List[source.ArgSection],
    expected: Option[Type]
  )(using Context): TyperResult[ValueType] = {

    // (1) Instantiate blocktype
    // e.g. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
    val (rigids, bt @ BlockType(_, params, ret, retEffs)) = Unification.instantiate(funTpe)

    if (targs.nonEmpty && targs.size != rigids.size)
      Context.abort(s"Wrong number of type arguments ${targs.size}")

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

    if (params.size != args.size)
      Context.error(s"Wrong number of argument sections, given ${args.size}, but ${name} expects ${params.size}.")

    def checkArgumentSection(ps: List[Type], args: source.ArgSection): Unit = (ps, args) match {
      case (ps: List[Type], source.ValueArgs(as)) =>
        if (ps.size != as.size)
          Context.error(s"Wrong number of arguments. Argument section of ${name} requires ${ps.size}, but ${as.size} given.")

        // check that types are actually value types
        val vps = ps map {
          case tpe: ValueType => tpe
          case _ =>
            Context.error("Wrong argument type, expected a value argument")
            return
        }

        (vps zip as) foreach { case (tpe, expr) => checkValueArgument(tpe, expr) }

      case (List(bt: BlockType), arg: source.BlockArg) =>
        checkBlockArgument(bt, arg)

      case (_, _) =>
        Context.error("Wrong type of argument section")
    }

    def checkValueArgument(tpe: ValueType, arg: source.Expr): Unit = Context.at(arg) {
      val tpe1 = Context.unifier substitute tpe // apply what we already know.
      val (tpe2 / exprEffs) = arg checkAgainst tpe1

      // Update substitution with new information
      Context.unify(tpe1, tpe2)

      effs = effs ++ exprEffs
    }

    // Example.
    //   BlockParam: def foo { f: Int => String / Print }
    //   BlockArg: foo { n => println("hello" + n) }
    //     or
    //   BlockArg: foo { (n: Int) => println("hello" + n) }
    def checkBlockArgument(tpe: BlockType, arg: source.BlockArg): Unit = Context.at(arg) {
      val bt @ BlockType(Nil, params, tpe1, handled) = Context.unifier substitute tpe

      // Annotate the block argument with the substituted type, so we can use it later to introduce capabilities
      Context.annotateBlockArgument(arg, bt)

      Context.define {
        checkAgainstDeclaration("block", params, arg.params)
      }

      val (tpe2 / stmtEffs) = arg.body checkAgainst tpe1

      Context.unify(tpe1, tpe2)
      effs = (effs ++ (stmtEffs -- handled))
    }

    (params zip args) foreach { case (ps, as) => checkArgumentSection(ps, as) }

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
    substRet / substEff
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

  private def freeTypeVars(o: Any): Set[TypeVar] = o match {
    case t: symbols.TypeVar => Set(t)
    case BlockType(tparams, params, ret, effs) =>
      freeTypeVars(params) ++ freeTypeVars(ret) ++ freeTypeVars(effs) -- tparams.toSet
    case e: Effects            => freeTypeVars(e.toList)
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case _ =>
      Set.empty
  }

  extension (expr: Expr) {
    def checkAgainst(tpe: ValueType)(using Context): TyperResult[ValueType] =
      checkExpr(expr, Some(tpe))
  }

  extension (stmt: Stmt) {
    def checkAgainst(tpe: ValueType)(using Context): TyperResult[ValueType] =
      checkStmt(stmt, Some(tpe))
  }

  /**
   * Combinators that also store the computed type for a tree in the TypesDB
   */
  def checkAgainst[T <: Tree](t: T, expected: Option[Type])(f: T => TyperResult[ValueType])(using Context): TyperResult[ValueType] =
    Context.at(t) {
      val (got / effs) = f(t)
      wellformed(got)
      wellformed(effs)
      expected foreach { Context.unify(_, got) }
      Context.assignType(t, got)
      Context.assignEffect(t, effs)
      got / effs
    }
}

/**
 * Instances of this class represent an immutable backup of the typer state
 */
private[typer] case class TyperState(effects: Effects, annotations: Annotations, unifier: Unifier)

trait TyperOps extends ContextOps { self: Context =>

  /**
   * the effects, whose declarations are _lexically_ in scope
   */
  private var lexicalEffects: Effects = Pure

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
    annotations.commit()
    annotate(Annotations.Unifier, module, currentUnifier)
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
  private[typer] def assignType(t: Tree, e: ValueType): Context = {
    annotations.annotate(Annotations.InferredType, t, e)
    this
  }

  private[typer] def assignEffect(t: Tree, e: Effects): Context = {
    annotations.annotate(Annotations.InferredEffect, t, e)
    this
  }

  // this also needs to be backtrackable to interact correctly with overload resolution
  private[typer] def annotateBlockArgument(t: source.BlockArg, tpe: BlockType): Context = {
    annotations.annotate(Annotations.BlockArgumentType, t, tpe)
    this
  }

  private[typer] def annotateTypeArgs(call: source.Call, targs: List[symbols.ValueType]): Context = {
    annotations.annotate(Annotations.TypeArguments, call, targs)
    this
  }

  private[typer] def define(s: Symbol, t: ValueType): Context = {
    assignType(s, t); this
  }

  private[typer] def define(s: Symbol, t: InterfaceType): Context = {
    assignType(s, t); this
  }

  private[typer] def define(bs: Map[Symbol, Type]): Context = {
    bs foreach {
      case (v: ValueSymbol, t: ValueType) => define(v, t)
      case (v: BlockSymbol, t: BlockType) => define(v, t)
      case other => panic(s"Internal Error: wrong combination of symbols and types: ${other}")
    }; this
  }

  private[typer] def define(ps: List[List[Param]]): Context = {
    ps.flatten.foreach {
      case s @ ValueParam(name, Some(tpe)) => define(s, tpe)
      case s @ BlockParam(name, tpe) => define(s, tpe)
      case s => panic(s"Internal Error: Cannot add $s to context.")
    }
    this
  }

  private[typer] def annotateTarget(t: source.CallTarget, tpe: BlockType): Unit = {
    annotations.annotate(Annotations.TargetType, t, tpe)
  }

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
