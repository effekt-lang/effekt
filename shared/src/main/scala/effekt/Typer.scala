package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.context.assertions.{ SymbolAssertions }
import effekt.source.{ AnyPattern, Def, Expr, IgnorePattern, MatchPattern, Stmt, TagPattern, Tree }
import effekt.subtitutions._
import effekt.symbols._
import effekt.symbols.builtins._
import effekt.util.messages.FatalPhaseError
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages

// We add a dependency to driver to resolve types of symbols from other modules
/**
 * Output: the types we inferred for function like things are written into "types"
 *   - Blocks
 *   - Functions
 *   - Resumptions
 */

case class TyperState(
  /**
   * the effects, whose declarations are _lexically_ in scope
   */
  effects: Effects = Pure,

  /**
   * Annotations added by typer
   *
   * The annotations are immutable and can be backtracked.
   */
  annotations: Annotations = Annotations.empty
) {
  def deepCopy(): TyperState = TyperState(effects, annotations.copy)
}

class Typer extends Phase[Module, Module] { typer =>

  def run(mod: Module)(implicit C: Context): Option[Module] = try {

    val module = mod.decl

    // Effects that are lexically in scope at the top level
    val toplevelEffects = mod.imports.foldLeft(mod.effects) { _ ++ _.effects }
    Context.typerState = TyperState(toplevelEffects)

    Context in {
      // We split the type-checking of definitions into "pre-check" and "check"
      // to allow mutually recursive defs
      module.defs.foreach { d => precheckDef(d) }
      module.defs.foreach { d =>
        val (_ / effs) = synthDef(d)
        if (effs.nonEmpty)
          Context.at(d) {
            Context.error("Unhandled effects: " + effs)
          }
      }
    }

    if (C.buffer.hasErrors) {
      None
    } else {
      Some(mod)
    }
  } finally {
    // Store the backtrackable annotations into the global DB
    // This is done regardless of errors, since
    Context.typerState.annotations.commit()
  }

  //<editor-fold desc="expressions">

  def checkExpr(expr: Expr, expected: Option[ValueType])(implicit C: Context): Effectful =
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

      case v: source.Var =>
        Context.valueTypeOf(v.definition) / Pure

      case e @ source.Assign(id, expr) =>
        // assert that it is a mutable variable
        val sym = e.definition.asVarBinder
        val (_ / eff) = expr checkAgainst Context.valueTypeOf(sym)
        TUnit / eff

      case c @ source.Call(fun, targs, args) =>
        checkOverloadedCall(c, targs map { resolveValueType }, args, expected)

      case c @ source.MethodCall(b, fun, targs, args) =>
        Context.panic("Method call syntax not allowed in source programs.")

      case source.TryHandle(prog, handlers) =>

        val (ret / effs) = checkStmt(prog, expected)

        val effects = handlers.map { c => c.definition }
        var handlerEffs = Pure

        handlers.foreach { h =>
          val effect = h.definition
          val covered = h.clauses.map { _.definition }
          val notCovered = effect.ops.toSet -- covered.toSet

          if (notCovered.nonEmpty) {
            val explanation = notCovered.map { op => s"${op.name} of effect ${op.effect.name}" }.mkString(", ")
            Context.error(s"Missing definitions for effect operations: ${explanation}")
          }

          if (covered.size > covered.distinct.size) {
            Context.error(s"Duplicate definitions of effect operations")
          }

          h.clauses foreach {
            case d @ source.OpClause(op, params, body, resume) =>
              val effectOp = d.definition

              val BlockType(_, pms, tpe / effs) = Context.blockTypeOf(effectOp)
              val ps = checkAgainstDeclaration(op.name, pms, params)

              val resumeType = if (effectOp.isBidirectional) {
                // resume { e }
                BlockType(Nil, List(List(BlockType(Nil, List(Nil), tpe / effectOp.otherEffects))), ret / Pure)
              } else {
                // resume(v)
                BlockType(Nil, List(List(tpe)), ret / Pure)
              }

              Context.define(ps).define(Context.symbolOf(resume), resumeType) in {
                val (_ / heffs) = body checkAgainst ret
                handlerEffs = handlerEffs ++ heffs
              }
          }
        }

        val unusedEffects = Effects(effects) -- effs

        if (unusedEffects.nonEmpty)
          Context.warning("Handling effects that are not used: " + unusedEffects)

        ret / ((effs -- Effects(effects)) ++ handlerEffs)

      case source.MatchExpr(sc, clauses) =>

        // (1) Check scrutinee
        // for example. tpe = List[Int]
        val (tpe / effs) = checkExpr(sc, None)

        // (2) check exhaustivity
        checkExhaustivity(tpe, clauses.map { _.pattern })

        val tpes = clauses.map {
          case c @ source.MatchClause(p, body) =>
            Context.define(checkPattern(tpe, p)) in {
              checkStmt(body, expected)
            }
        }

        val (tpeCases / effsCases) = tpes.reduce[Effectful] {
          case (tpe1 / effs1, tpe2 / effs2) =>
            Substitution.unify(tpe1, tpe2)
            tpe1 / (effs1 ++ effs2)
        }
        tpeCases / (effsCases ++ effs)

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
  def checkExhaustivity(sc: ValueType, cls: List[MatchPattern])(implicit C: Context): Unit = {
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

  def checkPattern(sc: ValueType, pattern: MatchPattern)(implicit C: Context): Map[Symbol, ValueType] = Context.focusing(pattern) {
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
      val (rigids, BlockType(_, pms, ret / _)) = Substitution.instantiate(sym.toType)

      // (5) given a scrutinee of `List[Int]`, we learn `?t1 -> Int`
      val subst = Substitution.unify(ret, sc)

      // (6) check for existential type variables
      // at the moment we do not allow existential type parameters on constructors.
      if (subst.skolems(rigids).nonEmpty) {
        Context.error(s"Unbound type variables in constructor ${id}: ${subst.skolems(rigids).map(_.underlying).mkString(", ")}")
      }

      // (7) refine parameter types of constructor
      // i.e. `(Int, List[Int])`
      val constructorParams = subst substitute pms

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

  def checkStmt(stmt: Stmt, expected: Option[ValueType])(implicit C: Context): Effectful =
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
  def precheckDef(d: Def)(implicit C: Context): Unit = Context.focusing(d) {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      d.symbol.ret.foreach { annot => Context.assignType(d.symbol, d.symbol.toType) }

    case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
      Context.assignType(d.symbol, d.symbol.toType)
      if (d.symbol.effects.userDefined.nonEmpty) {
        Context.abort("User defined effects on extern defs not allowed")
      }

    case d @ source.EffDef(id, ops) =>
      d.symbol.ops.foreach { op => Context.assignType(op, op.toType) }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.symbol
        Context.assignType(sym, sym.toType)

        sym.fields.foreach { field =>
          Context.assignType(field, field.toType)
        }
      }

    case d @ source.RecordDef(id, tparams, fields) =>
      val rec = d.symbol
      Context.assignType(rec, rec.toType)
      rec.fields.foreach { field =>
        Context.assignType(field, field.toType)
      }

    case d => ()
  }

  def synthDef(d: Def)(implicit C: Context): Effectful = Context.at(d) {
    d match {
      case d @ source.FunDef(id, tparams, params, ret, body) =>
        val sym = d.symbol
        Context.define(sym.params)
        sym.ret match {
          case Some(tpe / funEffs) =>
            val (_ / effs) = body checkAgainst tpe
            Context.wellscoped(effs)
            Context.assignType(d, tpe / effs)

            tpe / (effs -- funEffs) // the declared effects are considered as bound
          case None =>
            val (tpe / effs) = checkStmt(body, None)
            Context.wellscoped(effs) // check they are in scope
            Context.assignType(sym, sym.toType(tpe / effs))
            Context.assignType(d, tpe / effs)

            tpe / Pure // all effects are handled by the function itself (since they are inferred)
        }

      case d @ source.EffDef(id, ops) =>
        Context.withEffect(d.symbol)
        TUnit / Pure

      case d @ source.ValDef(id, annot, binding) =>
        val (t / effBinding) = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding, None)
        }
        Context.define(d.symbol, t)
        t / effBinding

      case d @ source.VarDef(id, annot, binding) =>
        val (t / effBinding) = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding, None)
        }
        Context.define(d.symbol, t)
        t / effBinding

      case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
        Context.define(d.symbol.params)
        TUnit / Pure

      // all other defintions have already been prechecked
      case d => TUnit / Pure
    }
  }

  //</editor-fold>

  //<editor-fold desc="arguments and parameters">

  // TODO we can remove this duplication, once every phase can write to every table.
  // then the namer phase can already store the resolved type symbol for the param.

  def resolveValueType(tpe: source.ValueType)(implicit C: Context): ValueType = tpe match {
    case t @ source.TypeApp(id, args) => TypeApp(t.definition, args.map(resolveValueType))
    case t @ source.TypeVar(id)       => t.definition
    case source.ValueTypeTree(tpe)    => tpe
  }

  /**
   * Invariant: Only call this on declarations that are fully annotated
   */
  def extractAllTypes(params: Params)(implicit C: Context): Sections = params map extractTypes

  def extractTypes(params: List[Param])(implicit C: Context): List[Type] = params map {
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
  )(implicit C: Context): Map[Symbol, Type] = {

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
            val annotType = annot.map(resolveValueType)
            annotType.foreach { t =>
              Context.at(p) { Substitution.unify(decl, t) }
            }
            (p.symbol, annotType.getOrElse(decl)) // use the annotation, if present.
        }.toMap
    }.toMap
  }

  /**
   * Attempts to check the call to sym, not reporting any errors but returning them instead.
   *
   * This is necessary for overload resolution by trying all alternatives.
   *   - if there is multiple without errors: Report ambiguity
   *   - if there is no without errors: report all possible solutions with corresponding errors
   */
  def checkOverloadedCall(
    call: source.Call,
    targs: List[ValueType],
    args: List[source.ArgSection],
    expected: Option[Type]
  )(implicit C: Context): Effectful = {

    val scopes = call.definition match {
      // an overloaded call target
      case CallTarget(name, syms) => syms
      // already resolved by a previous attempt to typecheck
      case sym                    => List(Set(sym))
    }

    // TODO improve: stop typechecking if one scope was successful

    val stateBefore = C.typerState.deepCopy()

    val results = scopes map { scope =>
      scope.toList.map { sym =>
        sym -> Try {
          C.typerState = stateBefore.deepCopy()
          val r = checkCallTo(call, sym, targs, args, expected)
          (r, C.typerState.deepCopy())
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
        C.typerState = st
        // reassign symbol of fun to resolved calltarget
        C.assignSymbol(call.id, sym)

        return tpe

      // Ambiguous reference
      case results =>
        val sucMsgs = results.map {
          case (sym, tpe) =>
            s"- ${sym.name} of type ${Context.blockTypeOf(sym)}"
        }.mkString("\n")

        val explanation =
          s"""| Ambiguous reference to ${call.id}. The following blocks would typecheck:
              |
              |${sucMsgs}
              |""".stripMargin

        C.abort(explanation)
    }

    errors match {
      case Nil =>
        C.abort("Cannot typecheck call, no function found")

      // exactly one error
      case List((sym, errs)) =>
        val msg = errs.head
        val msgs = errs.tail
        C.buffer.append(msgs)
        // reraise and abort
        // TODO clean this up
        C.at(msg.value.asInstanceOf[Tree]) { C.abort(msg.label) }

      case failed =>
        // reraise all and abort
        val msgs = failed.flatMap {
          // TODO also print signature!
          case (block, msgs) => msgs.map { m => m.copy(label = s"Possible overload ${block.name.qualifiedName}: ${m.label}") }
        }.toVector

        C.reraise(msgs)

        C.abort(s"Cannot typecheck call. There are multiple overloads, which all fail to check.")
    }
  }

  def checkCallTo(
    call: source.Call,
    sym: BlockSymbol,
    targs: List[ValueType],
    args: List[source.ArgSection],
    expected: Option[Type]
  )(implicit C: Context): Effectful = {

    // (1) Instantiate blocktype
    // e.g. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
    val (rigids, BlockType(_, params, ret / retEffs)) = Substitution.instantiate(Context.blockTypeOption(sym).getOrElse {
      Context.abort(s"Cannot find type for ${sym.name} -- if it is a recursive definition try to annotate the return type.")
    })

    if (targs.nonEmpty && targs.size != rigids.size)
      Context.abort(s"Wrong number of type arguments ${targs.size}")

    // (2) Compute substitutions from provided type arguments (if any)
    var subst: Unifier = if (targs.nonEmpty) {
      Unifier(((rigids: List[TypeVar]) zip targs).toMap)
    } else {
      Unifier.empty
    }

    // (3) refine substitutions by matching return type against expected type
    expected.foreach { expectedReturn =>
      val refinedReturn = subst substitute ret
      subst = (subst union Substitution.unify(refinedReturn, expectedReturn)).getUnifier
    }

    var effs = retEffs

    if (params.size != args.size)
      Context.error(s"Wrong number of argument sections, given ${args.size}, but ${sym.name} expects ${params.size}.")

    def checkArgumentSection(ps: List[Type], args: source.ArgSection): Unit = (ps, args) match {
      case (ps: List[Type], source.ValueArgs(as)) =>
        if (ps.size != as.size)
          Context.error(s"Wrong number of arguments. Argument section of ${sym.name} requires ${ps.size}, but ${as.size} given.")

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
      val tpe1 = subst substitute tpe // apply what we already know.
      val (tpe2 / exprEffs) = arg checkAgainst tpe1

      // Update substitution with new information
      // TODO Trying to unify here yields the same type error as the previous line, again.
      // For now we have to live with this duplicated messages
      subst = (subst union Substitution.unify(tpe1, tpe2)).getUnifier

      effs = effs ++ exprEffs
    }

    // Example.
    //   BlockParam: def foo { f: Int => String / Print }
    //   BlockArg: foo { n => println("hello" + n) }
    //     or
    //   BlockArg: foo { (n: Int) => println("hello" + n) }
    def checkBlockArgument(tpe: BlockType, arg: source.BlockArg): Unit = Context.at(arg) {
      val BlockType(Nil, params, tpe1 / handled) = subst substitute tpe

      // TODO make blockargs also take multiple argument sections.
      Context.define {
        checkAgainstDeclaration("block", params, arg.params)
      }

      val (tpe2 / stmtEffs) = arg.body checkAgainst tpe1

      subst = (subst union Substitution.unify(tpe1, tpe2)).getUnifier
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

    subst.checkFullyDefined(rigids).getUnifier

    // annotate call node with inferred type arguments
    val inferredTypeArgs = rigids.map(subst.substitute)
    Context.typerState.annotations.annotate(Annotations.TypeArguments, call, inferredTypeArgs)

    (subst substitute ret) / effs
  }

  /**
   * Returns Left(Messages) if there are any errors
   *
   * In the case of nested calls, currently only the errors of the innermost failing call
   * are reported
   */
  private def Try[T](block: => T)(implicit C: Context): Either[Messages, T] = {
    import org.bitbucket.inkytonik.kiama.util.Severities.Error

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

  private implicit class ExprOps(expr: Expr) {
    def checkAgainst(tpe: ValueType)(implicit C: Context): Effectful =
      checkExpr(expr, Some(tpe))
  }

  private implicit class StmtOps(stmt: Stmt) {
    def checkAgainst(tpe: ValueType)(implicit C: Context): Effectful =
      checkStmt(stmt, Some(tpe))
  }

  /**
   * Combinators that also store the computed type for a tree in the TypesDB
   */
  def checkAgainst[T <: Tree](t: T, expected: Option[Type])(f: T => Effectful)(implicit C: Context): Effectful =
    Context.at(t) {
      val (got / effs) = f(t)
      expected foreach { Substitution.unify(_, got) }
      C.assignType(t, got / effs)
      got / effs
    }

  def check[T <: Tree](t: T)(f: T => Effectful)(implicit C: Context): Effectful =
    Context.at(t) {
      val (got / effs) = f(t)
      Context.assignType(t, got / effs)
      got / effs
    }
}

trait TyperOps extends ContextOps { self: Context =>

  /**
   * The state of the typer phase
   */
  private[typer] var typerState: TyperState = _

  /**
   * Override the dynamically scoped `in` to also reset typer state
   */
  override def in[T](block: => T): T = {
    val before = typerState
    val result = super.in(block)

    // TyperState has two kinds of components:
    // - reader-like (like effects that are in scope)
    // - state-like (like annotations and unification constraints)
    //
    // The dynamic scoping of `in` should only affect the "reader" components of `typerState`, but
    // not the "state" components. For those, we manually perform backup and restore in typer.
    typerState = if (before != null) {
      val annos = typerState.annotations
      // keep the annotations
      before.copy(annotations = annos)
    } else { before }

    result
  }

  // State Access
  // ============
  private[typer] def effects: Effects = typerState.effects

  private[typer] def withEffect(e: Effect): Context = {
    typerState = typerState.copy(effects = typerState.effects + e);
    this
  }

  private[typer] def assignType(t: Tree, e: Effectful): Context = {
    typerState.annotations.annotate(Annotations.TypeAndEffect, t, e)
    this
  }

  private[typer] def wellscoped(a: Effects): Unit = {
    val forbidden = a.userDefined -- effects
    if (forbidden.nonEmpty) {
      error(s"Effects ${forbidden} leave their defining scope.")
    }
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
}
