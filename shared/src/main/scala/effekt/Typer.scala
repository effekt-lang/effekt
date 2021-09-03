package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.context.assertions.SymbolAssertions
import effekt.source.{ AnyPattern, Def, Expr, IgnorePattern, MatchPattern, ModuleDecl, Stmt, TagPattern, Tree }
import effekt.substitutions._
import effekt.symbols._
import effekt.symbols.builtins._
import effekt.symbols.kinds._
import effekt.util.messages.FatalPhaseError
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages

/**
 * Output: the types we inferred for function like things are written into "types"
 *   - Blocks
 *   - Functions
 *   - Resumptions
 */
class Typer extends Phase[ModuleDecl, ModuleDecl] {

  val phaseName = "typer"

  def run(module: ModuleDecl)(implicit C: Context): Option[ModuleDecl] = try {
    val mod = Context.module

    Context.initTyperstate()

    Context in {
      // We split the type-checking of definitions into "pre-check" and "check"
      // to allow mutually recursive defs
      module.defs.foreach { d => precheckDef(d) }
      module.defs.foreach { d =>
        val _ = synthDef(d)
      }
    }

    if (C.buffer.hasErrors) {
      None
    } else {
      Some(module)
    }
  } finally {
    // Store the backtrackable annotations into the global DB
    // This is done regardless of errors, since
    Context.commitTypeAnnotations()

  }

  // checks an expression in second-class position
  //<editor-fold desc="blocks">

  /**
   * We defer checking whether something is first-class or second-class to Typer now.
   */
  def checkExprAsBlock(expr: Expr, expected: Option[BlockType])(implicit C: Context): BlockType =
    checkAgainstBlock(expr, expected) {
      case source.Var(id) => id.symbol match {
        case b: BlockSymbol => Context.blockTypeOf(b)
        case e: ValueSymbol => Context.abort(s"Currently expressions cannot be used as blocks.")
      }

      case source.Select(expr, selector) =>
        checkExprAsBlock(expr, None) match {
          case i: Interface =>
            // try to find an operation with name "selector"
            val op = i.ops.collect {
              case op if op.name.name == selector.name => op
            } match {
              case Nil      => Context.abort(s"Cannot select ${selector} in type ${i}")
              case List(op) => op
              case _        => Context.abort(s"Multi operations match ${selector} in type ${i}")
            }

            op.toType
          case _ => Context.abort(s"Selection requires an interface type.")
        }

      case _ => Context.abort(s"Expected something of a block type.")
    }

  //</editor-fold>

  //<editor-fold desc="expressions">

  def checkExpr(expr: Expr, expected: Option[ValueType])(implicit C: Context): ValueType =
    checkAgainst(expr, expected) {
      case source.IntLit(n)     => TInt
      case source.BooleanLit(n) => TBoolean
      case source.UnitLit()     => TUnit
      case source.DoubleLit(n)  => TDouble
      case source.StringLit(s)  => TString

      case source.If(cond, thn, els) =>
        val cndTpe = cond checkAgainst TBoolean
        val thnTpe = checkStmt(thn, expected)
        val elsTpe = els checkAgainst thnTpe

        thnTpe

      case source.While(cond, block) =>
        val _ = cond checkAgainst TBoolean
        val _ = block checkAgainst TUnit
        TUnit

      // the variable now can also be a block variable
      case source.Var(id) => id.symbol match {
        case b: BlockSymbol => Context.abort(s"Blocks cannot be used as expressions.")
        case e: ValueSymbol => Context.valueTypeOf(e)
      }

      case e @ source.Assign(id, expr) =>
        // assert that it is a mutable variable
        val sym = e.definition.asVarBinder
        val _ = expr checkAgainst Context.valueTypeOf(sym)
        TUnit

      case c @ source.Call(e, targs, args) =>
        val btpe = checkExprAsBlock(e, None) match {
          case b: FunctionType => b
          case _               => Context.abort("Callee is required to have function type")
        }
        checkCallTo(c, "???", btpe, targs map { _.resolve }, args, expected)

      case source.TryHandle(prog, handlers) =>

        // (1) assign types to capabilities
        val capabilities = handlers.map { h => h.capability.symbol }
        Context.define(List(capabilities))

        // (2) check body
        val ret = checkStmt(prog, expected)

        // TODO implement checking of handlers

        ret
      //      case source.TryHandle(prog, handlers) =>
      //
      //        val ret = checkStmt(prog, expected)
      //
      //        var effects: List[symbols.Effect] = Nil
      //
      //        var handlerEffs = Pure
      //
      //        handlers foreach Context.withFocus { h =>
      //          val effect: Effect = h.effect.resolve
      //
      //          if (effects contains effect) {
      //            Context.error(s"Effect ${effect} is handled twice.")
      //          } else {
      //            effects = effects :+ effect
      //          }
      //
      //          val effectSymbol: UserEffect = h.definition
      //
      //          val tparams = effectSymbol.tparams
      //          val targs = h.effect.tparams.map(_.resolve)
      //
      //          val covered = h.clauses.map { _.definition }
      //          val notCovered = effectSymbol.ops.toSet -- covered.toSet
      //
      //          if (notCovered.nonEmpty) {
      //            val explanation = notCovered.map { op => s"${op.name} of effect ${op.effect.name}" }.mkString(", ")
      //            Context.error(s"Missing definitions for effect operations: ${explanation}")
      //          }
      //
      //          if (covered.size > covered.distinct.size) {
      //            Context.error(s"Duplicate definitions of effect operations")
      //          }
      //
      //          h.clauses foreach Context.withFocus {
      //            case d @ source.OpClause(op, params, body, resume) =>
      //              val effectOp = d.definition
      //
      //              // (1) Instantiate block type of effect operation
      //              val (rigids, BlockType(tparams, pms, tpe / effs)) = Unification.instantiate(Context.blockTypeOf(effectOp))
      //
      //              // (2) unify with given type arguments for effect (i.e., A, B, ...):
      //              //     effect E[A, B, ...] { def op[C, D, ...]() = ... }  !--> op[A, B, ..., C, D, ...]
      //              //     The parameters C, D, ... are existentials
      //              val existentials: List[TypeVar] = rigids.drop(targs.size).map { r => TypeVar(r.name) }
      //              Context.addToUnifier(((rigids: List[TypeVar]) zip (targs ++ existentials)).toMap)
      //
      //              // (3) substitute what we know so far
      //              val substPms = Context.unifier substitute pms
      //              val substTpe = Context.unifier substitute tpe
      //              val substEffs = Context.unifier substitute effectOp.otherEffects
      //
      //              // (4) check parameters
      //              val ps = checkAgainstDeclaration(op.name, substPms, params)
      //
      //              // (5) synthesize type of continuation
      //              val resumeType = if (effectOp.isBidirectional) {
      //                // resume { e }
      //                BlockType(Nil, List(List(BlockType(Nil, List(Nil), substTpe / substEffs))), ret / Pure)
      //              } else {
      //                // resume(v)
      //                BlockType(Nil, List(List(substTpe)), ret / Pure)
      //              }
      //
      //              Context.define(ps).define(Context.symbolOf(resume), resumeType) in {
      //                val (_ / heffs) = body checkAgainst ret
      //                handlerEffs = handlerEffs ++ heffs
      //
      //                val typesInEffects = freeTypeVars(heffs)
      //                existentials.foreach { t =>
      //                  if (typesInEffects.contains(t)) {
      //                    Context.error(s"Type variable ${t} escapes its scope as part of the effect types: $heffs")
      //                  }
      //                }
      //              }
      //          }
      //        }
      //
      //        val unusedEffects = Effects(effects) -- effs
      //
      //        if (unusedEffects.nonEmpty)
      //          Context.warning("Handling effects that are not used: " + unusedEffects)
      //
      //        ret / ((effs -- Effects(effects)) ++ handlerEffs)

      case source.MatchExpr(sc, clauses) =>

        // (1) Check scrutinee
        // for example. tpe = List[Int]
        val tpe = checkExpr(sc, None)

        // (2) check exhaustivity
        checkExhaustivity(tpe, clauses.map { _.pattern })

        // (3) infer types for all clauses
        val (fstTpe, _) :: tpes = clauses.map {
          case c @ source.MatchClause(p, body) =>
            Context.define(checkPattern(tpe, p)) in {
              (checkStmt(body, expected), body)
            }
        }

        // (4) unify clauses and collect effects
        val tpeCases = tpes.foldLeft(fstTpe) {
          case (expected, (clauseTpe, tree)) =>
            Context.at(tree) { Context.unify(expected, clauseTpe) }
            expected
        }
        tpeCases

      case source.Select(expr, selector) =>
        Context.abort("Block in expression position: automatic boxing currently not supported.")

      case source.Hole(stmt) =>
        val tpe = checkStmt(stmt, None)
        expected.getOrElse(THole)
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
      val (rigids, FunctionType(_, pms, ret)) = Unification.instantiate(sym.toType)

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

  def checkStmt(stmt: Stmt, expected: Option[ValueType])(implicit C: Context): ValueType =
    checkAgainst(stmt, expected) {
      case source.DefStmt(b, rest) =>
        val t = Context in { precheckDef(b); synthDef(b) }
        val r = checkStmt(rest, expected)
        r

      // <expr> ; <stmt>
      case source.ExprStmt(e, rest) =>
        val _ = checkExpr(e, None)
        val r = checkStmt(rest, expected)
        r

      case source.Return(e)        => checkExpr(e, expected)

      case source.BlockStmt(stmts) => checkStmt(stmts, expected)
    }

  // not really checking, only if defs are fully annotated, we add them to the typeDB
  // this is necessary for mutually recursive definitions
  def precheckDef(d: Def)(implicit C: Context): Unit = Context.focusing(d) {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      d.symbol.ret.foreach { annot =>
        Context.assignType(d.symbol, d.symbol.toType)
      }

    case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
      Context.assignType(d.symbol, d.symbol.toType)

    case d @ source.InterfaceDef(id, tparams, ops) =>
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

    case _ => ()
  }

  def synthDef(d: Def)(implicit C: Context): ValueType = Context.at(d) {
    d match {
      case d @ source.FunDef(id, tparams, params, ret, body) =>
        val sym = d.symbol
        Context.define(sym.params)
        sym.ret match {
          case Some(tpe) =>
            val _ = body checkAgainst tpe
            Context.assignType(d, tpe)
            tpe
          case None =>
            val tpe = checkStmt(body, None)
            Context.assignType(sym, sym.toType(tpe))
            Context.assignType(d, tpe)
            tpe
        }

      //      case d @ source.EffDef(id, tparams, ops) =>
      //        Context.withEffect(d.symbol)
      //        TUnit / Pure

      case d @ source.ValDef(id, annot, binding) =>
        val t = d.symbol.tpe match {
          case Some(t) =>
            binding checkAgainst t
          case None => checkStmt(binding, None)
        }
        Context.define(d.symbol, t)
        t

      case d @ source.VarDef(id, annot, binding) =>
        val t = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding, None)
        }
        Context.define(d.symbol, t)
        t

      case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
        Context.define(d.symbol.params)
        TUnit

      // all other defintions have already been prechecked
      case d => TUnit
    }
  }

  //</editor-fold>

  //<editor-fold desc="arguments and parameters">

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
      case (List(b1: FunctionType), b2: source.BlockParam) =>
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
    }.toMap
  }

  //  /**
  //   * Attempts to check a potentially overladed call, not reporting any errors but returning them instead.
  //   *
  //   * This is necessary for overload resolution by trying all alternatives.
  //   *   - if there is multiple without errors: Report ambiguity
  //   *   - if there is no without errors: report all possible solutions with corresponding errors
  //   */
  //  def checkOverloadedCall(
  //    call: source.Call,
  //    target: source.IdTarget,
  //    targs: List[ValueType],
  //    args: List[source.ArgSection],
  //    expected: Option[Type]
  //  )(implicit C: Context): ValueType = {
  //
  //    val scopes = target.definition match {
  //      // an overloaded call target
  //      case CallTarget(name, syms) => syms
  //      // already resolved by a previous attempt to typecheck
  //      case sym                    => List(Set(sym))
  //    }
  //
  //    // TODO improve: stop typechecking if one scope was successful
  //
  //    val stateBefore = C.backupTyperstate()
  //
  //    // TODO try to avoid duplicate error messages
  //    val results = scopes map { scope =>
  //      scope.toList.map { sym =>
  //        sym -> Try {
  //          C.restoreTyperstate(stateBefore)
  //          val tpe = Context.blockTypeOption(sym).getOrElse {
  //            if (sym.isInstanceOf[ValueSymbol]) {
  //              Context.abort(s"Expected a function type.")
  //            } else {
  //              Context.abort(s"Cannot find type for ${sym.name} -- if it is a recursive definition try to annotate the return type.")
  //            }
  //          }
  //          val r = checkCallTo(call, sym.name.name, tpe, targs, args, expected)
  //          (r, C.backupTyperstate())
  //        }
  //      }
  //    }
  //
  //    val successes = results.map { scope => scope.collect { case (sym, Right(r)) => sym -> r } }
  //    val errors = results.flatMap { scope => scope.collect { case (sym, Left(r)) => sym -> r } }
  //
  //    successes foreach {
  //      // continue in outer scope
  //      case Nil => ()
  //
  //      // Exactly one successful result in the current scope
  //      case List((sym, (tpe, st))) =>
  //        // use the typer state after this checking pass
  //        C.restoreTyperstate(st)
  //        // reassign symbol of fun to resolved calltarget symbol
  //        C.assignSymbol(target.id, sym)
  //
  //        return tpe
  //
  //      // Ambiguous reference
  //      case results =>
  //        val sucMsgs = results.map {
  //          case (sym, tpe) =>
  //            s"- ${sym.name} of type ${Context.blockTypeOf(sym)}"
  //        }.mkString("\n")
  //
  //        val explanation =
  //          s"""| Ambiguous reference to ${target.id}. The following blocks would typecheck:
  //              |
  //              |${sucMsgs}
  //              |""".stripMargin
  //
  //        C.abort(explanation)
  //    }
  //
  //    errors match {
  //      case Nil =>
  //        C.abort("Cannot typecheck call, no function found")
  //
  //      // exactly one error
  //      case List((sym, errs)) =>
  //        val msg = errs.head
  //        val msgs = errs.tail
  //        C.buffer.append(msgs)
  //        // reraise and abort
  //        // TODO clean this up
  //        C.at(msg.value.asInstanceOf[Tree]) { C.abort(msg.label) }
  //
  //      case failed =>
  //        // reraise all and abort
  //        val msgs = failed.flatMap {
  //          // TODO also print qualified name and signature!
  //          case (block, msgs) => msgs.map { m => m.copy(label = s"Possible overload ${block.name.name}: ${m.label}") }
  //        }.toVector
  //
  //        C.reraise(msgs)
  //
  //        C.abort(s"Cannot typecheck call. There are multiple overloads, which all fail to check.")
  //    }
  //  }

  def checkCallTo(
    call: source.Call,
    name: String,
    funTpe: FunctionType,
    targs: List[ValueType],
    args: List[source.ArgSection],
    expected: Option[Type]
  )(implicit C: Context): ValueType = {

    // (1) Instantiate blocktype
    // e.g. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
    val (rigids, bt @ FunctionType(_, params, ret)) = Unification.instantiate(funTpe)

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

      case (List(bt: FunctionType), arg: source.BlockArg) =>
        checkBlockArgument(bt, arg)

      case (List(ct: InterfaceType), arg: source.CapabilityArg) =>
        checkCapabilityArgument(ct, arg)

      case (_, _) =>
        Context.error("Wrong type of argument section")
    }

    def checkValueArgument(tpe: ValueType, arg: source.Expr): Unit = Context.at(arg) {
      val tpe1 = Context.unifier substitute tpe // apply what we already know.
      val tpe2 = arg checkAgainst tpe1

      // Update substitution with new information
      Context.unify(tpe1, tpe2)
    }

    // Example.
    //   BlockParam: def foo { f: Int => String / Print }
    //   BlockArg: foo { n => println("hello" + n) }
    //     or
    //   BlockArg: foo { (n: Int) => println("hello" + n) }
    def checkBlockArgument(tpe: FunctionType, arg: source.BlockArg): Unit = Context.at(arg) {
      val bt @ FunctionType(Nil, params, tpe1) = Context.unifier substitute tpe

      Context.define {
        checkAgainstDeclaration("block", params, arg.params)
      }

      val tpe2 = arg.body checkAgainst tpe1

      Context.unify(tpe1, tpe2)
    }

    def checkCapabilityArgument(tpe: InterfaceType, arg: source.CapabilityArg) = Context.at(arg) {
      val tpe1 = Context.unifier substitute tpe
      val tpe2 = arg.definition.tpe
      Context.unify(tpe1, tpe2)
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

    Context.unifier.substitute(ret)
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

  private def freeTypeVars(o: Any): Set[TypeVar] = o match {
    case t: symbols.TypeVar => Set(t)
    case FunctionType(tparams, params, ret) =>
      freeTypeVars(params) ++ freeTypeVars(ret) -- tparams.toSet
    // case e: Effects            => freeTypeVars(e.toList)
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case _ =>
      Set.empty
  }

  private implicit class ExprOps(expr: Expr) {
    def checkAgainst(tpe: ValueType)(implicit C: Context): ValueType =
      checkExpr(expr, Some(tpe))
  }

  private implicit class StmtOps(stmt: Stmt) {
    def checkAgainst(tpe: ValueType)(implicit C: Context): ValueType =
      checkStmt(stmt, Some(tpe))
  }

  /**
   * Combinators that also store the computed type for a tree in the TypesDB
   */
  def checkAgainst[T <: Tree](t: T, expected: Option[Type])(f: T => ValueType)(implicit C: Context): ValueType =
    Context.at(t) {
      val got = f(t)
      wellformed(got)
      expected foreach { Context.unify(_, got) }
      C.assignType(t, got)
      got
    }

  def checkAgainstBlock[T <: Tree](t: T, expected: Option[Type])(f: T => BlockType)(implicit C: Context): BlockType =
    Context.at(t) {
      val got = f(t)
      wellformed(got)
      expected foreach { Context.unify(_, got) }
      C.assignType(t, got)
      got
    }
}

/**
 * Instances of this class represent an immutable backup of the typer state
 */
private[typer] case class TyperState(annotations: Annotations, unifier: Unifier)

trait TyperOps extends ContextOps { self: Context =>

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
    val result = super.in(block)

    // TyperState has two kinds of components:
    // - state-like (like annotations and unification constraints)
    //
    // The dynamic scoping of `in` should only affect the "reader" components of `typerState`, but
    // not the "state" components. For those, we manually perform backup and restore in typer.
    result
  }

  private[typer] def initTyperstate(): Unit = {
    annotations = Annotations.empty
    currentUnifier = Unifier.empty
  }

  private[typer] def backupTyperstate(): TyperState =
    TyperState(annotations.copy, currentUnifier)

  private[typer] def restoreTyperstate(st: TyperState): Unit = {
    annotations = st.annotations.copy
    currentUnifier = st.unifier
  }

  private[typer] def commitTypeAnnotations(): Unit = {
    annotations.commit()
    annotate(Annotations.Unifier, module, currentUnifier)
  }

  // Inferred types
  // ==============

  private[typer] def assignType(t: Tree, e: ValueType): Context = {
    annotations.annotate(Annotations.InferredType, t, e)
    this
  }

  // TODO actually store in DB
  private[typer] def assignType(t: Tree, e: BlockType): Context = this

  // this also needs to be backtrackable to interact correctly with overload resolution
  private[typer] def annotateBlockArgument(t: source.BlockArg, tpe: FunctionType): Context = {
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

  private[typer] def define(s: Symbol, t: BlockType): Context = {
    assignType(s, t); this
  }

  private[typer] def define(bs: Map[Symbol, Type]): Context = {
    bs foreach {
      case (v: ValueSymbol, t: ValueType) => define(v, t)
      case (v: BlockSymbol, t: FunctionType) => define(v, t)
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
