package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.Context
import effekt.context.assertions.{ SymbolAssertions, TypeAssertions }
import effekt.source.{ Def, Expr, Stmt, Tree }
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
  effects: Effects = Pure // the effects, whose declarations are _lexically_ in scope
)

class Typer extends Phase[Module, Module] { typer =>

  val phaseName = "typer"

  def run(mod: Module)(implicit C: Context): Option[Module] = {

    val module = mod.decl

    // Effects that are lexically in scope at the top level
    val toplevelEffects = Effects(mod.types.values.collect {
      case e: Effect => e
    })
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
  }

  //<editor-fold desc="expressions">

  def checkExpr(expr: Expr, expected: Option[Type])(implicit C: Context): Effectful =
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
        checkCall(c.definition, targs map { resolveValueType }, args, expected)

      case source.TryHandle(prog, clauses) =>

        val (ret / effs) = checkStmt(prog, expected)

        val effectOps = clauses.map { c => c.definition }
        val effects = effectOps.map { _.effect }
        val requiredOps = effects.flatMap { _.ops }
        val notCovered = requiredOps.toSet -- effectOps.toSet

        if (notCovered.nonEmpty) {
          val explanation = notCovered.map { op => s"${op.name} of effect ${op.effect.name}" }.mkString(", ")
          Context.error(s"Missing definitions for effect operations: ${explanation}")
        }

        var handlerEffs = Pure

        clauses foreach {
          case d @ source.OpClause(op, params, body, resume) =>
            val effectOp = d.definition
            val bt = Context.blockTypeOf(effectOp)
            val ps = checkAgainstDeclaration(op.name, bt.params, params)
            val resumeType = BlockType(Nil, List(List(effectOp.ret.get.tpe)), ret / Pure)

            Context.define(ps).define(Context.symbolOf(resume), resumeType) in {
              val (_ / heffs) = body checkAgainst ret
              handlerEffs = handlerEffs ++ heffs
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

        // (2) Extract datatype to pattern match on
        // i.e. List
        val datatype = tpe match {
          case d: DataType             => d
          case TypeApp(d: DataType, _) => d
          case other                   => Context.abort(s"Cannot pattern match on value of type ${other}")
        }

        // (3) check exhaustivity
        val covered = clauses.map { c => c.definition }.toSet
        val cases: Set[Symbol] = datatype.ctors.toSet
        val notCovered = cases -- covered

        if (notCovered.nonEmpty) {
          Context.error(s"Missing cases: ${notCovered}")
        }

        val tpes = clauses.map {
          case c @ source.Clause(id, annotatedParams, body) =>
            val sym = c.definition

            // (4) Compute blocktype of this constructor with rigid type vars
            // i.e. Cons : `(?t1, List[?t1]) => List[?t1]`
            val (rigids, BlockType(_, pms, ret / _)) = Substitution.instantiate(sym.toType)

            // (5) given a scrutinee of `List[Int]`, we learn `?t1 -> Int`
            val subst = Substitution.unify(ret, tpe)

            // (6) check for existential type variables
            // at the moment we do not allow existential type parameters on constructors.
            val skolems = rigids.filterNot { subst.isDefinedAt }
            if (skolems.nonEmpty) {
              Context.error(s"Unbound type variables in constructor ${id}: ${skolems.map(_.underlying).mkString(", ")}")
            }

            // (7) refine parameter types of constructor
            // i.e. `(Int, List[Int])`
            val constructorParams = subst substitute pms

            // (8) check against parameters (potentially) annotated clause
            val paramBindings = checkAgainstDeclaration(id.name, constructorParams, annotatedParams)

            // (9) check body with bindings in place
            Context.define(paramBindings) in {
              checkStmt(body, expected)
            }
        }

        val (tpeCases / effsCases) = tpes.reduce[Effectful] {
          case (tpe1 / effs1, tpe2 / effs2) =>
            Substitution.unify(tpe1, tpe2)
            tpe1 / (effs1 ++ effs2)
        }
        tpeCases / (effsCases ++ effs)
    }

  //</editor-fold>

  //<editor-fold desc="statements and definitions">

  def checkStmt(stmt: Stmt, expected: Option[Type])(implicit C: Context): Effectful =
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

      case source.Return(e) => checkExpr(e, expected)
    }

  // not really checking, only if defs are fully annotated, we add them to the typeDB
  // this is necessary for mutually recursive definitions
  def precheckDef(d: Def)(implicit C: Context): Unit = Context.focusing(d) {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      d.symbol.ret.foreach { annot => Context.assignType(d.symbol, d.symbol.toType) }

    case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
      Context.assignType(d.symbol, d.symbol.toType)

    case d @ source.EffDef(id, ops) =>
      d.symbol.ops.foreach { op => Context.assignType(op, op.toType) }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.symbol
        Context.assignType(sym, sym.toType)
      }

    case d => ()
  }

  def synthDef(d: Def)(implicit C: Context): Effectful = check(d) {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      val sym = d.symbol
      Context.define(sym.params)
      sym.ret match {
        case Some(tpe / funEffs) =>
          val (_ / effs) = body checkAgainst tpe
          Context.wellscoped(effs)
          tpe / (effs -- funEffs) // the declared effects are considered as bound
        case None =>
          val (tpe / effs) = checkStmt(body, None)
          Context.wellscoped(effs) // check they are in scope
          Context.assignType(sym, sym.toType(tpe / effs))
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

    // all other defintions have already been prechecked
    case d => TUnit / Pure
  }

  //</editor-fold>

  //<editor-fold desc="arguments and parameters">

  // TODO we can remove this duplication, once every phase can write to every table.
  // then the namer phase can already store the resolved type symbol for the param.

  def resolveValueType(tpe: source.ValueType)(implicit C: Context): ValueType = tpe match {
    case t @ source.TypeApp(id, args) => TypeApp(t.definition, args.map(resolveValueType))
    case t @ source.TypeVar(id)       => t.definition
  }

  /**
   * Invariant: Only call this on declarations that are fully annotated
   */
  def extractAllTypes(params: Params)(implicit C: Context): Sections = params map extractTypes

  def extractTypes(params: List[Param])(implicit C: Context): List[Type] = params map {
    case BlockParam(_, tpe) => tpe
    case ValueParam(_, Some(tpe)) => tpe
    case _ => Context.abort("Cannot extract type")
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
        Context.at(b2) { Context.abort("Internal Compiler Error: Not yet supported") }
        ???

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
  def checkCall(
    target: CallTarget,
    targs: List[ValueType],
    args: List[source.ArgSection],
    expected: Option[Type]
  )(implicit C: Context): Effectful = {

    target.symbols.toList.map { sym =>
      sym -> Try { checkCallTo(sym, targs, args, expected) }
    }.partitionMap {
      case (sym, Left(msg))  => Left((sym, msg))
      case (sym, Right(res)) => Right((sym, res))
    } match {

      // Exactly one successful result
      case (_, List((sym, tpe))) =>
        target.symbols = Set(sym)
        tpe

      // Ambiguous reference
      case (_, succeeded) if succeeded.size > 1 =>
        val sucMsgs = succeeded.map {
          case (sym, tpe) =>
            s"- ${sym.name} of type ${Context.blockTypeOf(sym)}"
        }.mkString("\n")

        val explanation =
          s"""| Ambiguous reference to ${target.name}. The following blocks would typecheck:
              |
              |${sucMsgs}
              |""".stripMargin

        C.abort(explanation)

      // Exactly one error
      case (List((sym, errs)), Nil) =>
        val msg = errs.head
        val msgs = errs.tail
        C.buffer.append(msgs)
        // reraise and abort
        // TODO clean this up
        C.at(msg.value.asInstanceOf[Tree]) { C.abort(msg.label) }

      case (failed, Nil) =>
        C.abort(s"Cannot typecheck call. There are multiple overloads, which all fail to check.")
    }
  }

  def checkCallTo(
    sym: BlockSymbol,
    targs: List[ValueType],
    args: List[source.ArgSection],
    expected: Option[Type]
  )(implicit C: Context): Effectful = {

    // (1) Instantiate blocktype
    // e.g. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
    val (rigids, BlockType(_, params, ret / retEffs)) = Substitution.instantiate(Context.blockTypeOf(sym))

    if (targs.nonEmpty && targs.size != rigids.size)
      Context.abort(s"Wrong number of type arguments ${targs.size}")

    // (2) Compute substitutions from provided type arguments (if any)
    var subst: Substitutions = if (targs.nonEmpty) { (rigids zip targs).toMap } else { Map.empty }

    // (3) refine substitutions by matching return type against expected type
    expected.foreach { expectedReturn =>
      val refinedReturn = subst substitute ret
      subst = subst union Substitution.unify(refinedReturn, expectedReturn)
    }

    var effs = retEffs

    if (params.size != args.size)
      Context.error(s"Wrong number of argument sections, given ${args.size}, but ${sym.name} expects ${params.size}.")

    def checkArgumentSection(ps: List[Type], args: source.ArgSection): Unit = (ps, args) match {
      case (ps: List[ValueType @unchecked], source.ValueArgs(as)) =>
        if (ps.size != as.size)
          Context.error(s"Wrong number of arguments. Argument section of ${sym.name} requires ${ps.size}, but ${as.size} given.")

        (ps zip as) foreach { case (tpe, expr) => checkValueArgument(tpe, expr) }

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
      subst = subst union Substitution.unify(tpe1, tpe2)

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
        checkAgainstDeclaration("block", params, List(arg.params))
      }

      val (tpe2 / stmtEffs) = arg.body checkAgainst tpe1

      subst = subst union Substitution.unify(tpe1, tpe2)
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

    subst.checkFullyDefined(rigids)

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
    def checkAgainst(tpe: Type)(implicit C: Context): Effectful =
      checkExpr(expr, Some(tpe))
  }

  private implicit class StmtOps(stmt: Stmt) {
    def checkAgainst(tpe: Type)(implicit C: Context): Effectful =
      checkStmt(stmt, Some(tpe))
  }

  /**
   * Combinators that also store the computed type for a tree in the TypesDB
   */
  def checkAgainst[T <: Tree](t: T, expected: Option[Type])(f: T => Effectful)(implicit C: Context): Effectful =
    Context.at(t) {
      val (got / effs) = f(t)
      expected foreach { Substitution.unify(_, got) }
      Context.assignType(t, got / effs)
      got / effs
    }

  def check[T <: Tree](t: T)(f: T => Effectful)(implicit C: Context): Effectful =
    Context.at(t) {
      val (got / effs) = f(t)
      Context.assignType(t, got / effs)
      got / effs
    }
}

trait TyperOps { self: Context =>

  // State Access
  // ============
  private[typer] def effects: Effects = typerState.effects

  private[typer] def withEffect(e: Effect): Context = {
    typerState = typerState.copy(effects = typerState.effects + e);
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

  private[typer] def define(s: Symbol, t: BlockType): Context = {
    assignType(s, t); this
  }

  private[typer] def define(bs: Map[Symbol, Type]): Context = {
    bs foreach {
      case (v: ValueSymbol, t: ValueType) => define(v, t)
      case (v: BlockSymbol, t: BlockType) => define(v, t)
      case other => abort(s"Internal Error: wrong combination of symbols and types: ${other}")
    }; this
  }

  private[typer] def define(ps: List[List[Param]]): Context = {
    ps.flatten.foreach {
      case s @ ValueParam(name, Some(tpe)) => define(s, tpe)
      case s @ BlockParam(name, tpe) => define(s, tpe)
      case s => abort(s"Internal Error: Cannot add $s to context.")
    }
    this
  }
}
