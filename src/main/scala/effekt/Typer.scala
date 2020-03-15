package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.source.{ Tree, Expr, Id, Stmt, Def, FunDef }
import effekt.namer.Environment
import effekt.symbols._
import effekt.symbols.builtins._

import effekt.util.messages.{ MessageBuffer, ErrorReporter }

import org.bitbucket.inkytonik.kiama.util.Memoiser

// We add a dependency to driver to resolve types of symbols from other modules
/**
   * Output: the types we inferred for function like things are written into "types"
   *   - Blocks
   *   - Functions
   *   - Resumptions
   */
class Typer(given types: TypesDB, symbols: SymbolsDB) {

  given Assertions

  def run(module: source.ModuleDecl, env: Environment, buffer: MessageBuffer): Unit = {
    val toplevelEffects = Effects(List(EDivZero, EConsole) ++ env.types.values.collect { case e: Effect => e })
    Context(module, buffer, toplevelEffects) in {
      // pre-check to allow mutually recursive defs
      module.defs.foreach { d => precheckDef(d) }
      module.defs.foreach { d =>
        val (_ / effs) = synthDef(d)
        if (effs.nonEmpty)
          Context.at(d).error("Unhandled effects: " + effs)
      }
    }
  }

  //<editor-fold desc="expressions">

  def checkExpr(expected: Option[Type]): Checker[Expr] = checkAgainst(expected) {
    case source.IntLit(n) => TInt / Pure
    case source.BooleanLit(n) => TBoolean / Pure
    case source.UnitLit() => TUnit / Pure
    case source.DoubleLit(n) => TDouble / Pure
    case source.StringLit(s) => TString / Pure

    case source.If(cond, thn, els) =>
      val (cndTpe / cndEffs) = cond checkAgainst TBoolean
      val (thnTpe / thnEffs) = checkStmt(expected)(thn)
      val (elsTpe / elsEffs) = els checkAgainst thnTpe
      thnTpe / (cndEffs ++ thnEffs ++ elsEffs)

    case source.While(cond, block) =>
      val (_ / condEffs) = cond checkAgainst TBoolean
      val (_ / blockEffs) = block checkAgainst TUnit
      TUnit / (condEffs ++ blockEffs)

    case source.Var(id) =>
      Context.getValueType(id.symbol) / Pure

    case source.Assign(id, expr) =>
      id.symbol.asVarBinder // assert that it is a mutable variable
      val (_ / eff) = expr checkAgainst Context.getValueType(id.symbol)
      TUnit / eff

    case source.Call(fun, targs, args) =>
      checkCall(expected)(fun.symbol, targs map { resolveValueType }, args)

    case source.TryHandle(prog, clauses) =>

      val (ret / effs) = checkStmt(expected)(prog)

      val effectOps = clauses.map { c => c.op.symbol.asEffectOp }
      val effects = effectOps.map { _.effect }
      val requiredOps = effects.flatMap { _.ops }
      val notCovered = requiredOps.toSet -- effectOps.toSet

      if (notCovered.nonEmpty) {
        val explanation = notCovered.map { op => s"${op.name} of effect ${op.effect.name}" }.mkString(", ")
        Context.error(s"Missing definitions for effect operations: ${explanation}")
      }

      var handlerEffs = Pure

      clauses.map {
        case source.OpClause(op, params, body, resume) =>
          val effectOp = op.symbol.asEffectOp
          val effect = effectOp.effect
          val bt = Context.getBlockType(effectOp)
          val ps = checkAgainstDeclaration(op.name, bt.params, params)
          val resumeType = BlockType(Nil, List(List(effectOp.ret.get.tpe)), ret / Pure)

          Context.define(ps).define(resume.symbol, resumeType) in {
              val (_ / heffs) = body checkAgainst ret
              handlerEffs = handlerEffs ++ heffs
            }
      }
      ret / ((effs -- Effects(effects)) ++ handlerEffs)

    case source.MatchExpr(sc, clauses) =>
      val (tpe / effs) = checkExpr(None)(sc)

      val datatype = tpe match {
        case d: DataType => d
        case TypeApp(d, _) => d
      }

      // check exhaustivity
      val covered = clauses.map { c => c.op.symbol }.toSet
      val cases: Set[Symbol] = datatype.ctors.toSet
      val notCovered = cases -- covered

      if (notCovered.nonEmpty) {
        Context.error(s"Missing cases: ${notCovered}")
      }

      val tpes = clauses.map {
        case source.Clause(id, params, body) =>
          val sym = id.symbol.asConstructor

          val (dataType / _) = sym.ret.get

          // instantiate params: create unifier
          val u = Unifier(sym.tparams).merge(dataType, tpe)
          val pms = u substitute extractAllTypes(sym.params)

          val ps = checkAgainstDeclaration(id.name, pms, params)
          Context.define(ps) in { checkStmt(expected)(body) }
      }

      val (tpeCases / effsCases) = tpes.reduce { case (tpe1 / effs1, tpe2 / effs2) =>
        tpe1 =!= tpe2
        tpe1 / (effs1 ++ effs2)
      }
      tpeCases / (effsCases ++ effs)
  }

  //</editor-fold>


  //<editor-fold desc="statements and definitions">

  def checkStmt(expected: Option[Type]): Checker[Stmt] = checkAgainst(expected) {

    case source.DefStmt(d @ source.EffDef(id, tps, ps, ret), rest) =>
      precheckDef(d) // to bind types to the effect ops
      Context.withEffect(id.symbol.asEffect) in { checkStmt(expected)(rest) }

    case source.DefStmt(b, rest) =>
      val (t / effBinding) = { precheckDef(b); synthDef(b) }
      val (r / effStmt)    = Context.define(b.id.symbol, t) in { checkStmt(expected)(rest) }
      r / (effBinding ++ effStmt)

    // <expr> ; <stmt>
    case source.ExprStmt(e, rest) =>
      val (_ / eff1) = checkExpr(None)(e)
      val (r / eff2) = checkStmt(expected)(rest)
      r / (eff1 ++ eff2)

    case source.Return(e) => checkExpr(expected)(e)
  }

  // not really checking, only if defs are fully annotated, we add them to the typeDB
  // this is necessary for mutually recursive definitions
  def precheckDef(d: Def)(given Context): Unit = Context at d in { d match {
    case source.FunDef(id, tparams, params, ret, body) =>
      val sym = id.symbol.asFun
      sym.ret.foreach { annot => types.put(sym, sym.toType) }

    case source.ExternFun(pure, id, tparams, params, tpe, body) =>
      val sym = id.symbol.asFun
      types.put(sym, sym.toType)

    case source.EffDef(id, tparams, params, ret) =>
      val sym: UserEffect = id.symbol.asUserEffect
      sym.ops.foreach { op => types.put(op, op.toType) }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.id.symbol.asFun
        types.put(sym, sym.toType)
      }

    case d => ()
  }}


  def synthDef: Checker[Def] = checking {
    case source.FunDef(id, tparams, params, ret, body) =>
      val sym = id.symbol.asUserFunction

      Context.define(sym.params) in {
        sym.ret match {
          case Some(tpe / funEffs) =>
            val (_ / effs) = body checkAgainst tpe
            effs <:< Context.effects // check they are in scope
            tpe / (effs -- funEffs) // the declared effects are considered as bound
          case None =>
            val (tpe / effs) = checkStmt(None)(body)
            effs <:< Context.effects // check they are in scope
            types.put(sym, sym.toType(tpe / effs))
            tpe / Pure // all effects are handled by the function itself (since they are inferred)
        }
      }

    case source.ValDef(id, annot, binding) =>
      val sym = id.symbol.asValBinder
      sym.tpe match {
        case Some(t) => binding checkAgainst t
        case None    => checkStmt(None)(binding)
      }
    case source.VarDef(id, annot, binding) =>
      val sym = id.symbol.asVarBinder
      sym.tpe match {
        case Some(t) => binding checkAgainst t
        case None    => checkStmt(None)(binding)
      }

    // all other defintions have already been prechecked
    case d => TUnit / Pure
  }

  //</editor-fold>


  //<editor-fold desc="arguments and parameters">

  def resolveValueType(tpe: source.Type)(given Context): ValueType = tpe match {
    case source.TypeApp(id, args) => TypeApp(id.symbol.asDataType, args.map(resolveValueType))
    case source.TypeVar(id) => id.symbol.asValueType
  }

  /**
   * Invariant: Only call this on declarations that are fully annotated
   */
  def extractAllTypes(params: Params)(given Context): Sections = params map extractTypes

  def extractTypes(params: List[ValueParam] | BlockParam)(given Context): List[ValueType] | BlockType = params match {
    case BlockParam(_, tpe) => tpe
    case ps: List[ValueParam] => ps map {
      case ValueParam(_, Some(tpe)) => tpe
      case _ => Context.abort("Cannot extract type")
    }
  }

  /**
   * Returns the binders that will be introduced to check the corresponding body
   */
  def checkAgainstDeclaration(
    name: String,
    atCallee: List[List[ValueType] | BlockType],
    // we ask for the source Params here, since it might not be annotated
    atCaller: List[source.ParamSection])(given Context): Map[Symbol, Type] = {

    if (atCallee.size != atCaller.size)
      Context.error(s"Wrong number of argument sections, given ${atCaller.size}, but ${name} expects ${atCallee.size}.")

    // TODO add blockparams here!
    (atCallee zip atCaller).flatMap[(Symbol, Type)] {
      case (b1: BlockType, b2: source.BlockParam) =>
        Context.abort("not yet supported")
        ???

      case (ps1: List[ValueType], source.ValueParams(ps2)) =>
        (ps1 zip ps2).map[(Symbol, Type)] {
          case (decl, p @ source.ValueParam(id, annot)) =>
            val annotType = annot.map(resolveValueType)
            annotType.foreach { t => decl =!= t }
            (id.symbol, annotType.getOrElse(decl)) // use the annotation, if present.
        }.toMap
    }.toMap
  }


  def checkCall(expected: Option[Type])(sym: Symbol, targs: List[ValueType], args: List[source.ArgSection])(given Context): Effectful = {

    val BlockType(tparams, params, ret / retEffs) = Context.getBlockType(sym)

    if (targs.nonEmpty && targs.size != tparams.size)
      Context.abort(s"Wrong number of type arguments ${targs.size}")

    var unifier: Unifier = Unifier(tparams, if (targs.nonEmpty) { (tparams zip targs).toMap } else { Map.empty })

    expected.foreach { exp => unifier = unifier.merge(ret, exp) }

    var effs = retEffs

    if (params.size != args.size)
      Context.error(s"Wrong number of argument sections, given ${args.size}, but ${sym.name} expects ${params.size}.")

    // TODO we can improve the error positions here
    (params zip args) foreach {
      case (ps : List[ValueType], source.ValueArgs(as)) =>
        if (ps.size != as.size)
          Context.error(s"Wrong number of arguments. Argument section of ${sym.name} requires ${ps.size}, but ${as.size} given.")

        (ps zip as) foreach {
          case (tpe, expr) =>
            val tpe1 = unifier substitute tpe // apply what we already know.
            val (tpe2 / exprEffs) = checkExpr(None)(expr)

            unifier = unifier.merge(tpe1, tpe2)
            // println(s"From comparing ${tpe} and ${tpe2}, we just learnt that ${unifier}")

            effs = effs ++ exprEffs
        }

      // Example.
      //   BlockParam: def foo { f: Int => String / Print }
      //   BlockArg: foo { n => println("hello" + n) }
      //     or
      //   BlockArg: foo { (n: Int) => println("hello" + n) }
      case (bt: BlockType, source.BlockArg(params, stmt)) =>

        val blockType = unifier substitute bt
        // TODO make blockargs also take multiple argument sections.
        val bindings = checkAgainstDeclaration("block", blockType.params, List(source.ValueParams(params)))

        Context.define(bindings) in {
          val (tpe1 / handled) = blockType.ret
          val (tpe2 / stmtEffs)  = checkStmt(None)(stmt)

          unifier = unifier.merge(tpe1, tpe2)
          effs = (effs ++ (stmtEffs -- handled))
        }

      case (_, _) =>
        Context.error("Wrong type of argument section")
    }

    // check that unifier found a substitution for each tparam
    unifier.checkFullyDefined

    (unifier substitute ret) / effs
  }

  //</editor-fold>


  /**
   * Checker is a helper type, that simplifies passing around the typing context
   *
   * @tparam T the type of trees, this checker operates on
   */
  type Checker[T <: Tree] = T => (given Context) => Effectful

  def checkAgainst[T <: Tree](expected: Option[Type])(f: Checker[T]): Checker[T] = t => {
    Context at t in {
      Context.aborting {
        val (got / effs) = f(t)
        expected map { got =!= _ }
        got / effs
      }
    }
  }

  def checking[T <: Tree](f: Checker[T]): Checker[T] = t => {
    Context at t in {
      Context.aborting {
        f(t)
      }
    }
  }

  /**
   * Extension methods to improve readability of Typer
   */
  trait TyperOps {
    def (tpe: ValueType) / (effs: Effects) : Effectful = Effectful(tpe, effs)
    def (expr: Expr) checkAgainst (tpe: Type) (given Context): Effectful = checkExpr(Some(tpe))(expr)
    def (stmt: Stmt) checkAgainst (tpe: Type) (given Context): Effectful = checkStmt(Some(tpe))(stmt)
  }
  given TyperOps

  // this requires splitting in context related and Ops-related methods
  // define one XXXAssertions for every phase that requires being mixed with ErrorReporter
  case class Context(
    focus: Tree,                                // current type checking position
    buffer: MessageBuffer,
    effects: Effects = Pure,                    // the effects, whose declarations are lexically in scope (i.e. a conservative approximation of possible capabilities

    // TODO also move to a global map from symbol to type (for IDE support)
    values: Map[Symbol, ValueType] = Map.empty  // the types of value variables in the current environment
  ) extends TyperAssertions {

    // TODO does this correctly compare List[Int] with List[Int]?
    def (got: Type) =!= (expected: Type): Unit = (got, expected) match {
      case (TypeApp(c1, args1), TypeApp(c2, args2)) if c1 == c2 =>
        (args1 zip args2) foreach { _ =!= _ }
      case (t1, t2) => if (t1 != t2) {
        error(s"Expected $expected, but got $got")
      }
    }

    def getValueType(sym: Symbol): ValueType =
      values.getOrElse(sym, abort(s"Cannot find value binder for ${sym}."))

    def getBlockType(sym: Symbol): BlockType =
      types.blockType(sym)(given this)

    def define(s: Symbol, t: ValueType) = copy(values = values + (s -> t))

    def define(s: Symbol, t: BlockType) = {
      types.put(s, t)(given this)
      this
    }

    def define(bs: Map[Symbol, Type]): Context = bs.foldLeft(this) {
      case (ctx, (v: ValueSymbol, t: ValueType)) => ctx.define(v, t)
      case (ctx, (v: BlockSymbol, t: BlockType)) => ctx.define(v, t)
    }
    def define(ps: List[List[ValueParam] | BlockParam]): Context = define(ps.flatMap {
      case ps : List[ValueParam] => ps map {
        case s @ ValueParam(name, Some(tpe)) => s -> tpe
        case s @ ValueParam(name, None) => ??? // non annotated handler, or block param
      }
      case s @ BlockParam(name, tpe) => List(s -> tpe)
    }.toMap)

    def at(node: Tree): Context = copy(focus = node)

    def withEffect(e: Effect): Context = this.copy(effects = effects + e)

    def current: Context = this

    // always first look at the annotated type, then look it up in the dictionary
    def (f: Fun) returnType: Effectful = f.ret match {
      case Some(t) => t
      case None => types.blocks.getOrDefault(f,
        abort(s"Result type of recursive function ${f.name} needs to be annotated")).ret
    }
  }
  def Context(given c: Context): Context = c
}

trait TyperAssertions extends ErrorReporter {

  def (a: Effects) <:< (b: Effects): Effects = {
    val forbidden = a -- b
    if (forbidden.nonEmpty) {
      error(s"Inferred effects ${a.distinct} are not a subset of allowed / annotated effects ${b.distinct}.")
      b
    } else {
      b
    }
  }

  def (t: source.Type) asTypeVar: source.TypeVar = t match {
    case t: source.TypeVar => t
    case _ => abort("Expected a value type")
  }
  def (t: Type) asBlockType: BlockType = t match {
    case t: BlockType => t
    case _ => abort("Expected a block type")
  }
  def (t: Type) asValueType: ValueType = t match {
    case t: ValueType => t
    case _ => abort("Expected a value type")
  }
}