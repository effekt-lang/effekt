package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Context, Phase }
import effekt.context.assertions.{ SymbolAssertions, TypeAssertions }
import effekt.source.{ Def, Expr, Stmt, Tree }
import effekt.symbols._
import effekt.symbols.builtins._

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

class Typer extends Phase { typer =>

  val name = "typer"

  def run(module: source.ModuleDecl, mod: Module)(implicit C: Context): Unit = {
    val toplevelEffects = Effects(List(EDivZero, EConsole) ++ mod.types.values.collect {
      case e: Effect => e
    })

    C.typerState = TyperState(toplevelEffects)
    C.phases.init(typer)

    C in {
      // pre-check to allow mutually recursive defs
      module.defs.foreach { d => precheckDef(d) }
      module.defs.foreach { d =>
        val (_ / effs) = synthDef(d)
        if (effs.nonEmpty)
          Compiler.at(d) {
            Compiler.error("Unhandled effects: " + effs)
          }
      }
    }
  }

  //<editor-fold desc="expressions">

  def checkExpr(expr: Expr, expected: Option[Type])(implicit C: Context): Effectful =
    checkAgainst(expr, expected) {
      case source.IntLit(n) => TInt / Pure
      case source.BooleanLit(n) => TBoolean / Pure
      case source.UnitLit() => TUnit / Pure
      case source.DoubleLit(n) => TDouble / Pure
      case source.StringLit(s) => TString / Pure

      case source.If(cond, thn, els) =>
        val (cndTpe / cndEffs) = cond checkAgainst TBoolean
        val (thnTpe / thnEffs) = checkStmt(thn, expected)
        val (elsTpe / elsEffs) = els checkAgainst thnTpe
        thnTpe / (cndEffs ++ thnEffs ++ elsEffs)

      case source.While(cond, block) =>
        val (_ / condEffs) = cond checkAgainst TBoolean
        val (_ / blockEffs) = block checkAgainst TUnit
        TUnit / (condEffs ++ blockEffs)

      case v : source.Var =>
        Compiler.valueType(v.definition) / Pure

      case e @ source.Assign(id, expr) =>
        e.definition.asVarBinder // assert that it is a mutable variable
        val (_ / eff) = expr checkAgainst Compiler.valueType(e.definition)
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
          Compiler.error(s"Missing definitions for effect operations: ${explanation}")
        }

        var handlerEffs = Pure

        clauses foreach {
          case d @ source.OpClause(op, params, body, resume) =>
            val effectOp = d.definition
            val effect = effectOp.effect
            val bt = Compiler.blockType(effectOp)
            val ps = checkAgainstDeclaration(op.name, bt.params, params)
            val resumeType = BlockType(Nil, List(List(effectOp.ret.get.tpe)), ret / Pure)

            Compiler.define(ps).define(Compiler.lookup(resume), resumeType) in {
                val (_ / heffs) = body checkAgainst ret
                handlerEffs = handlerEffs ++ heffs
              }
        }

        val unusedEffects = Effects(effects) -- effs

        if (unusedEffects.nonEmpty)
          Compiler.warning("Handling effects that are not used: " + unusedEffects)

        ret / ((effs -- Effects(effects)) ++ handlerEffs)

      case source.MatchExpr(sc, clauses) =>
        val (tpe / effs) = checkExpr(sc, None)

        val datatype = tpe match {
          case d: DataType => d
          case TypeApp(d: DataType, _) => d
        }

        // check exhaustivity
        val covered = clauses.map { c => c.definition }.toSet
        val cases: Set[Symbol] = datatype.ctors.toSet
        val notCovered = cases -- covered

        if (notCovered.nonEmpty) {
          Compiler.error(s"Missing cases: ${notCovered}")
        }

        val tpes = clauses.map {
          case c @ source.Clause(id, params, body) =>
            val sym = c.definition

            val (dataType / _) = sym.ret.get

            // instantiate params: create unifier
            val u = Unifier(sym.tparams).merge(dataType, tpe)
            val pms = u substitute extractAllTypes(sym.params)

            val ps = checkAgainstDeclaration(id.name, pms, params)
            Compiler.define(ps) in { checkStmt(body, expected) }
        }

        val (tpeCases / effsCases) = tpes.reduce[Effectful] { case (tpe1 / effs1, tpe2 / effs2) =>
          Compiler.assertEqual(tpe1, tpe2)
          tpe1 / (effs1 ++ effs2)
        }
        tpeCases / (effsCases ++ effs)
    }

  //</editor-fold>


  //<editor-fold desc="statements and definitions">

  def checkStmt(stmt: Stmt, expected: Option[Type])(implicit C: Context): Effectful =
    checkAgainst(stmt, expected) {
      case source.DefStmt(b, rest) =>
        val (t / effBinding) = Compiler in { precheckDef(b); synthDef(b) }
        val (r / effStmt)    = checkStmt(rest, expected)
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
  def precheckDef(d: Def)(implicit C: Context): Unit = C.at(d) { d match {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      d.symbol.ret.foreach { annot => C.putBlock(d.symbol, d.symbol.toType) }

    case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
      C.putBlock(d.symbol, d.symbol.toType)

    case d @ source.EffDef(id, tparams, params, ret) =>
      d.symbol.ops.foreach { op => C.putBlock(op, op.toType) }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.symbol
        C.putBlock(sym, sym.toType)
      }

    case d => ()
  }}


  def synthDef(d: Def)(implicit C: Context): Effectful = C.focusing(d) {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      val sym = d.symbol
      C.define(sym.params)
      sym.ret match {
        case Some(tpe / funEffs) =>
          val (_ / effs) = body checkAgainst tpe
          C.wellscoped(effs)
          tpe / (effs -- funEffs) // the declared effects are considered as bound
        case None =>
          val (tpe / effs) = checkStmt(body, None)
          C.wellscoped(effs) // check they are in scope
          C.putBlock(sym, sym.toType(tpe / effs))
          tpe / Pure // all effects are handled by the function itself (since they are inferred)
      }

    case d @ source.EffDef(id, tps, ps, ret) =>
      C.withEffect(d.symbol)
      TUnit / Pure

    case d @ source.ValDef(id, annot, binding) =>
      val (t / effBinding) = d.symbol.tpe match {
        case Some(t) => binding checkAgainst t
        case None    => checkStmt(binding, None)
      }
      C.define(d.symbol, t)
      t / effBinding

    case d @ source.VarDef(id, annot, binding) =>
      val (t / effBinding) = d.symbol.tpe match {
        case Some(t) => binding checkAgainst t
        case None    => checkStmt(binding, None)
      }
      C.define(d.symbol, t)
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
    case t @ source.TypeVar(id) => t.definition
  }

  /**
   * Invariant: Only call this on declarations that are fully annotated
   */
  def extractAllTypes(params: Params)(implicit C: Context): Sections = params map extractTypes

  def extractTypes(params: List[Param])(implicit C: Context): List[Type] = params map {
    case BlockParam(_, tpe) => tpe
    case ValueParam(_, Some(tpe)) => tpe
    case _ => C.abort("Cannot extract type")
  }

  /**
   * Returns the binders that will be introduced to check the corresponding body
   */
  def checkAgainstDeclaration(
    name: String,
    atCallee: List[List[Type]],
    // we ask for the source Params here, since it might not be annotated
    atCaller: List[source.ParamSection])(implicit C: Context): Map[Symbol, Type] = {

    if (atCallee.size != atCaller.size)
      C.error(s"Wrong number of argument sections, given ${atCaller.size}, but ${name} expects ${atCallee.size}.")

    // TODO add blockparams here!
    (atCallee zip atCaller).flatMap[(Symbol, Type)] {
      case (List(b1: BlockType), b2: source.BlockParam) =>
        C.abort("not yet supported")
        ???

      case (ps1: List[ValueType], source.ValueParams(ps2)) =>
        (ps1 zip ps2).map[(Symbol, Type)] {
          case (decl, p @ source.ValueParam(id, annot)) =>
            val annotType = annot.map(resolveValueType)
            annotType.foreach { t => C.assertEqual(decl, t) }
            (p.symbol, annotType.getOrElse(decl)) // use the annotation, if present.
        }.toMap
    }.toMap
  }


  def checkCall(
    sym: Symbol,
    targs: List[ValueType],
    args: List[source.ArgSection],
    expected: Option[Type]
  )(implicit C: Context): Effectful = {

    val BlockType(tparams, params, ret / retEffs) = C.blockType(sym)

    if (targs.nonEmpty && targs.size != tparams.size)
      C.abort(s"Wrong number of type arguments ${targs.size}")

    var unifier: Unifier = Unifier(tparams, if (targs.nonEmpty) { (tparams zip targs).toMap } else { Map.empty })

    expected.foreach { exp => unifier = unifier.merge(ret, exp) }

    var effs = retEffs

    if (params.size != args.size)
      C.error(s"Wrong number of argument sections, given ${args.size}, but ${sym.name} expects ${params.size}.")

    // TODO we can improve the error positions here
    (params zip args) foreach {
      case (ps : List[ValueType], source.ValueArgs(as)) =>
        if (ps.size != as.size)
          C.error(s"Wrong number of arguments. Argument section of ${sym.name} requires ${ps.size}, but ${as.size} given.")

        (ps zip as) foreach {
          case (tpe, expr) =>
            val tpe1 = unifier substitute tpe // apply what we already know.
            val (tpe2 / exprEffs) = checkExpr(expr, None)

            unifier = unifier.merge(tpe1, tpe2)
            // println(s"From comparing ${tpe} and ${tpe2}, we just learnt that ${unifier}")

            effs = effs ++ exprEffs
        }

      // Example.
      //   BlockParam: def foo { f: Int => String / Print }
      //   BlockArg: foo { n => println("hello" + n) }
      //     or
      //   BlockArg: foo { (n: Int) => println("hello" + n) }
      case (List(bt: BlockType), source.BlockArg(params, stmt)) =>

        val blockType = unifier substitute bt
        // TODO make blockargs also take multiple argument sections.
        val bindings = checkAgainstDeclaration("block", blockType.params, List(source.ValueParams(params)))

        C.define(bindings)

        val (tpe1 / handled) = blockType.ret
        val (tpe2 / stmtEffs)  = checkStmt(stmt, None)

        unifier = unifier.merge(tpe1, tpe2)
        effs = (effs ++ (stmtEffs -- handled))

      case (_, _) =>
        C.error("Wrong type of argument section")
    }

    // check that unifier found a substitution for each tparam
    unifier.checkFullyDefined

    (unifier substitute ret) / effs
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
   * @tparam T the type of trees, this checker operates on
   */
  def checkAgainst[T <: Tree](t: T, expected: Option[Type])(f: T => Effectful)(implicit C: Context): Effectful =
    Compiler.at(t) {
      val (got / effs) = f(t)
      expected foreach { Compiler.assertEqual(got, _) }
      Compiler.annotate(t, got / effs)
      got / effs
    }
}


trait TyperOps { self: Context =>

  // State Access
  // ============
  def effects: Effects = typerState.effects

  def withEffect(e: Effect): Context = {
    typerState = typerState.copy(effects = typerState.effects + e);
    this
  }

  // was =!=
  def assertEqual(got: Type, expected: Type): Unit = (got, expected) match {
    case (TypeApp(c1, args1), TypeApp(c2, args2)) if c1 == c2 =>
      (args1 zip args2) foreach { case (t1, t2) => assertEqual(t1, t2) }
    case (t1, t2) => if (t1 != t2) {
      error(s"Expected $expected, but got $got")
    }
  }

  def wellscoped(a: Effects): Unit = {
    val forbidden = Effects(a.effs.filterNot { e => e.builtin }) -- effects
    if (forbidden.nonEmpty) {
      error(s"Effects ${forbidden} leave their defining scope.")
    }
  }

  def define(s: Symbol, t: ValueType): Context = {
    putValue(s, t); this
  }

  def define(s: Symbol, t: BlockType): Context = {
    putBlock(s, t); this
  }

  def define(bs: Map[Symbol, Type]): Context = { bs foreach {
    case (v: ValueSymbol, t: ValueType) => define(v, t)
    case (v: BlockSymbol, t: BlockType) => define(v, t)
  }; this }

  def define(ps: List[List[Param]]): Context = {
    ps.flatten.foreach {
      case s @ ValueParam(name, Some(tpe)) => define(s, tpe)
      case s @ ValueParam(name, None) => ??? // non annotated handler, or block param
      case s @ BlockParam(name, tpe) => define(s, tpe)
    }
    this
  }
}