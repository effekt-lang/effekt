package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import context.{ CompilerContext, Phase }
import effekt.source.{ Def, Expr, Stmt, Tree }
import effekt.namer.Environment
import effekt.symbols._
import effekt.symbols.builtins._

// We add a dependency to driver to resolve types of symbols from other modules
/**
   * Output: the types we inferred for function like things are written into "types"
   *   - Blocks
   *   - Functions
   *   - Resumptions
   */
class Typer extends Phase { typer =>

  val name = "typer"

  case class State(
    effects: Effects = Pure // the effects, whose declarations are _lexically_ in scope
  )

  given (given C: CompilerContext): TyperOps = new TyperOps {}

  def run(module: source.ModuleDecl, mod: Module, compiler: CompilerContext): Unit = {
    val toplevelEffects = Effects(List(EDivZero, EConsole) ++ mod.types.values.collect { case e: Effect => e })
    compiler.phases.init(typer)(State(toplevelEffects))

    compiler in {
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

  def checkExpr(expected: Option[Type])(given CompilerContext): Checker[Expr] = checkAgainst(expected) {
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

    case v : source.Var =>
      Compiler.valueType(v.definition) / Pure

    case e @ source.Assign(id, expr) =>
      e.definition.asVarBinder // assert that it is a mutable variable
      val (_ / eff) = expr checkAgainst Compiler.valueType(e.definition)
      TUnit / eff

    case c @ source.Call(fun, targs, args) =>
      checkCall(expected)(c.definition, targs map { resolveValueType }, args)

    case source.TryHandle(prog, clauses) =>

      val (ret / effs) = checkStmt(expected)(prog)

      val effectOps = clauses.map { c => c.definition }
      val effects = effectOps.map { _.effect }
      val requiredOps = effects.flatMap { _.ops }
      val notCovered = requiredOps.toSet -- effectOps.toSet

      if (notCovered.nonEmpty) {
        val explanation = notCovered.map { op => s"${op.name} of effect ${op.effect.name}" }.mkString(", ")
        Compiler.error(s"Missing definitions for effect operations: ${explanation}")
      }

      var handlerEffs = Pure

      clauses.map {
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
      val (tpe / effs) = checkExpr(None)(sc)

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
          Compiler.define(ps) in { checkStmt(expected)(body) }
      }

      val (tpeCases / effsCases) = tpes.reduce { case (tpe1 / effs1, tpe2 / effs2) =>
        tpe1 =!= tpe2
        tpe1 / (effs1 ++ effs2)
      }
      tpeCases / (effsCases ++ effs)
  }

  //</editor-fold>


  //<editor-fold desc="statements and definitions">

  def checkStmt(expected: Option[Type])(given CompilerContext): Checker[Stmt] = checkAgainst(expected) {
    case source.DefStmt(b, rest) =>
      val (t / effBinding) = Compiler in { precheckDef(b); synthDef(b) }
      val (r / effStmt)    = checkStmt(expected)(rest)
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
  def precheckDef(d: Def)(given CompilerContext): Unit = Compiler.at(d) { d match {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      d.symbol.ret.foreach { annot => Compiler.putBlock(d.symbol, d.symbol.toType) }

    case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
      Compiler.putBlock(d.symbol, d.symbol.toType)

    case d @ source.EffDef(id, tparams, params, ret) =>
      d.symbol.ops.foreach { op => Compiler.putBlock(op, op.toType) }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.symbol
        Compiler.putBlock(sym, sym.toType)
      }

    case d => ()
  }}


  def synthDef(given CompilerContext): Checker[Def] = checking {
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      val sym = d.symbol
      Compiler.define(sym.params)
      sym.ret match {
        case Some(tpe / funEffs) =>
          val (_ / effs) = body checkAgainst tpe
          Compiler.wellscoped(effs)
          tpe / (effs -- funEffs) // the declared effects are considered as bound
        case None =>
          val (tpe / effs) = checkStmt(None)(body)
          Compiler.wellscoped(effs) // check they are in scope
          Compiler.putBlock(sym, sym.toType(tpe / effs))
          tpe / Pure // all effects are handled by the function itself (since they are inferred)
      }

    case d @ source.EffDef(id, tps, ps, ret) =>
      Compiler.withEffect(d.symbol)
      TUnit / Pure

    case d @ source.ValDef(id, annot, binding) =>
      val (t / effBinding) = d.symbol.tpe match {
        case Some(t) => binding checkAgainst t
        case None    => checkStmt(None)(binding)
      }
      Compiler.define(d.symbol, t)
      t / effBinding

    case d @ source.VarDef(id, annot, binding) =>
      val (t / effBinding) = d.symbol.tpe match {
        case Some(t) => binding checkAgainst t
        case None    => checkStmt(None)(binding)
      }
      Compiler.define(d.symbol, t)
      t / effBinding

    // all other defintions have already been prechecked
    case d => TUnit / Pure
  }

  //</editor-fold>


  //<editor-fold desc="arguments and parameters">

  // TODO we can remove this duplication, once every phase can write to every table.
  // then the namer phase can already store the resolved type symbol for the param.

  def resolveValueType(tpe: source.ValueType)(given CompilerContext): ValueType = tpe match {
    case t @ source.TypeApp(id, args) => TypeApp(t.definition, args.map(resolveValueType))
    case t @ source.TypeVar(id) => t.definition
  }

  /**
   * Invariant: Only call this on declarations that are fully annotated
   */
  def extractAllTypes(params: Params)(given CompilerContext): Sections = params map extractTypes

  def extractTypes(params: List[ValueParam] | BlockParam)(given CompilerContext): List[ValueType] | BlockType = params match {
    case BlockParam(_, tpe) => tpe
    case ps: List[ValueParam] => ps map {
      case ValueParam(_, Some(tpe)) => tpe
      case _ => Compiler.abort("Cannot extract type")
    }
  }

  /**
   * Returns the binders that will be introduced to check the corresponding body
   */
  def checkAgainstDeclaration(
    name: String,
    atCallee: List[List[ValueType] | BlockType],
    // we ask for the source Params here, since it might not be annotated
    atCaller: List[source.ParamSection])(given CompilerContext): Map[Symbol, Type] = {

    if (atCallee.size != atCaller.size)
      Compiler.error(s"Wrong number of argument sections, given ${atCaller.size}, but ${name} expects ${atCallee.size}.")

    // TODO add blockparams here!
    (atCallee zip atCaller).flatMap[(Symbol, Type)] {
      case (b1: BlockType, b2: source.BlockParam) =>
        Compiler.abort("not yet supported")
        ???

      case (ps1: List[ValueType], source.ValueParams(ps2)) =>
        (ps1 zip ps2).map[(Symbol, Type)] {
          case (decl, p @ source.ValueParam(id, annot)) =>
            val annotType = annot.map(resolveValueType)
            annotType.foreach { t => decl =!= t }
            (p.symbol, annotType.getOrElse(decl)) // use the annotation, if present.
        }.toMap
    }.toMap
  }


  def checkCall(expected: Option[Type])(sym: Symbol, targs: List[ValueType], args: List[source.ArgSection])(given CompilerContext): Effectful = {

    val BlockType(tparams, params, ret / retEffs) = Compiler.blockType(sym)

    if (targs.nonEmpty && targs.size != tparams.size)
      Compiler.abort(s"Wrong number of type arguments ${targs.size}")

    var unifier: Unifier = Unifier(tparams, if (targs.nonEmpty) { (tparams zip targs).toMap } else { Map.empty })

    expected.foreach { exp => unifier = unifier.merge(ret, exp) }

    var effs = retEffs

    if (params.size != args.size)
      Compiler.error(s"Wrong number of argument sections, given ${args.size}, but ${sym.name} expects ${params.size}.")

    // TODO we can improve the error positions here
    (params zip args) foreach {
      case (ps : List[ValueType], source.ValueArgs(as)) =>
        if (ps.size != as.size)
          Compiler.error(s"Wrong number of arguments. Argument section of ${sym.name} requires ${ps.size}, but ${as.size} given.")

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

        Compiler.define(bindings)

        val (tpe1 / handled) = blockType.ret
        val (tpe2 / stmtEffs)  = checkStmt(None)(stmt)

        unifier = unifier.merge(tpe1, tpe2)
        effs = (effs ++ (stmtEffs -- handled))

      case (_, _) =>
        Compiler.error("Wrong type of argument section")
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
  type Checker[T <: Tree] = T => Effectful

  def checkAgainst[T <: Tree](expected: Option[Type])(f: Checker[T])(given CompilerContext): Checker[T] = t => {
    Compiler.at(t) {
      val (got / effs) = f(t)
      expected map { got =!= _ }
      Compiler.annotate(t, got / effs)
      got / effs
    }
  }

  def checking[T <: Tree](f: Checker[T])(given CompilerContext): Checker[T] = t => {
    Compiler.at(t) {
      f(t)
    }
  }


  trait TyperOps(given C: CompilerContext) {

    // State Access
    // ============
    def (C: CompilerContext) effects: Effects =
      C.phases.get(typer).effects

    def (C: CompilerContext) withEffect(e: Effect): CompilerContext =
      C.phases.update(typer) { state => state.copy(effects = state.effects + e) }

    def (got: Type) =!= (expected: Type): Unit = (got, expected) match {
      case (TypeApp(c1, args1), TypeApp(c2, args2)) if c1 == c2 =>
        (args1 zip args2) foreach { _ =!= _ }
      case (t1, t2) => if (t1 != t2) {
        C.error(s"Expected $expected, but got $got")
      }
    }

    def (a: Effects) <:< (b: Effects): Effects = {
      val forbidden = a -- b
      if (forbidden.nonEmpty) {
        C.error(s"Effects ${forbidden} leave their defining scope.")
        b
      } else {
        b
      }
    }

    def (C: CompilerContext) wellscoped(a: Effects) = {
      val forbidden = Effects(a.effs.filterNot { e => e.builtin }) -- C.effects
      if (forbidden.nonEmpty) {
        C.error(s"Effects ${forbidden} leave their defining scope.")
      }
    }

    def (C: CompilerContext) define(s: Symbol, t: ValueType) = {
      C.putValue(s, t); C
    }

    def (C: CompilerContext) define(s: Symbol, t: BlockType) = {
      C.putBlock(s, t); C
    }

    def (C: CompilerContext) define(bs: Map[Symbol, Type]): CompilerContext = bs.foldLeft(C) {
      case (C, (v: ValueSymbol, t: ValueType)) => C.define(v, t)
      case (C, (v: BlockSymbol, t: BlockType)) => C.define(v, t)
    }
    def (C: CompilerContext) define(ps: List[List[ValueParam] | BlockParam]): CompilerContext =
      C.define(ps.flatMap {
        case ps : List[ValueParam] => ps map {
          case s @ ValueParam(name, Some(tpe)) => s -> tpe
          case s @ ValueParam(name, None) => ??? // non annotated handler, or block param
        }
        case s @ BlockParam(name, tpe) => List(s -> tpe)
      }.toMap)


    // Extension methods to improve readability of Typer
    // =================
    def (tpe: ValueType) / (effs: Effects): Effectful = Effectful(tpe, effs)
    def (expr: Expr) checkAgainst (tpe: Type): Effectful = checkExpr(Some(tpe))(expr)
    def (stmt: Stmt) checkAgainst (tpe: Type): Effectful = checkStmt(Some(tpe))(stmt)
  }

  def Compiler(given c: CompilerContext): CompilerContext = c
}