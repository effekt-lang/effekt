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

class Typer(val symbols: Memoiser[Id, Symbol]) {

  given Assertions

  /**
   * Output: the types we inferred
   */
  lazy val types = Memoiser.makeIdMemoiser[Fun, Effectful]()

  def run(module: source.ModuleDecl, env: Environment, buffer: MessageBuffer): Unit = {
    val toplevelEffects = Effects(List(EDivZero, EConsole) ++ env.types.values.collect { case e: Effect => e })
    Context(module, buffer, toplevelEffects) in {
      module.defs foreach { d => synthToplevelDef(d) }
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
      val (_ / eff) = expr checkAgainst Context.get(id.symbol)
      TUnit / eff

    case source.Call(fun, targs, args) =>
      val targsResolved = targs map { resolveValueType }
      fun.symbol.asFun match {
        case f @ Fun(name, tparams, params, tpe) =>
          val (retType / retEffs) = f.returnType
          val (effs, unifier) = checkCall(expected)(f, tparams, params.map(extractTypes), f.returnType.tpe)(targsResolved, args)
          (unifier substitute retType) / (retEffs ++ effs)
      }

    case source.Do(op, targs, args) =>
      val targsResolved = targs map { resolveValueType }
      op.symbol.asEffectOp match {
        case f @ Fun(name, tparams, params, tpe) =>
          val (retType / retEffs) = f.returnType
          val (effs, unifier) = checkCall(expected)(f, tparams, params.map(extractTypes), f.returnType.tpe)(targsResolved, List(args))
          (unifier substitute retType) / (retEffs ++ effs)
      }

    case source.Yield(block, as) =>
      val blockTpe = block.symbol.asBlockParam.tpe
      val (ret / blockEffs) = blockTpe.ret
      val (argEffs, unifier) = checkCall(expected)(block.symbol, Nil, List(blockTpe.params), ret)(Nil, List(as))
      (unifier substitute ret) / (blockEffs ++ argEffs)

    // the same as for Yield
    case source.Resume(as) =>
      val blockTpe = Context.get(ResumeParam).asBlockType
      val (ret / blockEffs) = blockTpe.ret
      val (argEffs, unifier) = checkCall(expected)(ResumeParam, Nil, List(blockTpe.params), ret)(Nil, List(as))
      (unifier substitute ret) / (blockEffs ++ argEffs)

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
        case source.Clause(op, params, body) =>
          val effectOp = op.symbol.asEffectOp
          val effect = effectOp.effect
          // TODO right now effect ops only have one argument section
          val ps = checkAgainstDeclaration(op.name, params.params.allSymbols, extractTypes(effectOp.params.head))
          Context.define(ps).define(ResumeParam, BlockType(List(effectOp.ret.get.tpe), ret / Pure)) in {
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
          val pms = extractTypes(sym.params.head).map { u substitute _ }

          val ps = checkAgainstDeclaration(id.name, params.params.allSymbols, pms)
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
    case source.DefStmt(d @ source.FunDef(id, tparams, params, ret, body), rest) =>

      val sym = id.symbol.asUserFunction

      // does not return all the effects the function has, but only the effects
      // that are *not* annotated on the function
      val (t / effBinding) = Context.define(sym.params) in {
        sym.ret match {
          case Some(annot @ (tpe / funEffs)) =>
            types.put(sym, annot) // to type recursive functions
            val (_ / effs) = body checkAgainst tpe
            effs <:< Context.effects
            tpe / (effs -- funEffs) // the declared effects are considered as bound

          case None =>
            // TODO check whether the inferred effects are in the lexical scope
            val (tpe / effs) = checkStmt(None)(body)
            effs <:< Context.effects

            types.put(sym, tpe / effs)
            tpe / Pure // all effects are handled by the function itself (since they are inferred)
        }
      }
      val (r / effStmt) = Context.define(sym, t) in { checkStmt(expected)(rest) }
      r / (effBinding ++ effStmt)

    case source.DefStmt(source.EffDef(id, tps, ps, ret), rest) =>
      Context.withEffect(id.symbol.asEffect) in {
        checkStmt(expected)(rest)
      }

    case source.DefStmt(source.DataDef(id, tparams, ctors), rest) =>
      checkStmt(expected)(rest)

    case source.DefStmt(source.ExternType(id, tparams), rest) =>
      checkStmt(expected)(rest)

    case source.DefStmt(b, rest) =>
      val (t / effBinding) = synthBinderDef(b)
      val (r / effStmt)    = Context.define(b.id.symbol, t) in { checkStmt(expected)(rest) }
      r / (effBinding ++ effStmt)

    // <expr> ; <stmt>
    case source.ExprStmt(e, rest) =>
      val (_ / eff1) = checkExpr(None)(e)
      val (r / eff2) = checkStmt(expected)(rest)
      r / (eff1 ++ eff2)

    case source.Return(e) => checkExpr(expected)(e)
  }

  def synthBinderDef: Checker[Def] = checking {
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
  }

  def synthToplevelDef: Checker[Def] = checking {
    // annotate the inferred return type to the function node, so it can be
    // looked up at the call-site
    case d @ source.FunDef(id, tparams, params, ret, body) =>
      val sym = id.symbol.asUserFunction

      Context.define(sym.params) in {
        sym.ret match {
          case Some(annot @ (tpe / funEffs)) =>
            types.put(sym, annot) // to type recursive functions
            val (_ / effs) = body checkAgainst tpe
            effs <:< funEffs // this is too restrictive - it rules out local functions that close over capabilities
            effs <:< Context.effects
            annot
          case None =>
            val (tpe / effs) = checkStmt(None)(body)
            effs <:< Context.effects
            types.put(sym, tpe / effs)
            (tpe / Pure)
        }
      }

    case source.EffDef(id, tparams, params, ret) => TUnit / Pure

    case source.DataDef(id, tparams, ctors) => TUnit / Pure

    case source.ExternType(id, tparams) => TUnit / Pure

    case source.ExternEffect(id, tparams) => TUnit / Pure

    case source.ExternInclude(path) => TUnit / Pure

    case source.ExternFun(pure, id, tparams, params, tpe, body) =>
      id.symbol.asFun.ret.get
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
  def extractTypes(params: List[ValueParam] | BlockParam)(given Context): List[Type] = params match {
    case BlockParam(_, tpe) => List(tpe)
    case ps: List[ValueParam] => ps map {
      case ValueParam(_, Some(tpe)) => tpe
      case _ => Context.abort("Cannot extract type")
    }
  }

  /**
   * Returns the binders that will be introduced to check the corresponding body
   */
  def checkAgainstDeclaration(name: String, annotated: List[ValueParam], declared: List[Type])(given Context): Map[Symbol, Type] = {
    if (annotated.size != declared.size)
      Context.error(s"Wrong number of arguments, given ${annotated.size}, but ${name} expects ${declared.size}.")

    (declared zip annotated).map[(Symbol, Type)] {
      case (decl, p @ ValueParam(_, annot)) =>
        annot.foreach { t => decl =!= t }
        (p, annot.getOrElse(decl)) // use the annotation, if present.
    }.toMap
  }

  def checkCall(expected: Option[Type])(sym: Symbol, tparams: List[TypeVar], params: List[List[Type]], ret: Type)(targs: List[ValueType], args: List[source.ArgSection])(given Context): (Effects, Unifier) = {

    if (targs.nonEmpty && targs.size != tparams.size)
      Context.abort(s"Wrong number of type arguments ${targs.size}")

    var unifier: Unifier = Unifier(tparams, if (targs.nonEmpty) { (tparams zip targs).toMap } else { Map.empty })

    expected.foreach { exp => unifier = unifier.merge(ret, exp) }

    var effs = Pure

    if (params.size != args.size)
      Context.error(s"Wrong number of argument sections, given ${args.size}, but ${sym.name} expects ${params.size}.")

    // TODO we can improve the error positions here
    (params zip args) foreach {
      case (ps, source.ValueArgs(as)) =>
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
      case (List(bt: BlockType), source.BlockArg(params, stmt)) =>

        val blockType = unifier substitute bt
        val bindings = checkAgainstDeclaration("block", params.allSymbols, blockType.params)

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

    (effs, unifier)
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

    def (id: Id) symbol: Symbol = symbols(id)

    // looks up the symbols for a list of source value parameters
    def (ps: List[source.ValueParam]) allSymbols: List[ValueParam] =
      ps.map(_.id.symbol.asValueParam)
  }
  given TyperOps

  // this requires splitting in context related and Ops-related methods
  // define one XXXAssertions for every phase that requires being mixed with ErrorReporter
  case class Context(
    focus: Tree,                             // current type checking position
    buffer: MessageBuffer,
    effects: Effects = Pure,                 // the effects, whose declarations are lexically in scope (i.e. a conservative approximation of possible capabilities
    bindings: Map[Symbol, Type] = Map.empty // the types of variables in the current environment
  ) extends TyperAssertions {

    // TODO does this correctly compare List[Int] with List[Int]?
    def (got: Type) =!= (expected: Type): Unit = (got, expected) match {
      case (TypeApp(c1, args1), TypeApp(c2, args2)) if c1 == c2 =>
        (args1 zip args2) foreach { _ =!= _ }
      case (t1, t2) => if (t1 != t2) {
        error(s"Expected $expected, but got $got")
      }
    }

    //  def get(id: Id): Type = bindings(id.symbol)
    def get(sym: Symbol): Type = bindings(sym)
    def getValueType(sym: Symbol): ValueType = bindings(sym).asValueType

    def define(bs: Map[Symbol, Type]) = copy(bindings = bindings ++ bs)
    def define(ps: List[List[ValueParam] | BlockParam]) = copy(bindings = bindings ++ ps.flatMap {
      case ps : List[ValueParam] => ps map {
        case s @ ValueParam(name, Some(tpe)) => s -> tpe
        case s @ ValueParam(name, None) => ??? // non annotated handler, or block param
      }
      case s @ BlockParam(name, tpe) => List(s -> tpe)
    })
    def define(s: Symbol, t: Type) = copy(bindings = bindings + (s -> t))
    def at(node: Tree): Context = copy(focus = node)

    def withEffect(e: Effect): Context = this.copy(effects = effects + e)

    def current: Context = this

    // always first look at the annotated type, then look it up in the dictionary
    def (f: Fun) returnType: Effectful = f.ret match {
      case Some(t) => t
      case None => types.getOrDefault(f,
        abort(s"Result type of recursive function ${f.name} needs to be annotated"))
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