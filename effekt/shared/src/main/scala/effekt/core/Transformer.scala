package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Context, ContextOps }
import effekt.symbols._
import effekt.context.assertions._
import effekt.regions.{ Region, RegionSet }
import effekt.source.ExternFlag

object Transformer extends Phase[Typechecked, CoreTransformed] {

  val phaseName = "transformer"

  def run(input: Typechecked)(implicit C: Context) =
    val Typechecked(source, tree, mod) = input
    C.initTransformerState()
    Some(CoreTransformed(source, tree, mod, transform(mod, tree)))

  def transform(mod: Module, tree: source.ModuleDecl)(implicit C: Context): ModuleDecl = {
    val source.ModuleDecl(path, imports, defs) = tree
    val exports = mod.terms.flatMap {
      case (name, syms) => syms.collect {
        // TODO export valuebinders properly
        case sym: Fun if !sym.isInstanceOf[EffectOp] && !sym.isInstanceOf[Field] => sym
        case sym: ValBinder => sym
      }
    }.toList

    val transformed = defs.foldRight(Ret(UnitLit()) : Stmt) {
      case (d, r) => transform(d, r)
    }

    val optimized = optimize(transformed)

    ModuleDecl(path, imports.map { _.path }, optimized, exports).inheritPosition(tree)
  }

  /**
   * the "rest" is a thunk so that traversal of statements takes place in the correct order.
   */
  def transform(d: source.Def, rest: => Stmt)(implicit C: Context): Stmt = withPosition(d) {
    case f @ source.FunDef(id, _, vps, bps, _, body) =>
      val sym = f.symbol
      val ps = (vps map transform) ++ (bps map transform)
      Def(sym, C.blockTypeOf(sym), BlockLit(ps, transform(body)), rest)

    case d @ source.DataDef(id, _, ctors) =>
      Data(d.symbol, ctors.map { c => c.symbol }, rest)

    case d @ source.RecordDef(id, _, _) =>
      val rec = d.symbol
      core.Record(rec, rec.fields, rest)

    case v @ source.ValDef(id, _, binding) if C.pureOrIO(binding) =>
      Let(v.symbol, Run(transform(binding)), rest)

    case v @ source.ValDef(id, _, binding) =>
      Val(v.symbol, transform(binding), rest)

    // This phase introduces capabilities for state effects
    case v @ source.VarDef(id, _, binding) =>
      val sym = v.symbol
      val state = C.state(sym)
      val b = transform(binding)
      val params = List(core.BlockParam(state.param, state.param.tpe))
      State(state.effect, C.valueTypeOf(sym), state.get, state.put, b, BlockLit(params, rest))

    case source.ExternType(id, tparams) =>
      rest

    case source.TypeDef(id, tparams, tpe) =>
      rest

    case source.EffectDef(id, effs) =>
      rest

    case f @ source.ExternFun(pure, id, tps, vps, bps, ret, body) =>
      val sym = f.symbol
      Def(f.symbol, C.functionTypeOf(sym), Extern((vps map transform) ++ (bps map transform), body), rest)

    case e @ source.ExternInclude(path) =>
      Include(e.contents, rest)

    case e: source.ExternEffect =>
      rest

    case d @ source.EffDef(id, tparams, ops) =>
      core.Record(d.symbol, ops.map { e => e.symbol }, rest)
  }

  def transform(tree: source.Stmt)(implicit C: Context): Stmt = withPosition(tree) {
    case source.DefStmt(d, rest) =>
      transform(d, transform(rest))

    // { e; stmt } --> { val _ = e; stmt }
    case source.ExprStmt(e, rest) if C.pureOrIO(e) =>
      val (expr, bs) = C.withBindings { transform(e) }
      val let = Let(freshWildcardFor(e), expr, transform(rest))
      if (bs.isEmpty) { let }
      else { reifyBindings(let, bs) }

    case source.ExprStmt(e, rest) =>
      Val(freshWildcardFor(e), insertBindings { Ret(transform(e)) }, transform(rest))

    case source.Return(e) =>
      insertBindings { Ret(transform(e)) }

    case source.BlockStmt(b) =>
      transform(b)
  }

  def transformLit[T](tree: source.Literal[T])(implicit C: Context): Literal[T] = withPosition[source.Literal[T], Literal[T]](tree) {
    case source.UnitLit()         => UnitLit()
    case source.IntLit(value)     => IntLit(value)
    case source.BooleanLit(value) => BooleanLit(value)
    case source.DoubleLit(value)  => DoubleLit(value)
    case source.StringLit(value)  => StringLit(value)
  }

  def transform(tree: source.Term)(implicit C: Context): Expr = withPosition(tree) {
    case v: source.Var => v.definition match {
      case sym: VarBinder =>
        val state = C.state(sym)
        val get = App(Member(BlockVar(state.param), state.get), Nil, Nil)
        C.bind(C.valueTypeOf(sym), get)
      case sym => ValueVar(sym)
    }

    case a @ source.Assign(id, expr) =>
      val e = transform(expr)
      val state = C.state(a.definition)
      val put = App(Member(BlockVar(state.param), state.put), Nil, List(e))
      C.bind(builtins.TUnit, put)

    case l: source.Literal[t] => transformLit(l)

    case l @ source.Lambda(id, vps, bps, body) =>
      val tpe = C.functionTypeOf(l.symbol)
      Closure(BlockLit((vps map transform) ++ (bps map transform), transform(body)))

    case source.If(cond, thn, els) =>
      val c = transform(cond)
      val exprTpe = C.inferredTypeOf(tree)
      C.bind(exprTpe, If(c, transform(thn), transform(els)))

    case source.While(cond, body) =>
      val exprTpe = C.inferredTypeOf(tree)
      C.bind(exprTpe, While(insertBindings { Ret(transform(cond)) }, transform(body)))

    case source.Match(sc, clauses) =>
      val scrutinee = transform(sc)

      val cs: List[(Pattern, BlockLit)] = clauses.map {
        case cl @ source.MatchClause(pattern, body) =>
          val (p, ps) = transform(pattern)
          (p, BlockLit(ps, transform(body)))
      }
      C.bind(C.inferredTypeOf(tree), Match(scrutinee, cs))

    case c @ source.Call(source.ExprTarget(expr), targs, vargs, bargs) =>
      val e = transform(expr)
      val valueArgs = vargs.map(transform)
      val blockArgs = bargs.map(transform)
      C.bind(C.inferredTypeOf(tree), App(Unbox(e), Nil, valueArgs ++ blockArgs))

    case c @ source.Call(source.MemberTarget(block, op), _, vargs, bargs) =>
      // the type arguments, inferred by typer
      // val targs = C.typeArguments(c)
      val valueArgs = vargs.map(transform)
      val blockArgs = bargs.map(transform)
      val app = App(Member(BlockVar(block.symbol.asBlockSymbol), op.symbol.asEffectOp), null, valueArgs ++ blockArgs)
      C.bind(C.inferredTypeOf(tree), app)

    case c @ source.Call(fun: source.IdTarget, _, vargs, bargs) =>
      // assumption: typer removed all ambiguous references, so there is exactly one
      val sym: Symbol = fun.definition

      val as = vargs.map(transform) ++ bargs.map(transform)

      // the type arguments, inferred by typer
      val targs = C.typeArguments(c)

      sym match {
        case f: BuiltinFunction if ExternFlag.directStyle(f.purity) =>
          PureApp(BlockVar(f), targs, as)
        case r: Record =>
          PureApp(BlockVar(r), targs, as)
        case f: EffectOp =>
          C.panic("Should have been translated to a method call!")
        case f: Field =>
          val List(arg: Expr) = as
          Select(arg, f)
        case f: BlockSymbol if C.pureOrIO(f) && bargs.forall { C.pureOrIO } =>
          Run(App(BlockVar(f), targs, as))
        case f: BlockSymbol =>
          C.bind(C.inferredTypeOf(tree), App(BlockVar(f), targs, as))
        case f: ValueSymbol =>
          C.bind(C.inferredTypeOf(tree), App(Unbox(ValueVar(f)), targs, as))
      }

    case source.TryHandle(prog, handlers) =>

      val effects = handlers.map(_.definition)
      val caps = handlers.map { h =>
        val cap = h.capability.get.symbol
        core.BlockParam(cap, cap.tpe)
      }
      val body = BlockLit(caps, transform(prog))

      // to obtain a canonical ordering of operation clauses, we use the definition ordering
      val hs = handlers.map {
        case h @ source.Handler(eff, cap, cls) =>
          val clauses = cls.map { cl => (cl.definition, cl) }.toMap

          Handler(h.definition, h.definition.ops.map(clauses.apply).map {
            case op @ source.OpClause(id, vps, body, resume) =>
              val ps = vps map transform

              // introduce a block parameter for resume
              val resumeParam = BlockParam(resume.symbol.asInstanceOf[BlockSymbol])

              val opBlock = BlockLit(ps :+ resumeParam, transform(body))
              (op.definition, opBlock)
          })
      }

      C.bind(C.inferredTypeOf(tree), Handle(body, hs))

    case source.Hole(stmts) =>
      C.bind(C.inferredTypeOf(tree), Hole)

  }

  def transform(block: source.BlockArg)(implicit C: Context): core.Block = block match {
    case source.FunctionArg(tps, vps, bps, body) =>
      BlockLit((vps map transform) ++ (bps map transform), transform(body))
    case c @ source.InterfaceArg(id) => BlockVar(c.definition)
  }

  def transform(block: source.FunctionArg)(implicit C: Context): core.BlockLit = block match {
    case source.FunctionArg(tps, vps, bps, body) => BlockLit((vps map transform) ++ (bps map transform), transform(body))
  }

  def transform(p: source.BlockParam)(implicit C: Context): core.BlockParam = BlockParam(p.symbol)
  def transform(p: source.ValueParam)(implicit C: Context): core.ValueParam = ValueParam(p.symbol)

  def transform(tree: source.MatchPattern)(implicit C: Context): (Pattern, List[core.ValueParam]) = tree match {
    case source.IgnorePattern()    => (core.IgnorePattern(), Nil)
    case source.LiteralPattern(l)  => (core.LiteralPattern(transformLit(l)), Nil)
    case p @ source.AnyPattern(id) => (core.AnyPattern(), List(ValueParam(p.symbol)))
    case p @ source.TagPattern(id, ps) =>
      val (patterns, params) = ps.map(transform).unzip
      (core.TagPattern(p.definition, patterns), params.flatten)
  }

  def transform(exprs: List[source.Term])(implicit C: Context): List[Expr] =
    exprs.map(transform)

  def freshWildcardFor(e: source.Tree)(implicit C: Context): Wildcard = {
    val x = Wildcard(C.module)
    C.inferredTypeOption(e) match {
      case Some(t) => C.assignType(x, t)
      case _           => C.abort("Internal Error: Missing type of source expression.")
    }
    x
  }

  def withPosition[T <: source.Tree, R <: core.Tree](t: T)(block: T => R)(implicit C: Context): R =
    block(t).inheritPosition(t)

  def insertBindings(stmt: => Stmt)(implicit C: Context): Stmt = {
    val (body, bindings) = C.withBindings { stmt }
    reifyBindings(body, bindings)
  }

  def reifyBindings(body: Stmt, bindings: ListBuffer[(Tmp, symbols.ValueType, Stmt)])(implicit C: Context): Stmt = {
    bindings.foldRight(body) {
      // optimization: remove unnecessary binds
      case ((x, tpe, b), Ret(ValueVar(y))) if x == y => b
      case ((x, tpe, b), body) => Val(x, b, body)
    }
  }

  // Helpers to constructed typed trees
  def ValueParam(id: ValueSymbol)(implicit C: Context): core.ValueParam =
    core.ValueParam(id, C.valueTypeOf(id))

  def BlockParam(id: BlockSymbol)(implicit C: Context): core.BlockParam =
    core.BlockParam(id, C.blockTypeOf(id))

  def Val(id: ValueSymbol, binding: Stmt, body: Stmt)(implicit C: Context): core.Val =
    core.Val(id, C.valueTypeOf(id), binding, body)

  def Let(id: ValueSymbol, binding: Expr, body: Stmt)(implicit C: Context): core.Let =
    core.Let(id, C.valueTypeOf(id), binding, body)

  def optimize(s: Stmt)(implicit C: Context): Stmt = {

    // a very small and easy post processing step...
    // reduces run-return pairs
    object eliminateReturnRun extends core.Tree.Rewrite {
      override def expr = {
        case core.Run(core.Ret(e)) => rewrite(e)
      }
      override def stmt = {
        case core.Ret(core.Run(s)) => rewrite(s)
      }
    }
    eliminateReturnRun.rewrite(s)
  }
}
trait TransformerOps extends ContextOps { Context: Context =>

  case class StateCapability(param: BlockParam, effect: ControlEffect, get: EffectOp, put: EffectOp)

  private def StateCapability(binder: VarBinder)(implicit C: Context): StateCapability = {
    val tpe = C.valueTypeOf(binder)
    val eff = ControlEffect(binder.name, Nil)
    val get = EffectOp(binder.name.rename(name => "get"), Nil, Nil, tpe, Pure, eff)
    val put = EffectOp(binder.name.rename(name => "put"), Nil, List(ValueParam(binder.name, Some(tpe))), builtins.TUnit, Pure, eff)

    val param = BlockParam(binder.name, CapabilityType(eff))
    eff.ops = List(get, put)
    StateCapability(param, eff, get, put)
  }

  /**
   * Synthesized state effects for var-definitions
   */
  private var stateEffects: Map[VarBinder, StateCapability] = Map.empty

  /**
   * A _mutable_ ListBuffer that stores all bindings to be inserted at the current scope
   */
  private var bindings: ListBuffer[(Tmp, symbols.ValueType, Stmt)] = ListBuffer()

  private[core] def initTransformerState() = {
    stateEffects = Map.empty
    bindings = ListBuffer()
  }

  /**
   * Override the dynamically scoped `in` to also reset transformer state
   */
  override def in[T](block: => T): T = {
    val before = stateEffects
    val result = super.in(block)
    stateEffects = before
    result
  }

  private[core] def state(binder: VarBinder): StateCapability = {
    stateEffects.get(binder) match {
      case Some(v) => v
      case None =>
        val cap = StateCapability(binder)
        stateEffects = stateEffects + (binder -> cap)
        cap
    }
  }

  /**
   * Introduces a binding for the given statement.
   *
   * @param tpe the type of the bound statement
   * @param s the statement to be bound
   */
  private[core] def bind(tpe: symbols.ValueType, s: Stmt): Expr = {

    // create a fresh symbol and assign the type
    val x = Tmp(module)
    assignType(x, tpe)

    val binding = (x, tpe, s)
    bindings += binding

    ValueVar(x)
  }

  private[core] def withBindings[R](block: => R): (R, ListBuffer[(Tmp, symbols.ValueType, Stmt)]) = Context in {
    val before = bindings
    val b = ListBuffer.empty[(Tmp, symbols.ValueType, Stmt)]
    bindings = b
    val result = block
    bindings = before
    (result, b)
  }

  // we conservatively approximate to false
  def pureOrIO(t: source.Tree): Boolean = inferredRegionOption(t) match {
    case Some(reg) => pureOrIO(reg)
    case _         => false
  }

  def pureOrIO(t: BlockSymbol): Boolean = false //pureOrIO(regionOf(t).asRegionSet)

  def pureOrIO(r: RegionSet): Boolean = r.subsetOf(Region(builtins.IOCapability))
}
