package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Context, ContextOps }
import effekt.symbols._

case class TransformerState(
  /**
   * Synthesized state effects for var-definitions
   */
  stateEffects: Map[VarBinder, StateCapability] = Map.empty,

  /**
   * Used to map each lexically scoped capability to its termsymbol
   */
  capabilities: Map[Effect, symbols.Capability] = Map.empty,

  /**
   * A _mutable_ ListBuffer that stores all bindings to be inserted at the current scope
   */
  bindings: ListBuffer[(Tmp, symbols.ValueType, Stmt)] = ListBuffer()
)

class Transformer extends Phase[Module, core.ModuleDecl] {

  def run(mod: Module)(implicit C: Context): Option[ModuleDecl] = Context in {
    // initialize transformer state
    C.transformerState = TransformerState()
    Some(transform(mod))
  }

  def transform(mod: Module)(implicit C: Context): ModuleDecl = {
    val source.ModuleDecl(path, imports, defs) = mod.decl
    val exports: Stmt = Exports(path, mod.terms.flatMap {
      case (name, syms) => syms.collect {
        // TODO export valuebinders properly
        case sym: Fun if !sym.isInstanceOf[EffectOp] && !sym.isInstanceOf[Field] => sym
        case sym: ValBinder => sym
      }
    }.toList)

    val transformed = defs.foldRight(exports) {
      case (d, r) => transform(d, r)
    }

    ModuleDecl(path, imports.map { _.path }, transformed).inheritPosition(mod.decl)
  }

  /**
   * the "rest" is a thunk so that traversal of statements takes place in the correct order.
   */
  def transform(d: source.Def, rest: => Stmt)(implicit C: Context): Stmt = withPosition(d) {
    case f @ source.FunDef(id, _, params, _, body) =>
      val sym = f.symbol

      val effs = sym.effects.userEffects

      C.bindingCapabilities(effs) { caps =>
        val ps = transformParams(params)
        // TODO also change the annotated type to include the added capabilities!
        Def(sym, C.blockTypeOf(sym), BlockLit(ps ++ caps, transform(body)), rest)
      }

    case d @ source.DataDef(id, _, ctors) =>
      Data(d.symbol, ctors.map { c => c.symbol }, rest)

    case d @ source.RecordDef(id, _, _) =>
      val rec = d.symbol
      core.Record(rec, rec.fields, rest)

    case v @ source.ValDef(id, _, binding) =>
      Val(v.symbol, transform(binding), rest)

    case v @ source.VarDef(id, _, binding) =>
      val sym = v.symbol
      val eff = C.state(sym)
      val b = transform(binding)
      val res = C.bindingCapabilities(List(eff.effect)) { caps =>
        State(eff.effect, C.valueTypeOf(sym), eff.get, eff.put, b, BlockLit(caps, rest))
      }
      res

    case source.ExternType(id, tparams) =>
      rest

    case source.TypeDef(id, tparams, tpe) =>
      rest

    case source.EffectDef(id, effs) =>
      rest

    case f @ source.ExternFun(pure, id, tparams, params, ret, body) =>
      val sym = f.symbol
      // C&P from FunDef
      // usually extern funs do not have userdefined capabilities
      if (sym.effects.userDefined.nonEmpty) {
        C.abort("User defined effects on extern defs not allowed")
      }
      val ps = transformParams(params)
      Def(sym, C.blockTypeOf(sym), Extern(ps, body), rest)

    case e @ source.ExternInclude(path) =>
      Include(e.contents, rest)

    case e: source.ExternEffect =>
      rest

    case d @ source.EffDef(id, ops) =>
      core.Record(d.symbol, ops.map { e => e.symbol }, rest)
  }

  def transform(tree: source.Stmt)(implicit C: Context): Stmt = withPosition(tree) {
    case source.DefStmt(d, rest) =>
      transform(d, transform(rest))

    // { e; stmt } --> { val _ = e; stmt }
    case source.ExprStmt(e, rest) =>
      Val(freshWildcardFor(e), insertBindings { Ret(transform(e)) }, transform(rest))

    case source.Return(e) =>
      insertBindings { Ret(transform(e)) }

    case source.BlockStmt(b) =>
      transform(b)
  }

  def transformLit[T](tree: source.Literal[T])(implicit C: Context): Literal[T] = withPosition(tree) {
    case source.UnitLit()         => UnitLit()
    case source.IntLit(value)     => IntLit(value)
    case source.BooleanLit(value) => BooleanLit(value)
    case source.DoubleLit(value)  => DoubleLit(value)
    case source.StringLit(value)  => StringLit(value)
  }

  def transform(tree: source.Expr)(implicit C: Context): Expr = withPosition(tree) {
    case v: source.Var => v.definition match {
      case sym: VarBinder =>
        val state = C.state(sym)
        val get = App(Member(C.resolveCapability(state.effect), state.get), Nil, Nil)
        C.bind(C.valueTypeOf(sym), get)
      case sym => ValueVar(sym)
    }

    case a @ source.Assign(id, expr) =>
      val e = transform(expr)
      val state = C.state(a.definition)
      val put = App(Member(C.resolveCapability(state.effect), state.put), Nil, List(e))
      C.bind(builtins.TUnit, put)

    case l: source.Literal[t] => transformLit(l)

    case source.If(cond, thn, els) =>
      val c = transform(cond)
      val exprTpe = C.inferredTypeOf(tree).tpe
      C.bind(exprTpe, If(c, transform(thn), transform(els)))

    case source.While(cond, body) =>
      val exprTpe = C.inferredTypeOf(tree).tpe
      C.bind(exprTpe, While(insertBindings { Ret(transform(cond)) }, transform(body)))

    case source.MatchExpr(sc, clauses) =>
      val scrutinee = transform(sc)

      val cs: List[(Pattern, BlockLit)] = clauses.map {
        case cl @ source.MatchClause(pattern, body) =>
          val (p, ps) = transform(pattern)
          (p, BlockLit(ps, transform(body)))
      }
      C.bind(C.inferredTypeOf(tree).tpe, Match(scrutinee, cs))

    // assumption: typer removed all ambiguous references, so there is exactly one
    case c @ source.MethodCall(block, op, _, args) => ???
    case c @ source.Call(fun, _, args) =>
      val sym: Symbol = c.definition

      // we do not want to provide capabilities for the effect itself
      val ownEffect = sym match {
        case e: EffectOp => Effects(List(e.effect))
        case _           => symbols.Pure
      }

      val BlockType(tparams, params, ret / effs) = C.blockTypeOf(sym)

      // Do not provide capabilities for builtin effects and also
      // omit the capability for the effect itself (if it is an effect operation
      val effects = (effs -- ownEffect).userDefined
      val capabilityArgs = effects.toList.map { C.resolveCapability }

      val as: List[Argument] = (args zip params).flatMap {
        case (source.ValueArgs(as), _) =>
          as.map(transform)
        case (source.BlockArg(params, body), List(p: BlockType)) =>
          val ps = transformParams(params)
          C.bindingCapabilities(p.ret.effects.userEffects) { caps =>
            List(BlockLit(ps ++ caps, transform(body)))
          }
      }

      // the type arguments, inferred by typer
      val targs = C.typeArguments(c)

      // right now only builtin functions are pure of control effects
      // later we can have effect inference to learn which ones are pure.
      sym match {
        case f: BuiltinFunction if f.pure =>
          PureApp(BlockVar(f), targs, as ++ capabilityArgs)
        case r: Record =>
          PureApp(BlockVar(r), targs, as ++ capabilityArgs)
        case f: EffectOp =>
          C.bind(C.inferredTypeOf(tree).tpe, App(Member(C.resolveCapability(f.effect), f), targs, as ++ capabilityArgs))
        case f: Field =>
          val List(arg: Expr) = as
          Select(arg, f)
        case f: BlockSymbol =>
          C.bind(C.inferredTypeOf(tree).tpe, App(BlockVar(f), targs, as ++ capabilityArgs))
      }

    case source.TryHandle(prog, handlers) =>

      val effects = handlers.map(_.definition)
      val body = C.bindingCapabilities(effects) { caps =>
        BlockLit(caps, transform(prog))
      }

      // to obtain a canonical ordering of operation clauses, we use the definition ordering
      val hs = handlers.map {
        case h @ source.Handler(eff, cap, cls) =>
          val clauses = cls.map { cl => (cl.definition, cl) }.toMap

          Handler(h.definition, h.definition.ops.map(clauses.apply).map {
            case op @ source.OpClause(id, params, body, resume) =>
              val ps = transformParams(params)
              val opBlock = BlockLit(ps :+ BlockParam(resume.symbol.asInstanceOf[BlockSymbol]), transform(body))
              (op.definition, opBlock)
          })
      }

      C.bind(C.inferredTypeOf(tree).tpe, Handle(body, hs))

    case source.Hole(stmts) =>
      C.bind(C.inferredTypeOf(tree).tpe, Hole)

  }

  def transformParams(ps: List[source.ParamSection])(implicit C: Context): List[core.Param] =
    ps.flatMap {
      case b @ source.BlockParam(id, _)      => List(BlockParam(b.symbol))
      case b @ source.CapabilityParam(id, _) => List(BlockParam(b.symbol))
      case v @ source.ValueParams(ps)        => ps.map { p => ValueParam(p.symbol) }
    }

  def transform(tree: source.MatchPattern)(implicit C: Context): (Pattern, List[core.ValueParam]) = tree match {
    case source.IgnorePattern()    => (core.IgnorePattern(), Nil)
    case source.LiteralPattern(l)  => (core.LiteralPattern(transformLit(l)), Nil)
    case p @ source.AnyPattern(id) => (core.AnyPattern(), List(ValueParam(p.symbol)))
    case p @ source.TagPattern(id, ps) =>
      val (patterns, params) = ps.map(transform).unzip
      (core.TagPattern(p.definition, patterns), params.flatten)
  }

  def transform(exprs: List[source.Expr])(implicit C: Context): List[Expr] =
    exprs.map(transform)

  def freshWildcardFor(e: source.Tree)(implicit C: Context): Wildcard = {
    val x = Wildcard(C.module)
    C.inferredTypeOption(e) match {
      case Some(t / _) => C.assignType(x, t)
      case _           => C.abort("Internal Error: Missing type of source expression.")
    }
    x
  }

  def withPosition[T <: source.Tree, R <: core.Tree](t: T)(block: T => R)(implicit C: Context): R =
    block(t).inheritPosition(t)

  def insertBindings(stmt: => Stmt)(implicit C: Context): Stmt = Context in {
    val bindings = ListBuffer.empty[(Tmp, symbols.ValueType, Stmt)]
    C.transformerState = C.transformerState.copy(bindings = bindings)
    val body = stmt

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
}
trait TransformerOps extends ContextOps { Context: Context =>

  /**
   * The state of the transformer phase
   */
  private[core] var transformerState: TransformerState = _

  /**
   * Override the dynamically scoped `in` to also reset transformer state
   */
  override def in[T](block: => T): T = {
    val before = transformerState
    val result = super.in(block)
    transformerState = before
    result
  }

  private[core] def state(binder: VarBinder): StateCapability = {
    val stateEffects = transformerState.stateEffects
    stateEffects.get(binder) match {
      case Some(v) => v
      case None =>
        val cap = StateCapability(binder)
        // TODO move to TransformerState
        transformerState = transformerState.copy(stateEffects = stateEffects + (binder -> cap))
        cap
    }
  }

  /**
   * runs the given block, binding the provided capabilities, so that
   * "resolveCapability" will find them.
   */
  private[core] def bindingCapabilities[R](effs: List[UserEffect])(block: List[core.BlockParam] => R): R = {
    val before = transformerState.capabilities;
    // create a fresh cabability-symbol for each bound effect
    val caps = effs.map { eff => CapabilityParam(eff.name, CapabilityType(eff)) }
    // additional block parameters for capabilities
    // TODO we need to come up with a block type for capabilities here!
    //      however, blocks right now don't admit selection and are specialized
    //      to one observation, only.
    val params = caps.map { sym => core.BlockParam(sym, sym.tpe) }

    Context in {
      // update state with capabilities
      transformerState = transformerState.copy(
        capabilities = before ++ caps.map { c => (c.effect -> c) }.toMap
      )
      // run block
      block(params)
    }
  }

  private[core] def resolveCapability(e: Effect): core.BlockVar =
    transformerState.capabilities.get(e).map { core.BlockVar }.getOrElse(
      Context.abort(s"Compiler error: cannot find capability for ${e}")
    )

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
    transformerState.bindings += binding

    ValueVar(x)
  }
}
