package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Context, ContextOps }
import effekt.symbols._
import effekt.context.assertions.SymbolAssertions

class Transformer extends Phase[Module, core.ModuleDecl] {

  def run(mod: Module)(implicit C: Context): Option[ModuleDecl] = Context in {
    C.initTransformerState()
    Some(transform(mod))
  }

  def transform(mod: Module)(implicit C: Context): ModuleDecl = {
    // use capability passing transform
    val Some(modCPS) = C.capabilityPassing(mod.decl)

    val source.ModuleDecl(path, imports, defs) = modCPS
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
      val ps = transformParams(params)
      Def(sym, C.interfaceTypeOf(sym), BlockLit(ps, transform(body)), rest)

    case d @ source.DataDef(id, _, ctors) =>
      Data(d.symbol, ctors.map { c => c.symbol }, rest)

    case d @ source.RecordDef(id, _, _) =>
      val rec = d.symbol
      core.Record(rec, rec.fields, rest)

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

    case f @ source.ExternFun(pure, id, tparams, params, ret, body) =>
      val sym = f.symbol
      Def(f.symbol, C.blockTypeOf(sym), Extern(transformParams(params), body), rest)

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

    case c @ source.MethodCall(block, op, _, args) =>
      // the type arguments, inferred by typer
      // val targs = C.typeArguments(c)

      val app = App(Member(BlockVar(block.symbol.asBlockSymbol), op.symbol.asEffectOp), null, args.flatMap(transform))
      C.bind(C.inferredTypeOf(tree).tpe, app)

    case c @ source.Call(fun, _, args) =>
      // assumption: typer removed all ambiguous references, so there is exactly one
      val sym: Symbol = c.definition

      val as = args.flatMap(transform)

      // the type arguments, inferred by typer
      val targs = C.typeArguments(c)

      // right now only builtin functions are pure of control effects
      // later we can have effect inference to learn which ones are pure.
      sym match {
        case f: BuiltinFunction if f.pure =>
          PureApp(BlockVar(f), targs, as)
        case r: Record =>
          PureApp(BlockVar(r), targs, as)
        case f: EffectOp =>
          C.panic("Should have been translated to a method call!")
        case f: Field =>
          val List(arg: Expr) = as
          Select(arg, f)
        case f: BlockSymbol =>
          C.bind(C.inferredTypeOf(tree).tpe, App(BlockVar(f), targs, as))
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
            case op @ source.OpClause(id, params, body, resume) =>
              val ps = transformParams(params)

              // introduce a block parameter for resume
              val resumeParam = BlockParam(resume.symbol.asInstanceOf[BlockSymbol])

              val opBlock = BlockLit(ps :+ resumeParam, transform(body))
              (op.definition, opBlock)
          })
      }

      C.bind(C.inferredTypeOf(tree).tpe, Handle(body, hs))

    case source.Hole(stmts) =>
      C.bind(C.inferredTypeOf(tree).tpe, Hole)

  }

  def transform(arg: source.ArgSection)(implicit C: Context): List[core.Argument] = arg match {
    case source.ValueArgs(args)        => args.map(transform)
    case source.BlockArg(params, body) => List(BlockLit(transformParams(params), transform(body)))
    case c @ source.CapabilityArg(id)  => List(BlockVar(c.definition))
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

  def insertBindings(stmt: => Stmt)(implicit C: Context): Stmt = {
    val (body, bindings) = C.withBindings { stmt }

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
    core.BlockParam(id, C.interfaceTypeOf(id))

  def Val(id: ValueSymbol, binding: Stmt, body: Stmt)(implicit C: Context): core.Val =
    core.Val(id, C.valueTypeOf(id), binding, body)
}
trait TransformerOps extends ContextOps { Context: Context =>

  case class StateCapability(param: CapabilityParam, effect: UserEffect, get: EffectOp, put: EffectOp)

  private def StateCapability(binder: VarBinder)(implicit C: Context): StateCapability = {
    val tpe = C.valueTypeOf(binder)
    val eff = UserEffect(binder.name, Nil)
    val get = EffectOp(binder.name.rename(name => "get"), Nil, List(Nil), Some(tpe / Pure), eff)
    val put = EffectOp(binder.name.rename(name => "put"), Nil, List(List(ValueParam(binder.name, Some(tpe)))), Some(builtins.TUnit / Pure), eff)

    val param = CapabilityParam(binder.name, CapabilityType(eff))
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
}
