package effekt
package core

import scala.collection.mutable.ListBuffer

import effekt.context.Context
import effekt.symbols._
import effekt.util.Task

case class Wildcard(module: Module) extends ValueSymbol { val name = Name("_", module) }
case class Tmp(module: Module) extends ValueSymbol { val name = Name("tmp" + Symbol.fresh.next(), module) }

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
    C.transformerState = TransformerState()
    Some(transform(mod))
  }

  def transform(mod: Module)(implicit C: Context): ModuleDecl = {
    val source.ModuleDecl(path, imports, defs) = mod.decl
    val exports: Stmt = Exports(path, mod.terms.flatMap {
      case (name, syms) => syms.collect {
        // TODO export valuebinders properly
        case sym if sym.isInstanceOf[Fun] && !sym.isInstanceOf[EffectOp] && !sym.isInstanceOf[Field] => sym
        case sym if sym.isInstanceOf[ValBinder] => sym
      }
    }.toList)

    ModuleDecl(path, imports.map { _.path }, defs.foldRight(exports) {
      case (d, r) =>
        transform(d, r)(C)
    }).inheritPosition(mod.decl)
  }

  /**
   * the "rest" is a thunk so that traversal of statements takes place in the correct order.
   */
  def transform(d: source.Def, rest: => Stmt)(implicit C: Context): Stmt = (d match {
    case f @ source.FunDef(id, _, params, _, body) =>
      val sym = f.symbol

      val effs = sym.effects.userEffects

      C.bindingCapabilities(effs) { caps =>
        val ps = params.flatMap {
          case b @ source.BlockParam(id, _) => List(core.BlockParam(b.symbol))
          case v @ source.ValueParams(ps)   => ps.map { p => core.ValueParam(p.symbol) }
        } ++ caps

        Def(sym, BlockLit(ps, transform(body)), rest)
      }

    case d @ source.DataDef(id, _, ctors) =>
      Data(d.symbol, ctors.map { c => c.symbol }, rest)

    case d @ source.RecordDef(id, _, _) =>
      val rec = d.symbol
      core.Record(rec, rec.fields, rest)

    case v @ source.ValDef(id, _, binding) =>
      Val(v.symbol, transform(binding), rest)

    case v @ source.VarDef(id, _, binding) =>
      val eff = C.state(v.symbol)
      val b = transform(binding)
      val res = C.bindingCapabilities(List(eff.effect)) { caps =>
        State(eff.effect, eff.get, eff.put, b, BlockLit(caps, rest))
      }
      res

    case source.ExternType(id, tparams) =>
      rest

    case source.TypeDef(id, tparams, tpe) =>
      rest

    case source.EffectDef(id, effs) =>
      rest

    case f @ source.ExternFun(pure, id, tparams, params, ret, body) =>
      // C&P from FunDef
      // usually extern funs do not have userdefined capabilities
      if (f.symbol.effects.userDefined.nonEmpty) {
        C.abort("User defined effects on extern defs not allowed")
      }
      val ps = params.flatMap {
        case b @ source.BlockParam(id, _) => List(core.BlockParam(b.symbol))
        case v @ source.ValueParams(ps)   => ps.map { p => core.ValueParam(p.symbol) }
      }
      Def(f.symbol, Extern(ps, body), rest)

    case e @ source.ExternInclude(path) =>
      Include(e.contents, rest)

    case e: source.ExternEffect =>
      rest

    case d @ source.EffDef(id, ops) =>
      core.Record(d.symbol, ops.map { e => e.symbol }, rest)
  }).inheritPosition(d)

  def transform(tree: source.Stmt)(implicit C: Context): Stmt = (tree match {
    case source.DefStmt(d, rest) =>
      transform(d, transform(rest))

    case source.ExprStmt(e, rest) => {
      Val(freshWildcardFor(tree), C.ANF { Ret(transform(e)) }, transform(rest))
    }

    case source.Return(e) =>
      C.ANF { Ret(transform(e)) }

    case source.BlockStmt(b) =>
      transform(b)

  }).inheritPosition(tree)

  def transformLit[T](tree: source.Literal[T])(implicit C: Context): Literal[T] = tree match {
    case source.UnitLit()         => UnitLit()
    case source.IntLit(value)     => IntLit(value)
    case source.BooleanLit(value) => BooleanLit(value)
    case source.DoubleLit(value)  => DoubleLit(value)
    case source.StringLit(value)  => StringLit(value)
  }

  def transform(tree: source.Expr)(implicit C: Context): Expr = (tree match {
    case v: source.Var => v.definition match {
      case sym: VarBinder =>
        val state = C.state(sym)
        C.bind(C.inferredTypeOf(tree).tpe, App(Member(C.resolveCapability(state.effect), state.get), Nil))
      case sym => ValueVar(sym)
    }

    case a @ source.Assign(id, expr) =>
      val e = transform(expr)
      val state = C.state(a.definition)
      C.bind(C.inferredTypeOf(tree).tpe, App(Member(C.resolveCapability(state.effect), state.put), List(e)))

    case l: source.Literal[t] => transformLit(l)

    case source.If(cond, thn, els) =>
      val c = transform(cond)
      C.bind(C.inferredTypeOf(tree).tpe, If(c, transform(thn), transform(els)))

    case source.While(cond, body) =>
      C.bind(C.inferredTypeOf(tree).tpe, While(C.ANF { Ret(transform(cond)) }, transform(body)))

    case source.MatchExpr(sc, clauses) =>

      val cs: List[(Pattern, BlockLit)] = clauses.map {
        case cl @ source.MatchClause(pattern, body) =>
          val (p, ps) = transform(pattern)
          (p, BlockLit(ps, transform(body)))
      }
      val scrutinee = transform(sc)
      C.bind(C.inferredTypeOf(tree).tpe, Match(scrutinee, cs))

    // assumption: typer removed all ambiguous references, so there is exactly one
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
        case (source.ValueArgs(as), _) => as.map(transform)
        case (source.BlockArg(ps, body), List(p: BlockType)) =>
          val params = ps.params.map { v => core.ValueParam(v.symbol) }
          C.bindingCapabilities(p.ret.effects.userEffects) { caps =>
            List(BlockLit(params ++ caps, transform(body)))
          }
      }

      // right now only builtin functions are pure of control effects
      // later we can have effect inference to learn which ones are pure.
      sym match {
        case f: BuiltinFunction if f.pure =>
          PureApp(BlockVar(f), as ++ capabilityArgs)
        case r: Record =>
          PureApp(BlockVar(r), as ++ capabilityArgs)
        case f: EffectOp =>
          C.bind(C.inferredTypeOf(tree).tpe, App(Member(C.resolveCapability(f.effect), f), as ++ capabilityArgs))
        case f: Field =>
          val List(arg: Expr) = as
          Select(arg, f)
        case f: BlockSymbol =>
          C.bind(C.inferredTypeOf(tree).tpe, App(BlockVar(f), as ++ capabilityArgs))
      }

    case source.TryHandle(prog, handlers) =>

      val effects = handlers.map(_.definition)
      val body = C.bindingCapabilities(effects) { caps =>
        BlockLit(caps, transform(prog))
      }

      // to obtain a canonical ordering of operation clauses, we use the definition ordering
      val hs = handlers.map {
        case h @ source.Handler(eff, cls) =>
          val clauses = cls.map { cl => (cl.definition, cl) }.toMap

          Handler(h.definition, h.definition.ops.map(clauses.apply).map {
            case op @ source.OpClause(id, params, body, resume) =>
              val ps = params.flatMap { _.params.map { v => core.ValueParam(v.symbol) } }
              (op.definition, BlockLit(ps :+ core.BlockParam(resume.symbol.asInstanceOf[BlockSymbol]), transform(body)))
          })
      }

      C.bind(C.inferredTypeOf(tree).tpe, Handle(body, hs))

    case source.Hole(stmts) =>
      C.bind(C.inferredTypeOf(tree).tpe, Hole)

  }).inheritPosition(tree)

  def transform(tree: source.MatchPattern)(implicit C: Context): (Pattern, List[core.ValueParam]) = tree match {
    case source.IgnorePattern()    => (core.IgnorePattern(), Nil)
    case source.LiteralPattern(l)  => (core.LiteralPattern(transformLit(l)), Nil)
    case p @ source.AnyPattern(id) => (core.AnyPattern(), List(core.ValueParam(p.symbol)))
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
}
trait TransformerOps { Context: Context =>

  def state(binder: VarBinder): StateCapability = {
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
  def bindingCapabilities[R](effs: List[UserEffect])(block: List[core.BlockParam] => R): R = {
    val before = transformerState.capabilities;
    // create a fresh cabability-symbol for each bound effect
    val caps = effs.map { UserCapability }
    // additional block parameters for capabilities
    val params = caps.map { core.BlockParam }

    Context in {
      // update state with capabilities
      transformerState = transformerState.copy(
        capabilities = before ++ caps.map { c => (c.effect -> c) }.toMap
      )
      // run block
      block(params)
    }
  }

  def resolveCapability(e: Effect): core.BlockVar =
    transformerState.capabilities.get(e).map { core.BlockVar }.getOrElse(
      Context.abort(s"Compiler error: cannot find capability for ${e}")
    )

  /**
   * Introduces a binding for the given statement.
   *
   * @param tpe the type of the bound statement
   * @param s the statement to be bound
   */
  def bind(tpe: symbols.ValueType, s: Stmt): Expr = {

    // create a fresh symbol and assign the type
    val x = Tmp(module)
    assignType(x, tpe)

    val binding = (x, tpe, s)
    transformerState.bindings += binding

    ValueVar(x)
  }

  // TODO rename
  def ANF(stmt: => Stmt): Stmt = Context in {
    transformerState = transformerState.copy(
      bindings = ListBuffer()
    )
    val body = stmt
    val bindings = transformerState.bindings

    bindings.foldRight(body) {
      // optimization: remove unnecessary binds
      case ((x, tpe, b), Ret(ValueVar(y))) if x == y => b
      case ((x, tpe, b), body) => Val(x, b, body)
    }
  }
}
