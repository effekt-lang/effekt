package effekt
package core

import scala.collection.mutable

import effekt.context.Context
import effekt.context.assertions.SymbolAssertions
import effekt.symbols._
import effekt.util.{ Task, control }
import effekt.util.control._

case class Wildcard(module: Module) extends ValueSymbol { val name = Name("_", module) }
case class Tmp(module: Module) extends ValueSymbol { val name = Name("tmp" + Symbol.fresh.next(), module) }

class Transformer extends Phase[Module, core.ModuleDecl] {

  def run(mod: Module)(implicit C: Context): Option[ModuleDecl] =
    Some(transform(mod)(TransformerContext(C)))

  def transform(mod: Module)(implicit C: TransformerContext): ModuleDecl = {
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
  def transform(d: source.Def, rest: => Stmt)(implicit C: TransformerContext): Stmt = (d match {
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

  def transform(tree: source.Stmt)(implicit C: TransformerContext): Stmt = (tree match {
    case source.DefStmt(d, rest) =>
      transform(d, transform(rest))

    case source.ExprStmt(e, rest) => {
      Val(freshWildcardFor(tree), ANF { transform(e).map(Ret) }, transform(rest))
    }

    case source.Return(e) =>
      ANF { transform(e).map(Ret) }

    case source.BlockStmt(b) =>
      transform(b)

  }).inheritPosition(tree)

  def transformLit[T](tree: source.Literal[T])(implicit C: TransformerContext): Literal[T] = tree match {
    case source.UnitLit()         => UnitLit()
    case source.IntLit(value)     => IntLit(value)
    case source.BooleanLit(value) => BooleanLit(value)
    case source.DoubleLit(value)  => DoubleLit(value)
    case source.StringLit(value)  => StringLit(value)
  }

  def transform(tree: source.Expr)(implicit C: TransformerContext): Control[Expr] = (tree match {
    case v: source.Var => v.definition match {
      case sym: VarBinder =>
        val state = C.state(sym)
        bind(freshTmpFor(tree), App(Member(C.resolveCapability(state.effect), state.get), Nil))
      case sym => pure { ValueVar(sym) }
    }

    case a @ source.Assign(id, expr) =>
      transform(expr).flatMap { e =>
        val state = C.state(a.definition)
        bind(freshTmpFor(tree), App(Member(C.resolveCapability(state.effect), state.put), List(e)))
      }

    case l: source.Literal[t] => pure { transformLit(l) }

    case source.If(cond, thn, els) =>
      transform(cond).flatMap { c => bind(freshTmpFor(tree), If(c, transform(thn), transform(els))) }

    case source.While(cond, body) =>
      bind(freshTmpFor(tree), While(ANF { transform(cond) map Ret }, transform(body)))

    case source.MatchExpr(sc, clauses) =>

      val cs: List[(Pattern, BlockLit)] = clauses.map {
        case cl @ source.MatchClause(pattern, body) =>
          val (p, ps) = transform(pattern)
          (p, BlockLit(ps, transform(body)))
      }
      transform(sc).flatMap { scrutinee => bind(freshTmpFor(tree), Match(scrutinee, cs)) }

    // assumption: typer removed all ambiguous references, so there is exactly one
    case c @ source.Call(fun, _, args) =>
      val sym: Symbol = c.definition

      // we do not want to provide capabilities for the effect itself
      val ownEffect = sym match {
        case e: EffectOp => Effects(List(e.effect))
        case _           => Pure
      }

      val BlockType(tparams, params, ret / effs) = C.blockTypeOf(sym)

      // Do not provide capabilities for builtin effects and also
      // omit the capability for the effect itself (if it is an effect operation
      val effects = (effs -- ownEffect).userDefined
      val capabilityArgs = effects.toList.map { C.resolveCapability }

      val as: List[Control[List[Argument]]] = (args zip params) map {
        case (source.ValueArgs(as), _) => sequence(as.map(transform))
        case (source.BlockArg(ps, body), List(p: BlockType)) =>
          val params = ps.params.map { v => core.ValueParam(v.symbol) }
          pure {
            C.bindingCapabilities(p.ret.effects.userEffects) { caps =>
              List(BlockLit(params ++ caps, transform(body)))
            }
          }
      }

      val as2: Control[List[Argument]] = sequence(as).map { ls => ls.flatten }

      // right now only builtin functions are pure of control effects
      // later we can have effect inference to learn which ones are pure.
      sym match {
        case f: BuiltinFunction if f.pure =>
          as2.map { args => PureApp(BlockVar(f), args ++ capabilityArgs) }
        case r: Record =>
          as2.map { args => PureApp(BlockVar(r), args ++ capabilityArgs) }
        case f: EffectOp =>
          as2.flatMap { args =>
            bind(freshTmpFor(tree), App(Member(C.resolveCapability(f.effect), f), args ++ capabilityArgs))
          }
        case f: Field =>
          as2.map { case List(arg: Expr) => Select(arg, f) }
        case f: BlockSymbol =>
          as2.flatMap { args => bind(freshTmpFor(tree), App(BlockVar(f), args ++ capabilityArgs)) }
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

      bind(freshTmpFor(tree), Handle(body, hs))

    case source.Hole(stmts) =>
      bind(freshTmpFor(tree), Hole)

  }).map { _.inheritPosition(tree) }

  def transform(tree: source.MatchPattern)(implicit C: TransformerContext): (Pattern, List[core.ValueParam]) = tree match {
    case source.IgnorePattern()    => (core.IgnorePattern(), Nil)
    case source.LiteralPattern(l)  => (core.LiteralPattern(transformLit(l)), Nil)
    case p @ source.AnyPattern(id) => (core.AnyPattern(), List(core.ValueParam(p.symbol)))
    case p @ source.TagPattern(id, ps) =>
      val (patterns, params) = ps.map(transform).unzip
      (core.TagPattern(p.definition, patterns), params.flatten)
  }

  def transform(exprs: List[source.Expr])(implicit C: TransformerContext): Control[List[Expr]] = exprs match {
    case Nil => pure { Nil }
    case (e :: rest) => for {
      ev <- transform(e)
      rv <- transform(rest)
    } yield ev :: rv
  }

  def freshTmpFor(e: source.Tree)(implicit C: TransformerContext): Tmp = {
    val x = Tmp(C.module)
    C.typeOf(e) match {
      case Some(t / _) => C.assignType(x, t)
      case _           => C.abort("Internal Error: Missing type of source expression.")
    }
    x
  }

  def freshWildcardFor(e: source.Tree)(implicit C: TransformerContext): Wildcard = {
    val x = Wildcard(C.module)
    C.typeOf(e) match {
      case Some(t / _) => C.assignType(x, t)
      case _           => C.abort("Internal Error: Missing type of source expression.")
    }
    x
  }

  private val delimiter: Cap[Stmt] = new control.Capability { type Res = Stmt }

  def ANF(e: Control[Stmt]): Stmt = control.handle(delimiter)(e).run()
  def bind(x: Tmp, e: Stmt)(implicit C: TransformerContext): Control[Expr] = control.use(delimiter) { k =>
    k.apply(ValueVar(x)).map {
      case Ret(ValueVar(y)) if x == y => e
      case body => Val(x, e, body)
    }
  }

  case class TransformerContext(context: Context) {
    /**
     * Synthesized state effects for var-definitions
     */
    private var stateEffects = mutable.Map.empty[VarBinder, StateCapability]
    def state(binder: VarBinder): StateCapability =
      stateEffects.getOrElseUpdate(binder, StateCapability(binder)(context))

    /**
     * Used to map each lexically scoped capability to its termsymbol
     */
    private var capabilities = Map.empty[Effect, symbols.Capability]

    /**
     * runs the given block, binding the provided capabilities, so that
     * "resolveCapability" will find them.
     */
    def bindingCapabilities[R](effs: List[UserEffect])(block: List[core.BlockParam] => R): R = {
      val before = capabilities;
      // create a fresh cabability-symbol for each bound effect
      val caps = effs.map { UserCapability }
      // additional block parameters for capabilities
      val params = caps.map { core.BlockParam }

      capabilities = capabilities ++ caps.map { c => (c.effect -> c) }.toMap
      val res = block(params)
      capabilities = before
      res
    }

    def resolveCapability(e: Effect) =
      capabilities.get(e).map { core.BlockVar }.getOrElse(
        context.abort(s"Compiler error: cannot find capability for ${e}")
      )
  }
  private implicit def asContext(C: TransformerContext): Context = C.context
  private implicit def getContext(implicit C: TransformerContext): Context = C.context
}
