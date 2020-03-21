package effekt
package core

import context.CompilerContext
import effekt.typer.Typer
import effekt.symbols._
import effekt.symbols.builtins._
import effekt.util.control
import effekt.util.control._
import effekt.util.messages.{ ErrorReporter, MessageBuffer }
import org.bitbucket.inkytonik.kiama.util.Memoiser

object Wildcard extends Symbol { val name = LocalName("_") }

class Transformer {

  def run(mod: Module, compiler: CompilerContext): ModuleDecl = {
    val source.ModuleDecl(path, imports, defs) = mod.decl
    val exports: Stmt = Exports(path, mod.terms.collect {
      case (name, sym) if sym.isInstanceOf[Fun] && !sym.isInstanceOf[EffectOp] => sym
    }.toList)

    val ctx = Context(compiler)

    ModuleDecl(path, imports.map { _.path }, defs.foldRight(exports) { case (d, r) =>
      transform(d, r)(given ctx)
    })
  }

  def transform(d: source.Def, rest: Stmt)(given Context): Stmt = d match {
    case f @ source.FunDef(id, _, params, _, body) =>
      val sym = f.symbol
      val effs = sym.effects

      val ps = params.flatMap {
        case b @ source.BlockParam(id, _) => List(core.BlockParam(b.symbol))
        case v @ source.ValueParams(ps) => ps.map { p => core.ValueParam(p.symbol) }
      } ++ effs.filterNot(_.builtin).map { core.BlockParam }

      Def(sym, BlockDef(ps, transform(body)), rest)

    case d @ source.DataDef(id, _, ctors) =>
      Data(d.symbol, ctors.map { c => c.symbol }, rest)

    case v @ source.ValDef(id, _, binding) =>
      Val(v.symbol, transform(binding), rest)

    case v @ source.VarDef(id, _, binding) =>
      Var(v.symbol, transform(binding), rest)

    case source.ExternType(id, tparams) =>
      rest

    case f @ source.ExternFun(pure, id, tparams, params, ret, body) =>
      // C&P from FunDef
      val effs = f.symbol.effects
      val ps = params.flatMap {
        case b @ source.BlockParam(id, _) => List(core.BlockParam(b.symbol))
        case v @ source.ValueParams(ps) => ps.map { p => core.ValueParam(p.symbol) }
      } ++ effs.filterNot(_.builtin).map { core.BlockParam }
      Def(f.symbol, Extern(ps, body), rest)

    case e @ source.ExternInclude(path) =>
      Include(e.contents, rest)

    case e: source.ExternEffect =>
      rest

    case e: source.EffDef =>
      rest
  }

  def transform(tree: source.Stmt)(given Context): Stmt = tree match {
    case source.DefStmt(d, rest) =>
      transform(d, transform(rest))

    case source.ExprStmt(e, rest) =>
      Val(Wildcard, ANF { transform(e).map(Ret) }, transform(rest))

    case source.Return(e) =>
      ANF { transform(e).map(Ret) }
  }

  def transform(tree: source.Expr)(given Context): Control[Expr] = tree match {
    case v : source.Var => v.definition match {
      case sym: VarBinder => pure { Deref(sym) }
      case sym => pure { ValueVar(sym) }
    }

    case a @ source.Assign(id, expr) =>
      transform(expr).map { e => Assign(a.definition, e) }

    case source.UnitLit() => pure { UnitLit() }
    case source.IntLit(value) => pure { IntLit(value) }
    case source.BooleanLit(value) => pure { BooleanLit(value) }
    case source.DoubleLit(value) => pure { DoubleLit(value) }
    case source.StringLit(value) => pure { StringLit(value) }

    case source.If(cond, thn, els) =>
      transform(cond).flatMap { c => bind(If(c, transform(thn), transform(els))) }

    case source.While(cond, body) =>
      bind(While(ANF { transform(cond) map Ret }, transform(body)))

    case source.MatchExpr(sc, clauses) =>
      val cs = clauses.map {
        case cl @ source.Clause(id, params, body) =>
          val ps = params.flatMap {
            case source.ValueParams(params) => params.map { v => core.ValueParam(v.symbol) }
          }

          (cl.definition, BlockDef(ps, transform(body)))
      }
      transform(sc).flatMap { scrutinee => bind(Match(scrutinee, cs)) }

    case source.Call(fun, _, args) =>
      val sym = fun.termSymbol

      // we do not want to provide capabilities for the effect itself
      val ownEffect = sym match {
        case e: EffectOp => Effects(List(e.effect))
        case _ => Pure
      }

      val BlockType(tparams, params, ret / effs) = Compiler.blockType(sym)

      // Do not provide capabilities for builtin effects and also
      // omit the capability for the effect itself (if it is an effect operation
      val effects = (effs -- ownEffect).effs.filterNot { _.builtin }
      val capabilities = effects.map { BlockVar }

      val as: List[Control[List[Expr | Block]]] = (args zip params) map {
        case (source.ValueArgs(as), _) => traverse(as.map(transform))
        case (source.BlockArg(ps, body), p: BlockType) =>
          val params = ps.map { v => core.ValueParam(v.symbol) }
          val caps = p.ret.effects.effs.filterNot(_.builtin).map { core.BlockParam }
          pure { List(BlockDef(params ++ caps, transform(body))) }
      }

      val as2: Control[List[Expr | Block]] = traverse(as).map { ls => ls.flatMap(identity) }

      // right now only builtin functions are pure of control effects
      // later we can have effect inference to learn which ones are pure.
      if ((sym.builtin && sym.asBuiltinFunction.pure) || sym.isInstanceOf[Constructor]) {
        as2.map { args => PureApp(BlockVar(sym), args ++ capabilities) }
      } else {
        as2.flatMap { args => bind(App(BlockVar(sym), args ++ capabilities)) }
      }

    case source.TryHandle(prog, clauses) =>
      val capabilities = clauses.map { c => core.BlockParam(c.definition) }
      val body = BlockDef(capabilities, transform(prog))
      val cs = clauses.map {
        case op @ source.OpClause(id, params, body, resume) =>
          val ps = params.flatMap { _.params.map { v => core.ValueParam(v.symbol) } }
          (op.definition, BlockDef(ps :+ core.BlockParam(resume.symbol), transform(body)))
      }
      bind(Handle(body, cs))
  }

  def traverse[R](ar: List[Control[R]])(given Context): Control[List[R]] = ar match {
    case Nil => pure { Nil }
    case (r :: rs) => for {
      rv <- r
      rsv <- traverse(rs)
    } yield rv :: rsv
  }

  def transform(exprs: List[source.Expr])(given Context): Control[List[Expr]] = exprs match {
    case Nil => pure { Nil }
    case (e :: rest) => for {
      ev <- transform(e)
      rv <- transform(rest)
    } yield ev :: rv
  }

  case class Context(compiler: CompilerContext) {

    def (f: Fun) effects = (f.ret match {
      case Some(t) => t
      case None => compiler.blockType(f).ret
    }).effects.effs

    def (id: source.Id) symbol = compiler.lookup(id)
    def (id: source.Id) termSymbol = compiler.lookup(id).asInstanceOf[TermSymbol]

    def (tree: source.Definition) symbol: tree.symbol = compiler.get(tree)
    def (tree: source.Reference) definition: tree.symbol = compiler.get(tree)
  }
  def Context(given c: Context): Context = c

  private val delimiter: Cap[Stmt] = new Capability { type Res = Stmt }
  case class Tmp() extends Symbol { val name = LocalName("tmp" + Symbol.fresh.next()) }
  def ANF(e: Control[Stmt]): Stmt = control.handle(delimiter)(e).run()
  def bind(e: Stmt): Control[Expr] = control.use(delimiter) { k =>
    val x = Tmp()
    k.apply(ValueVar(x)).map {
      case Ret(ValueVar(y)) if x == y => e
      case body => Val(x, e, body)
    }
  }
  def Compiler(given c: Context): CompilerContext = c.compiler
  given (given ctx: Context): CompilerContext = ctx.compiler
}