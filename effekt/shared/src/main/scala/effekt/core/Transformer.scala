package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Context, ContextOps }
import effekt.symbols._
import symbols.builtins.TState
import effekt.context.assertions._

class Transformer extends Phase[Module, core.ModuleDecl] {

  val phaseName = "transformer"

  def run(mod: Module)(implicit C: Context): Option[ModuleDecl] = Context in {
    C.initTransformerState()
    Some(transform(mod))
  }

  def transform(mod: Module)(implicit C: Context): ModuleDecl = {
    val source.ModuleDecl(path, imports, defs) = mod.ast
    val exports: Stmt = Exports(path, mod.terms.flatMap {
      case (name, syms) => syms.collect {
        // TODO export valuebinders properly
        case sym: Fun if /* !sym.isInstanceOf[EffectOp] && */ !sym.isInstanceOf[Field] => sym
        case sym: ValBinder => sym
      }
    }.toList)

    val transformed = defs.foldRight(exports) {
      case (d, r) => transform(d, () => r)
    }

    ModuleDecl(path, imports.map { _.path }, transformed).inheritPosition(mod.decl)
  }

  /**
   * the "rest" is a thunk so that traversal of statements takes place in the correct order.
   */
  def transform(d: source.Def, rest: () => Stmt)(implicit C: Context): Stmt = withPosition(d) {

    case b @ source.BlockDef(id, tpe, block) =>
      val sym = b.symbol
      Def(sym, C.interfaceTypeOf(sym), transform(block), rest())

    case f @ source.FunDef(id, _, vparams, bparams, _, body) =>
      val sym = f.symbol
      val vps = vparams map transform
      val bps = bparams map transform
      Def(sym, C.interfaceTypeOf(sym), BlockLit(vps, bps, transform(body)), rest())

    case d @ source.DataDef(id, _, ctors) =>
      Data(d.symbol, ctors.map { c => c.symbol }, rest())

    case source.ExternType(id, tparams) =>
      rest()

    case f @ source.ExternFun(pure, id, tparams, vparams, ret, body) =>
      val sym = f.symbol
      Def(f.symbol, C.blockTypeOf(sym), Extern(pure, vparams map transform, body), rest())

    case e @ source.ExternInclude(path) =>
      Include(e.contents, rest())

    //    case e: source.ExternEffect =>
    //      rest
    //
    case d @ source.InterfaceDef(id, tparams, ops) =>
      core.Interface(d.symbol, ops.map { e => e.symbol }, rest())
  }

  def transform(tree: source.Stmt)(implicit C: Context): Stmt = withPosition(tree) {
    case source.MutualStmt(defs, rest) =>
      val thunk = defs.foldLeft(() => transform(rest)) { case (rest, d) => () => transform(d, rest) }
      thunk()

    case v @ source.ValDef(id, _, binding, rest) =>
      Val(v.symbol, transform(binding), transform(rest))

    // This phase introduces capabilities for state effects
    case v @ source.VarDef(id, _, reg, binding, rest) =>
      val sym = v.symbol
      val b = transform(binding)
      State(b, reg.map { r => r.symbol }, BlockLit(List(), List(BlockParam(sym)), transform(rest)))

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

  def transformUnbox(tree: source.Term)(implicit C: Context): Block =
    Unbox(transform(tree))

  def transformBox(tree: source.Term)(implicit C: Context): Expr =
    Box(transformAsBlock(tree))

  def transformAsBlock(tree: source.Term)(implicit C: Context): Block = withPosition(tree) {
    case v: source.Var => v.definition match {
      case sym: ValueSymbol => transformUnbox(tree)
      case sym: BlockSymbol => BlockVar(sym)
    }

    case source.Select(receiver, selector) =>
      Select(transformAsBlock(receiver), selector.name)

    case source.Unbox(b) =>
      Unbox(transform(b))

    case _ =>
      transformUnbox(tree)
  }

  // TODO move and share
  def getStateType(v: VarBinder)(implicit C: Context): ValueType = C.blockTypeOf(v) match {
    case BlockTypeApp(TState, List(tpe)) => tpe
  }

  def transform(tree: source.Term)(implicit C: Context): Expr = withPosition(tree) {
    case v: source.Var => v.definition match {
      case sym: VarBinder =>
        val tpe = getStateType(sym)
        val get = App(Select(BlockVar(sym), "get"), Nil, Nil, Nil)
        C.bind(tpe, get)
      case sym: ValueSymbol => ValueVar(sym)
      case sym: BlockSymbol => transformBox(tree)
    }

    case a @ source.Assign(id, expr) =>
      val e = transform(expr)
      val sym = a.definition
      val put = App(Select(BlockVar(sym), "put"), Nil, List(e), Nil)
      C.bind(builtins.TUnit, put)

    case l: source.Literal[t] => transformLit(l)

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
          (p, BlockLit(ps, Nil, transform(body)))
      }
      C.bind(C.inferredTypeOf(tree), Match(scrutinee, cs))

    case source.Box(_, block) =>
      Box(transform(block))

    case source.Unbox(b) => transformBox(tree)

    // TODO generate "pure" applications again
    case c @ source.Call(e, _, vargs, bargs) =>
      val b = transformAsBlock(e)
      val vas = vargs map transform
      val bas = bargs map transform
      C.bind(C.inferredTypeOf(tree), App(b, Nil, vas, bas))

    case source.Select(receiver, selector) => transformBox(tree)

    case source.Region(id, body) =>
      C.bind(
        C.inferredTypeOf(tree),
        Region(BlockLit(Nil, List(BlockParam(C.symbolOf(id).asInstanceOf[BlockSymbol])), transform(body)))
      )

    case source.TryHandle(prog, handlers) =>

      val caps = handlers.map { h =>
        BlockParam(h.capability.symbol)
      }
      val body = BlockLit(Nil, caps, transform(prog))

      // to obtain a canonical ordering of operation clauses, we use the definition ordering
      val hs = handlers.map {
        case h @ source.Handler(cap, cls) =>

          val effect = cap.symbol.tpe.asInterfaceType.interface
          val clauses = cls.map { cl => (cl.definition, cl) }.toMap

          Handler(effect, effect.ops.map(clauses.apply).map {
            case op @ source.OpClause(id, tparams, vparams, body, resume) =>
              val vps = vparams map transform

              // introduce a block parameter for resume
              val resumeParam = BlockParam(resume.symbol.asInstanceOf[BlockSymbol])

              val opBlock = BlockLit(vps, List(resumeParam), transform(body))
              (op.definition, opBlock)
          })
      }

      C.bind(C.inferredTypeOf(tree), Handle(body, hs))

    case source.Hole(stmts) =>
      C.bind(C.inferredTypeOf(tree), Hole)

  }

  def transform(arg: source.BlockArg)(implicit C: Context): core.Block = arg match {
    case source.FunctionArg(tparams, vparams, bparams, body) => BlockLit(vparams map transform, bparams map transform, transform(body))
    case c @ source.InterfaceArg(id) => transformAsBlock(source.Var(id))
    case source.UnboxArg(expr) => Unbox(transform(expr))
    case source.NewArg(tpe, members) => New(tpe.resolve.asInterfaceType, members map {
      case op @ source.OpClause(id, tparams, vparams, body, resume) =>
        val vps = vparams map transform
        // currently the don't take block params
        val opBlock = BlockLit(vps, List(), transform(body))
        (op.definition, opBlock)
    })
  }

  def transform(p: source.ValueParam)(implicit C: Context): core.ValueParam = ValueParam(p.symbol)
  def transform(p: source.BlockParam)(implicit C: Context): core.BlockParam = BlockParam(p.symbol)

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
      case _       => C.abort("Internal Error: Missing type of source expression.")
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
    core.BlockParam(id, C.blockTypeOf(id))

  def Val(id: ValueSymbol, binding: Stmt, body: Stmt)(implicit C: Context): core.Val =
    core.Val(id, C.valueTypeOf(id), binding, body)
}
trait TransformerOps extends ContextOps { Context: Context =>

  /**
   * A _mutable_ ListBuffer that stores all bindings to be inserted at the current scope
   */
  private var bindings: ListBuffer[(Tmp, symbols.ValueType, Stmt)] = ListBuffer()

  private[core] def initTransformerState() = {
    bindings = ListBuffer()
  }

  /**
   * Override the dynamically scoped `in` to also reset transformer state
   */
  override def in[T](block: => T): T = {
    val result = super.in(block)
    result
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
