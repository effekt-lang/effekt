package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*

object BoxUnboxInference extends Phase[NameResolved, NameResolved] {

  import source._

  val phaseName = "box-unbox"

  def run(input: NameResolved)(using Context) = {
    val transformedTree = rewrite(input.tree)

    if (Context.messaging.hasErrors) { None }
    else { Some(input.copy(tree = transformedTree)) }
  }


  def rewrite(e: ModuleDecl)(using C: Context): ModuleDecl = visit(e) {
    case ModuleDecl(path, imports, defs) =>
      ModuleDecl(path, imports, defs.map(rewrite))
  }

  /**
   * There are only a few limited constructors for blocks:
   *
   * - identifiers (e.g., `f`)
   * - explicit unboxing (e.g. `unbox EXPR`, or maybe postfix `EXPR!`)
   * - object literals (e.g. `new T {}`)
   * - selection (e.g., `f.m.n`)
   */
  def rewriteAsBlock(e: Term)(using C: Context): Term = visit(e) {
    case v: Var => v.definition match {
      case sym: (ValueSymbol | symbols.VarBinder) => Unbox(v).inheritPosition(v)
      case sym: BlockSymbol => v
    }

    case Unbox(t) => Unbox(rewriteAsExpr(t))
    case New(impl) => New(rewrite(impl))
    case BlockLiteral(tps, vps, bps, body) => BlockLiteral(tps, vps, bps, rewrite(body))
    case other => Unbox(rewriteAsExpr(other))
  }

  def rewriteAsExpr(e: Term)(using C: Context): Term = visit(e) {

    case Unbox(expr) => rewriteAsExpr(expr)

    case v: Var => v.definition match {
      // TODO maybe we should synthesize a call to get here already?
      case sym: (ValueSymbol | symbols.VarBinder) => v
      case sym: BlockSymbol => Box(None, v).inheritPosition(v)
    }

    case n: New => Box(None, rewriteAsBlock(n)).inheritPosition(n)

    case b: BlockLiteral => Box(None, rewriteAsBlock(b)).inheritPosition(b)

    case l: Literal => l

    case Assign(id, expr) =>
      Assign(id, rewriteAsExpr(expr))

    case If(cond, thn, els) =>
      If(rewriteAsExpr(cond), rewrite(thn), rewrite(els))

    case While(cond, body) =>
      While(rewriteAsExpr(cond), rewrite(body))

    case Match(sc, clauses) =>
      Match(rewriteAsExpr(sc), clauses.map(rewrite))

    case s @ Select(recv, name) if s.definition.isInstanceOf[Field] =>
      Select(rewriteAsExpr(recv), name)

    case s @ Select(recv, name) =>
      C.abort("selection on blocks not supported yet.")

    case Do(effect, id, targs, vargs) =>
      Do(effect, id, targs, vargs.map(rewriteAsExpr))

    case Call(fun, targs, vargs, bargs) =>
      Call(rewrite(fun), targs, vargs.map(rewriteAsExpr), bargs.map(rewriteAsBlock))

    case m @ MethodCall(receiver, id, targs, vargs, bargs) =>
      val vargsTransformed = vargs.map(rewriteAsExpr)
      val bargsTransformed = bargs.map(rewriteAsBlock)

      val syms = m.definition match {
        // an overloaded call target
        case symbols.CallTarget(name, syms) => syms.flatten
        case s => C.panic(s"Not a valid method or function: ${id.name}")
      }

      val (funs, methods) = syms.partitionMap {
        case t: symbols.Operation => Right(t)
        case t: symbols.Callable => Left(t)
        case t => C.abort(pp"Not a valid method or function: ${t}")
      }

      // we prefer methods over uniform call syntax
      if (methods.nonEmpty) {
        MethodCall(rewriteAsBlock(receiver), id, targs, vargsTransformed, bargsTransformed)
      } else {
        Call(IdTarget(id).inheritPosition(id), targs, rewriteAsExpr(receiver) :: vargsTransformed, bargsTransformed)
      }

    case TryHandle(prog, handlers) =>
      TryHandle(rewrite(prog), handlers.map(rewrite))

    case Region(name, body) =>
      Region(name, rewrite(body))

    case Hole(stmts) =>
      Hole(rewrite(stmts))

    case Box(c, b) =>
      Box(c, rewriteAsBlock(b))
  }

  def rewrite(target: source.CallTarget)(using C: Context): source.CallTarget = visit(target) {
    case source.ExprTarget(receiver) => source.ExprTarget(rewriteAsBlock(receiver))
    case t: source.IdTarget => t.definition match {
      case sym: (ValueSymbol | symbols.VarBinder) =>
        source.ExprTarget(source.Unbox(source.Var(t.id).inheritPosition(t)).inheritPosition(t)).inheritPosition(t)
      case sym: BlockSymbol =>
        t
    }
  }

  def rewrite(t: Def)(using C: Context): Def = visit(t) {

    case FunDef(id, tparams, vparams, bparams, ret, body) =>
      FunDef(id, tparams, vparams, bparams, ret, rewrite(body))

    case ValDef(id, annot, binding) =>
      ValDef(id, annot, rewrite(binding))

    case RegDef(id, annot, region, binding) =>
      RegDef(id, annot, region, rewrite(binding))

    case VarDef(id, annot, binding) =>
      VarDef(id, annot, rewrite(binding))

    case DefDef(id, annot, binding) =>
      val block = rewriteAsBlock(binding)
      (binding, block) match {
        case (Unbox(_), _) => ()
        // If the binding wasn't an `Unbox` and now it is, it means that the compiler synthesized it.
        // We therefore annotate the new `Unbox` expression with its original definition.
        // See [[Annotations.UnboxParentDef]] for more details about this annotation.
        case (_, u @ Unbox(_)) => C.annotate(Annotations.UnboxParentDef, u, t)
        case (_, _) => ()
      }
      DefDef(id, annot, block)

    case d: InterfaceDef   => d
    case d: DataDef        => d
    case d: RecordDef      => d
    case d: TypeDef        => d
    case d: EffectDef      => d

    case d: ExternType     => d
    case d: ExternDef      => d
    case d: ExternResource => d
    case d: ExternInterface => d
    case d: ExternInclude  => d
  }

  def rewrite(t: Stmt)(using C: Context): Stmt = visit(t) {
    case DefStmt(d, rest) =>
      DefStmt(rewrite(d), rewrite(rest))

    case ExprStmt(e, rest) =>
      ExprStmt(rewriteAsExpr(e), rewrite(rest))

    case Return(e) =>
      Return(rewriteAsExpr(e))

    case BlockStmt(b) =>
      BlockStmt(rewrite(b))
  }

  def rewrite(h: Handler)(using Context): Handler = visit(h) {
    case Handler(capability, impl) =>
      Handler(capability, rewrite(impl))
  }

  def rewrite(i: Implementation)(using Context): Implementation = visit(i) {
    case Implementation(interface, clauses) =>
      Implementation(interface, clauses.map(rewrite))
  }

  def rewrite(h: OpClause)(using Context): OpClause = visit(h) {
    case OpClause(id, tparams, params, ret, body, resume) =>
      OpClause(id, tparams, params, ret, rewrite(body), resume)
  }

  def rewrite(c: MatchClause)(using Context): MatchClause = visit(c) {
    case MatchClause(pattern, body) =>
      MatchClause(pattern, rewrite(body))
  }

  /**
   * Copies all annotations and position information from source to target
   */
  def visit[T <: Tree, R <: Tree](source: T)(block: T => R)(using C: Context): R = {
    val target = block(source)
    target.inheritPosition(source)
    C.copyAnnotations(source, target)
    target
  }
}
