package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*

object PreTyper extends Phase[NameResolved, NameResolved] {

  val phaseName = "pre-typer"

  def run(input: NameResolved)(implicit C: Context) = {
    val traversal = new BoxUnboxInference
    val transformedTree = traversal.rewrite(input.tree)

    if (Context.buffer.hasErrors) { None }
    else { Some(input.copy(tree = transformedTree)) }
  }
}


class BoxUnboxInference {

  import source._

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
      case sym: ValueSymbol => Unbox(v).inheritPosition(v)
      case sym: BlockSymbol => v
    }

    case Unbox(t) => Unbox(rewriteAsExpr(t))
    case other => Unbox(rewriteAsExpr(other))
  }

  def rewriteAsExpr(e: Term)(using C: Context): Term = visit(e) {

    case Unbox(expr)              => rewriteAsExpr(expr)

    case v: Var => v.definition match {
      case sym: ValueSymbol => v
      // TODO maybe we should synthesize a call to get here already?
      case sym: VarBinder => v
      case sym: BlockSymbol => Box(None, InterfaceArg(v.id).inheritPosition(v)).inheritPosition(v)
    }

    case l: Literal[t]            => l

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
      Call(rewrite(fun), targs, vargs.map(rewriteAsExpr), bargs.map(rewrite))

    case m @ MethodCall(receiver, id, targs, vargs, bargs) =>
      val vargsTransformed = vargs.map(rewriteAsExpr)
      val bargsTransformed = bargs.map(rewrite)

      val syms = m.definition match {
        // an overloaded call target
        case symbols.CallTarget(name, syms) => syms.flatten
        case s => C.panic(s"Not a valid method or function: ${id.name}")
      }

      val (funs, methods) = syms.partitionMap {
        case t: symbols.Operation => Right(t)
        case t: symbols.Fun => Left(t)
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

    case Hole(stmts) =>
      Hole(rewrite(stmts))

    case Box(c, b) =>
      Box(c, rewrite(b))
  }

  def rewrite(target: source.CallTarget)(using C: Context): source.CallTarget = visit(target) {
    case source.ExprTarget(receiver) => source.ExprTarget(rewriteAsBlock(receiver))
    case t: source.IdTarget => t.definition match {
      case sym: ValueSymbol =>
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

    case VarDef(id, annot, region, binding) =>
      VarDef(id, annot, region, rewrite(binding))

    case d: InterfaceDef        => d
    case d: DataDef       => d
    case d: RecordDef     => d
    case d: TypeDef       => d
    case d: EffectDef     => d

    case d: ExternType    => d
    case d: ExternEffect  => d
    case d: ExternFun     => d
    case d: ExternInclude => d
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

  def rewrite(b: BlockArg)(using C: Context): BlockArg = b match {
    case b: FunctionArg  => rewrite(b)
    case b: InterfaceArg => b
  }

  def rewrite(b: FunctionArg)(using C: Context): FunctionArg =  visit(b) {
    case FunctionArg(tps, vps, bps, body) => FunctionArg(tps, vps, bps, rewrite(body))
  }

  def rewrite(h: Handler)(using C: Context): Handler = visit(h) {
    case Handler(effect, capability, clauses) =>
      Handler(effect, capability, clauses.map(rewrite))
  }

  def rewrite(h: OpClause)(using C: Context): OpClause = visit(h) {
    case OpClause(id, tparams, params, body, resume) =>
      OpClause(id, tparams, params, rewrite(body), resume)
  }

  def rewrite(c: MatchClause)(using C: Context): MatchClause = visit(c) {
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
