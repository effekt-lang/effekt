package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*

object UnboxInference extends Phase[NameResolved, NameResolved] {

  import source._

  val phaseName = "unbox"

  def run(input: NameResolved)(using Context) = {
    val transformedTree = Context.timed(phaseName, input.source.name) { rewrite(input.tree) }

    if (Context.messaging.hasErrors) { None }
    else { Some(input.copy(tree = transformedTree)) }
  }


  def rewrite(e: ModuleDecl)(using C: Context): ModuleDecl = visit(e) {
    case ModuleDecl(path, imports, defs, doc, span) =>
      ModuleDecl(path, imports, defs.map(rewrite), doc, span)
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
      case sym: (ValueSymbol | symbols.RefBinder) => Unbox(v, v.span.synthesized).inheritPosition(v)
      case sym: BlockSymbol => v
    }

    case Unbox(t, span) => Unbox(rewriteAsExpr(t), span)
    case New(impl, span) => New(rewrite(impl), span)
    case BlockLiteral(tps, vps, bps, body, span) => BlockLiteral(tps, vps, bps, rewrite(body), span)
    case other => Unbox(rewriteAsExpr(other), other.span.synthesized)
  }

  def rewriteAsExpr(a: ValueArg)(using C: Context): ValueArg = ValueArg(a.name, rewriteAsExpr(a.value), a.span)

  def rewriteAsExpr(e: Term)(using C: Context): Term = visit(e) {

    case Unbox(expr, _) => rewriteAsExpr(expr)

    case v: Var => v.definition match {
      // TODO maybe we should synthesize a call to get here already?
      case sym: (ValueSymbol | symbols.RefBinder) => v
      case sym: BlockSymbol =>
        C.error(pp"Computation ${sym.name.name} is used in an expression position, which requires boxing (e.g. `box ${sym.name.name}`)")
        v
    }

    case n: New =>
      C.error(pp"Creating an instance in an expression requires boxing (e.g. `box new ${n.impl.id}[...] { ... }`)")
      rewriteAsBlock(n)

    case b: BlockLiteral =>
      C.error(pp"Function literals in expression position require boxing (e.g. `box { (${b.vparams.map(_.id).mkString(", ")}) => ... `)")
      rewriteAsBlock(b)

    case l: Literal => l

    case Assign(id, expr, span) =>
      Assign(id, rewriteAsExpr(expr), span)

    case If(guards, thn, els, span) =>
      If(guards.map(rewrite), rewrite(thn), rewrite(els), span)

    case While(guards, body, default, span) =>
      While(guards.map(rewrite), rewrite(body), default.map(rewrite), span)

    case Match(scs, clauses, default, span) =>
      Match(scs.map(rewriteAsExpr), clauses.map(rewrite), default.map(rewrite), span)

    case s @ Select(recv, name, span) if s.definition.isInstanceOf[Field] =>
      Select(rewriteAsExpr(recv), name, span)

    case s @ Select(recv, name, span) =>
      C.abort("selection on blocks not supported yet.")

    case Do(id, targs, vargs, bargs, span) =>
      Do(id, targs, vargs.map(rewriteAsExpr), bargs.map(rewriteAsBlock), span)

    case Call(fun, targs, vargs, bargs, span) =>
      Call(rewrite(fun), targs, vargs.map(rewriteAsExpr), bargs.map(rewriteAsBlock), span)

    case m @ MethodCall(receiver, id, targs, vargs, bargs, span) =>
      val vargsTransformed = vargs.map(rewriteAsExpr)
      val bargsTransformed = bargs.map(rewriteAsBlock)

      val hasMethods = m.definition match {
        // an overloaded call target
        case symbols.CallTarget(syms) => syms.flatten.exists(b => b.isInstanceOf[symbols.Operation])
        case s => false
      }

      // we prefer methods over uniform call syntax
      if (hasMethods) {
        MethodCall(rewriteAsBlock(receiver), id, targs, vargsTransformed, bargsTransformed, span)
      } else {
        Call(IdTarget(id).inheritPosition(id), targs, rewriteAsExpr(ValueArg.Unnamed(receiver)) :: vargsTransformed, bargsTransformed, span)
      }

    case TryHandle(prog, handlers, span) =>
      TryHandle(rewrite(prog), handlers.map(rewrite), span)

    case Region(name, body, span) =>
      Region(name, rewrite(body), span)

    case Hole(id, Template(strings, args), span) =>
      Hole(id, Template(strings, args.map(rewrite)), span)

    case Box(c, b, span) =>
      Box(c, rewriteAsBlock(b), span)
  }

  def rewrite(target: source.CallTarget)(using C: Context): source.CallTarget = visit(target) {
    case source.ExprTarget(receiver) => source.ExprTarget(rewriteAsBlock(receiver))
    case t: source.IdTarget => t.definition match {
      case sym: (ValueSymbol | symbols.RefBinder) =>
        source.ExprTarget(source.Unbox(source.Var(t.id, t.id.span).inheritPosition(t), t.span.synthesized).inheritPosition(t)).inheritPosition(t)
      case sym: BlockSymbol =>
        t
    }
  }

  def rewrite(t: Def)(using C: Context): Def = visit(t) {

    case FunDef(id, tparams, vparams, bparams, captures, ret, body, doc, span) =>
      FunDef(id, tparams, vparams, bparams, captures, ret, rewrite(body), doc, span)

    case ValDef(id, annot, binding, doc, span) =>
      ValDef(id, annot, rewrite(binding), doc, span)

    case RegDef(id, annot, region, binding, doc, span) =>
      RegDef(id, annot, region, rewrite(binding), doc, span)

    case VarDef(id, annot, binding, doc, span) =>
      VarDef(id, annot, rewrite(binding), doc, span)

    case DefDef(id, captures, annot, binding, doc, span) =>
      val block = rewriteAsBlock(binding)
      (binding, block) match {
        case (Unbox(_, _), _) => ()
        // If the binding wasn't an `Unbox` and now it is, it means that the compiler synthesized it.
        // We therefore annotate the new `Unbox` expression with its original definition.
        // See [[Annotations.UnboxParentDef]] for more details about this annotation.
        case (_, u @ Unbox(_, _)) => C.annotate(Annotations.UnboxParentDef, u, t)
        case (_, _) => ()
      }
      DefDef(id, captures, annot, block, doc, span)

    case NamespaceDef(name, defs, doc, span) =>
      NamespaceDef(name, defs.map(rewrite), doc, span)

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
    case DefStmt(d, rest, span) =>
      DefStmt(rewrite(d), rewrite(rest), span)

    case ExprStmt(e, rest, span) =>
      ExprStmt(rewriteAsExpr(e), rewrite(rest), span)

    case Return(e, span) =>
      Return(rewriteAsExpr(e), span)

    case BlockStmt(b, span) =>
      BlockStmt(rewrite(b), span)
  }

  def rewrite(h: Handler)(using Context): Handler = visit(h) {
    case Handler(capability, impl, span) =>
      Handler(capability, rewrite(impl), span)
  }

  def rewrite(i: Implementation)(using Context): Implementation = visit(i) {
    case Implementation(interface, clauses, span) =>
      Implementation(interface, clauses.map(rewrite), span)
  }

  def rewrite(h: OpClause)(using Context): OpClause = visit(h) {
    case OpClause(id, tparams, vparams, bparams, ret, body, resume, span) =>
      OpClause(id, tparams, vparams, bparams, ret, rewrite(body), resume, span)
  }

  def rewrite(c: MatchClause)(using Context): MatchClause = visit(c) {
    case MatchClause(pattern, guards, body, span) =>
      MatchClause(pattern, guards.map(rewrite), rewrite(body), span)
  }

  def rewrite(g: MatchGuard)(using Context): MatchGuard = g match {
    case BooleanGuard(condition, span) => BooleanGuard(rewriteAsExpr(condition), span)
    case PatternGuard(scrutinee, pattern, span) => PatternGuard(rewriteAsExpr(scrutinee), pattern, span)
  }

  /**
   * Copies all annotations and position information from source to target
   */
  def visit[T <: Tree, R <: Tree](source: T)(block: T => R)(using C: Context): R = C.at(source) {
    val target = block(source)
    target.inheritPosition(source)
    C.copyAnnotations(source, target)
    target
  }
}
