package effekt
package core
package optimizer

object DirectStyle {

  def rewrite(m: ModuleDecl): ModuleDecl = m match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      ModuleDecl(path, includes, declarations, externs, definitions.map(rewrite), exports)
  }

  def rewrite(d: Toplevel): Toplevel = d match {
    case Toplevel.Def(id, block) => Toplevel.Def(id, rewrite(block))
    case Toplevel.Val(id, tpe, binding) => Toplevel.Val(id, tpe, rewrite(binding))
  }

  def rewrite(b: Block): Block = b match {
    case Block.BlockVar(id, tpe, capt) => b
    case b: Block.BlockLit => rewrite(b)
    case Block.Unbox(pure) => Block.Unbox(rewrite(pure))
    case Block.New(impl) => Block.New(rewrite(impl))
  }

  def rewrite(b: Block.BlockLit): Block.BlockLit = b match {
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      Block.BlockLit(tparams, cparams, vparams, bparams, rewrite(body))
  }

  def rewrite(impl: Implementation): Implementation = impl match {
    case Implementation(interface, operations) =>
      Implementation(interface, operations.map { op =>
        op.copy(body = rewrite(op.body))
      })
  }

  def rewrite(e: Expr): Expr = e match {
    case DirectApp(b, targs, vargs, bargs) =>
      DirectApp(b, targs, vargs.map(rewrite), bargs.map(rewrite))
    case p: Pure => rewrite(p)
  }

  def rewrite(p: Pure): Pure = p match {
    case Pure.ValueVar(id, tpe) => p
    case Pure.Literal(value, tpe) => p
    case Pure.PureApp(b, targs, vargs) =>
      Pure.PureApp(b, targs, vargs.map(rewrite))
    case Pure.Make(data, tag, vargs) =>
      Pure.Make(data, tag, vargs.map(rewrite))
    case Pure.Box(b, capt) => Pure.Box(rewrite(b), capt)
  }

  def rewrite(s: Stmt): Stmt = s match {

    // val x = { ... return 42 }; stmt2
    //
    //   -->
    //
    // def l(x) = stmt2;
    // ...
    // l(42)
    case Val(id, tpe, binding, body) =>
      val rewrittenBinding = rewrite(binding)
      val rewrittenBody = rewrite(body)

      if canBeDirect(rewrittenBinding) then
        val l = Id("l")
        val joinpoint = BlockLit(Nil, Nil, ValueParam(id, tpe) :: Nil, Nil, rewrittenBody)
        Def(l, joinpoint, toDirectStyle(rewrittenBinding, BlockVar(l, joinpoint.tpe, joinpoint.capt)))
      else
        Val(id, tpe, rewrittenBinding, rewrittenBody)

    // Congruences
    case Def(id, block, body) =>
      Def(id, rewrite(block), rewrite(body))

    case Let(id, tpe, binding, body) =>
      Let(id, tpe, rewrite(binding), rewrite(body))

    case Return(expr) =>
      Return(rewrite(expr))

    case App(callee, targs, vargs, bargs) =>
      App(callee, targs, vargs.map(rewrite), bargs.map(rewrite))

    case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      Invoke(callee, method, methodTpe, targs, vargs.map(rewrite), bargs.map(rewrite))

    case If(cond, thn, els) =>
      If(rewrite(cond), rewrite(thn), rewrite(els))

    case Match(scrutinee, clauses, default) =>
      Match(rewrite(scrutinee),
        clauses.map { case (id, bl) => (id, rewrite(bl)) },
        default.map(rewrite))

    case Reset(body) => Reset(rewrite(body))
    case Shift(prompt, body) => Shift(prompt, rewrite(body))
    case Resume(k, body) => Resume(k, rewrite(body))
    case Region(body) => Region(rewrite(body))

    case Alloc(id, init, region, body) =>
      Alloc(id, rewrite(init), region, rewrite(body))

    case Var(ref, init, capture, body) =>
      Var(ref, rewrite(init), capture, rewrite(body))

    case Get(id, tpe, ref, capt, body) =>
      Get(id, tpe, ref, capt, rewrite(body))

    case Put(ref, capt, value, body) =>
      Put(ref, capt, rewrite(value), rewrite(body))

    case Hole() => s
  }

  private def canBeDirect(s: Stmt): Boolean = s match {
    case Return(expr) => true
    case Hole() => true

    // non-tail calls
    case App(_, _, _, _) => false
    case Invoke(_, _, _, _, _, _) => false

    // control effects
    case Reset(_) => false
    case Region(_) => false
    case Resume(_, _) => false
    case Shift(_, _) => false

    // Congruences
    case Def(id, block, body) => canBeDirect(body)
    case Let(id, tpe, binding, body) => canBeDirect(body)
    case Val(id, tpe, binding, body) => canBeDirect(body)
    case If(cond, thn, els) => canBeDirect(thn) && canBeDirect(els)
    case Match(scrutinee, clauses, default) =>
      clauses.forall { case (id, bl) => canBeDirect(bl.body) } && default.forall(canBeDirect)

    case Alloc(id, init, region, body) => canBeDirect(body)
    case Var(ref, init, capture, body) => canBeDirect(body)
    case Get(id, tpe, ref, capt, body) => canBeDirect(body)
    case Put(ref, capt, value, body) => canBeDirect(body)
  }

  private def toDirectStyle(stmt: Stmt, label: Block.BlockVar): Stmt = stmt match {
    case Return(expr) => App(label, Nil, List(expr), Nil)
    case Hole() => stmt

    // non-tail calls
    case App(_, _, _, _) => stmt
    case Invoke(_, _, _, _, _, _) => stmt

    // control effects
    case Reset(_) => stmt
    case Resume(_, _) => stmt
    case Shift(_, _) => stmt
    case Region(_) => stmt

    // Congruences
    case Def(id, block, body) =>
      Def(id, block, toDirectStyle(body, label))

    case Let(id, tpe, binding, body) =>
      Let(id, tpe, binding, toDirectStyle(body, label))

    case Val(id, tpe, binding, body) =>
      Val(id, tpe, binding, toDirectStyle(body, label))

    case If(cond, thn, els) =>
      If(cond, toDirectStyle(thn, label), toDirectStyle(els, label))

    case Match(scrutinee, clauses, default) =>
      Match(scrutinee,
        clauses.map { case (id, bl) => (id, bl.copy(body = toDirectStyle(bl.body, label))) },
        default.map(body => toDirectStyle(body, label)))

    case Alloc(id, init, region, body) =>
      Alloc(id, init, region, toDirectStyle(body, label))

    case Var(ref, init, capture, body) =>
      Var(ref, init, capture, toDirectStyle(body, label))

    case Get(id, tpe, ref, capt, body) =>
      Get(id, tpe, ref, capt, toDirectStyle(body, label))

    case Put(ref, capt, value, body) =>
      Put(ref, capt, value, toDirectStyle(body, label))
  }
}
