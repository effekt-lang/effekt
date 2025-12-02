package effekt
package core
package optimizer

object DirectStyle extends Tree.Rewrite {

  override def stmt = {

    // val x = { ... return 42 }; stmt2
    //
    //   -->
    //
    // def l(x) = stmt2;
    // ...
    // l(42)
    case Val(id, binding, body) =>
      val rewrittenBinding = rewrite(binding)
      val rewrittenBody = rewrite(body)

      if canBeDirect(rewrittenBinding) then
        val l = Id("l")
        val joinpoint = BlockLit(Nil, Nil, ValueParam(id, rewrittenBinding.tpe) :: Nil, Nil, rewrittenBody)
        Def(l, joinpoint, toDirectStyle(rewrittenBinding, BlockVar(l, joinpoint.tpe, joinpoint.capt)))
      else
        Val(id, rewrittenBinding, rewrittenBody)

  }

  private def canBeDirect(s: Stmt): Boolean = s match {
    case Return(expr) => true
    case Hole(_, _) => true

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
    case Let(id, binding, body) => canBeDirect(body)
    case ImpureApp(id, callee, targs, vargs, bargs, body) => canBeDirect(body)
    case Val(id, binding, body) => canBeDirect(body)
    case If(cond, thn, els) => canBeDirect(thn) && canBeDirect(els)
    case Match(scrutinee, tpe, clauses, default) =>
      clauses.forall { case (id, bl) => canBeDirect(bl.body) } && default.forall(canBeDirect)

    case Alloc(id, init, region, body) => canBeDirect(body)
    case Var(ref, init, capture, body) => canBeDirect(body)
    case Get(id, tpe, ref, capt, body) => canBeDirect(body)
    case Put(ref, capt, value, body) => canBeDirect(body)
  }

  private def toDirectStyle(stmt: Stmt, label: Block.BlockVar): Stmt = stmt match {
    case Return(expr) => App(label, Nil, List(expr), Nil)
    case Hole(_, _) => stmt

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

    case Let(id, binding, body) =>
      Let(id, binding, toDirectStyle(body, label))

    case ImpureApp(id, callee, targs, vargs, bargs, body) =>
      ImpureApp(id, callee, targs, vargs, bargs, toDirectStyle(body, label))

    case Val(id, binding, body) =>
      Val(id, binding, toDirectStyle(body, label))

    case If(cond, thn, els) =>
      If(cond, toDirectStyle(thn, label), toDirectStyle(els, label))

    case Match(scrutinee, tpe, clauses, default) =>
      // This changes the answer type of the match to the result of the join point, which is called in return position.
      val newTpe = label.tpe match {
        case BlockType.Function(_, _, _, _, result) => result
        case BlockType.Interface(_, _) => sys error "Join points need to have a function type"
      }
      Match(scrutinee, newTpe,
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
