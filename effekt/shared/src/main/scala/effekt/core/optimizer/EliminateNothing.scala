package effekt
package core
package optimizer

object EliminateNothing extends Tree.Rewrite {

  override def stmt = {

    // do raise() --> do raise() match {}
    case stmt @ Invoke(callee, method, methodTpe, targs, vargs, bargs) if stmt.tpe == Type.TBottom =>
      val id = Id("tmp")
      Val(id, Type.TBottom, Invoke(rewrite(callee), method, methodTpe, targs, vargs.map(rewrite), bargs.map(rewrite)),
        Match(ValueVar(id, Type.TBottom), Nil, None))

    case stmt @ App(block, targs, vargs, bargs) if stmt.tpe == Type.TBottom =>
      val id = Id("tmp")
      Val(id, Type.TBottom, App(rewrite(block), targs, vargs.map(rewrite), bargs.map(rewrite)),
        Match(ValueVar(id, Type.TBottom), Nil, None))
  }
}
