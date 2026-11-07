package effekt
package core
package optimizer

/**
 * Binds the body of every method of a named object to a definition of its own, which the method calls:
 *
 *   def o = new I { def m(x) = s }   ~>   def m'(x) = s; def o = new I { def m(x) = m'(x) }
 *
 * The [[Normalizer]] then always resolves a call to a method of a known object to a call of `m'`,
 * which it inlines or not like any other definition.
 * A local object whose method uses the object itself keeps that method, since `m'` comes before the object.
 */
object LiftMethods {

  def transform(m: ModuleDecl): ModuleDecl = m.copy(definitions = m.definitions.flatMap {
    case Toplevel.Def(id, Block.New(impl)) =>
      val (lifted, forwarding) = lift(id, lifting.rewrite(impl), selfAllowed = true)
      lifted.map { case (name, block) => Toplevel.Def(name, block) } :+ Toplevel.Def(id, Block.New(forwarding))
    case other => List(lifting.rewrite(other))
  })

  private object lifting extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, Block.New(impl), body) =>
        val (lifted, forwarding) = lift(id, rewrite(impl), selfAllowed = false)
        lifted.foldRight(Stmt.Def(id, Block.New(forwarding), rewrite(body))) {
          case ((name, block), rest) => Stmt.Def(name, block, rest)
        }
      case other => super.rewrite(other)
    }
  }

  private def lift(id: Id, impl: Implementation, selfAllowed: Boolean): (List[(Id, BlockLit)], Implementation) = {
    val lifted: List[(Option[(Id, BlockLit)], Operation)] = impl.operations.map {
      case op @ Operation(name, tparams, cparams, vparams, bparams, body)
          if forwards(tparams, vparams, bparams, body) || (!selfAllowed && body.free.freeIds.contains(id)) =>
        (None, op)
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        val literal: BlockLit = BlockLit(tparams, cparams, vparams, bparams, body)
        val (block, _) = Renamer.rename(literal)
        val method = Id(name)
        val call = Stmt.App(Block.BlockVar(method, block.tpe, block.capt),
          tparams.map(ValueType.Var.apply),
          vparams.map { p => Expr.ValueVar(p.id, p.tpe) },
          bparams.map { p => Block.BlockVar(p.id, p.tpe, p.capt) })
        (Some(method -> block), Operation(name, tparams, cparams, vparams, bparams, call))
    }
    (lifted.flatMap(_._1), Implementation(impl.interface, lifted.map(_._2)))
  }

  /** Whether [[body]] only calls a block with exactly the parameters. */
  def forwards(tparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt): Boolean = body match {
    case Stmt.App(Block.BlockVar(_, _, _), targs, vargs, bargs) =>
      targs == tparams.map(ValueType.Var.apply) &&
        vargs.size == vparams.size && vargs.collect { case Expr.ValueVar(x, _) => x } == vparams.map(_.id) &&
        bargs.size == bparams.size && bargs.collect { case Block.BlockVar(x, _, _) => x } == bparams.map(_.id)
    case _ => false
  }
}
