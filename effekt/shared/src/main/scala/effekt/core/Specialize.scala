package effekt
package core

import effekt.context.Context
import effekt.PhaseResult.CoreTransformed
import effekt.core.substitutions.{ Substitution, substitute }
import effekt.util.DB

import scala.collection.mutable

/**
 * Specializes definitions taking one block to the local definitions passed to them, placing each copy right after
 * the later of the two definitions, where it sees everything both of them see:
 *
 *   def h(x) = x + i; … f(5){h}   ~>   def h(x) = x + i; def f_h(y) = [[ body of f ]][g := h]; … f_h(5)
 *
 * [[names]] gives every literal block argument a name, [[specialize]] makes the copies, and [[optimizer.cleanup]]
 * removes what is left unused and inlines what is called once.
 */
def specialization(m: ModuleDecl): ModuleDecl =
  optimizer.cleanup(specialize(names(m)))

object Specialize extends Phase[CoreTransformed, CoreTransformed] {
  val phaseName: String = "specialize"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case input @ CoreTransformed(_, _, _, core) =>
      if Context.config.optimize() then Some(input.copy(core = specialization(core))) else Some(input)
  }
}


// Names
// -----

/**
 * [[ f(…){ b } ]] = def g = [[ b ]]; f(…){ g }   for every block argument `b` that is a literal or an object
 */
def names(m: ModuleDecl): ModuleDecl = {
  object naming extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.App(callee, targs, vargs, bargs) =>
        val named = bargs.map {
          case block @ (_: BlockLit | _: Block.New) =>
            val id = Id("f")
            (Some(id -> rewrite(block)), Block.BlockVar(id, block.tpe, block.capt))
          case block => (None, rewrite(block))
        }
        named.flatMap(_._1).foldRight(Stmt.App(rewrite(callee), targs, vargs.map(rewrite), named.map(_._2)): Stmt) {
          case ((id, block), rest) => Stmt.Def(id, block, rest)
        }
      case other => super.rewrite(other)
    }
  }
  naming.rewrite(m)
}


// Specialize
// ----------

/**
 * What is in scope: the local definitions of literals and objects, the aliases `def x = y`, the copies, and the
 * callees and arguments of the copies we are in, by where they come from.
 */
case class Scope(
  definitions: Map[Id, Block],
  aliases: Map[Id, Id],
  copies: Map[(Id, Id), Id],
  within: Set[(Id, Id)]
) {
  def resolve(id: Id): Id = aliases.get(id).map(resolve).getOrElse(id)
}

/**
 * One copy per callee taking one block and local definition passed to it, right after the later of the two:
 *
 *   [[ def g = b; s ]] = def g = [[ b ]]; def f_g(x) = [[ body of f ]][k := g]; [[ s ]]   for each `f(…){g}` in `s`
 *   [[ f(e){g} ]] = f_g(e)   if the copy is in scope
 *
 * A copy asking for another copy with a block defined outside of it, which is not in scope, gets it at its beginning,
 * so that mutual recursion `ping(…){g}`, `pong(…){g}` ends up nested.
 * A copy of `f` for a block defined at the same place as the block of a copy of `f` we are in is built up recursively,
 * so we do not make it.
 */
def specialize(m: ModuleDecl): ModuleDecl = {
  val toplevel: Map[Id, BlockLit] = m.definitions.collect {
    case Toplevel.Def(id, block: BlockLit) => id -> block
  }.toMap

  val origin = mutable.Map.empty[Id, Id]
  def originOf(id: Id): Id = origin.get(id).map(originOf).getOrElse(id)

  def fresh(block: BlockLit): BlockLit = {
    val (renamed, renaming) = Renamer.rename(block)
    renaming.foreach { case (fresh, old) => origin(fresh) = old }
    renamed
  }

  /** The definition taking one block that [[f]] stands for. */
  def callee(f: Id, scope: Scope): Option[BlockLit] =
    scope.definitions.get(f).orElse(toplevel.get(f)).collect {
      case block: BlockLit if block.bparams.size == 1 => block
    }

  /** The calls `f(…){g}` in [[s]], by what the names stand for. */
  def calls(s: Stmt, scope: Scope): Set[(Id, Id)] = {
    object collecting extends Tree.Query[Map[Id, Id], Set[(Id, Id)]] {
      def empty = Set.empty
      def combine = _ ++ _
      def resolve(id: Id)(using aliases: Map[Id, Id]): Id = aliases.get(id).map(resolve).getOrElse(scope.resolve(id))
      override def stmt(using aliases: Map[Id, Id]) = {
        case Stmt.Def(id, Block.BlockVar(x, _, _), body) => query(body)(using aliases + (id -> x))
        case Stmt.App(Block.BlockVar(f, _, _), targs, vargs, List(Block.BlockVar(x, _, _))) =>
          combine(Set(resolve(f) -> resolve(x)), all(vargs, query))
      }
    }
    collecting.query(s)(using Map.empty)
  }

  /** The copies [[s]] asks for that [[wanted]] decides to make here, named, and the scope with them. */
  def provide(s: Stmt, scope: Scope)(wanted: (Id, Id) => Boolean): (List[(Id, Id, Id)], Scope) = {
    val requests = calls(s, scope).toList.filter { case (f, g) =>
      !scope.copies.contains((f, g)) && wanted(f, g) && callee(f, scope).isDefined &&
        scope.definitions.contains(g) && !scope.within.contains((originOf(f), originOf(g)))
    }
    val named = requests.map { case (f, g) => (f, g, Id(f.name.name + "_" + g.name.name)) }
    (named, scope.copy(copies = scope.copies ++ named.map { case (f, g, c) => (f, g) -> c }))
  }

  def copy(f: Id, g: Id, scope: Scope): BlockLit = {
    val block = fresh(callee(f, scope).get)
    val argument = scope.definitions(g)
    val k = block.bparams.head
    val variable = Block.BlockVar(g, argument.tpe, argument.capt)
    val body = substitute(block.body)(using Substitution(DB.empty,
      DB.from(block.cparams.map { c => c -> argument.capt }), DB.empty, DB.from(List(k.id -> variable))))
    val inner = scope.copy(within = scope.within + ((originOf(f), originOf(g))))
    val (outside, withOutside) = provide(body, inner) { (_, _) => true }
    BlockLit(block.tparams, Nil, block.vparams, Nil, bind(outside, withOutside, specializing.rewrite(body)(using withOutside)))
  }

  /** Binds the copies [[named]] in order, each seeing itself and the ones before it. */
  def bind(named: List[(Id, Id, Id)], scope: Scope, rest: Stmt): Stmt = {
    val before = scope.copy(copies = scope.copies -- named.map { case (f, g, _) => (f, g) })
    def go(named: List[(Id, Id, Id)], scope: Scope): Stmt = named match {
      case Nil => rest
      case (f, g, c) :: more =>
        val seeing = scope.copy(copies = scope.copies + ((f, g) -> c))
        Stmt.Def(c, copy(f, g, seeing), go(more, seeing))
    }
    go(named, before)
  }

  object specializing extends Tree.RewriteWithContext[Scope] {
    override def rewrite(s: Stmt)(using scope: Scope): Stmt = s match {
      case Stmt.Def(id, alias @ Block.BlockVar(x, _, _), body) =>
        Stmt.Def(id, alias, rewrite(body)(using scope.copy(aliases = scope.aliases + (id -> x))))

      case Stmt.Def(id, block @ (_: BlockLit | _: Block.New), body) =>
        val defined = scope.copy(definitions = scope.definitions + (id -> block))
        val (named, withCopies) = provide(body, defined) { (f, g) => f == id || g == id }
        Stmt.Def(id, rewrite(block)(using defined), bind(named, withCopies, rewrite(body)(using withCopies)))

      case Stmt.App(callee @ Block.BlockVar(f, _, capt), targs, vargs, List(argument @ Block.BlockVar(x, _, _)))
          if scope.copies.contains((scope.resolve(f), scope.resolve(x))) =>
        val c = scope.copies((scope.resolve(f), scope.resolve(x)))
        val tpe = callee.tpe match {
          case BlockType.Function(tparams, _, vparams, _, result) => BlockType.Function(tparams, Nil, vparams, Nil, result)
          case other => other
        }
        Stmt.App(Block.BlockVar(c, tpe, capt ++ argument.capt), targs, vargs.map(rewrite), Nil)

      case other => super.rewrite(other)
    }
  }

  val empty = Scope(Map.empty, Map.empty, Map.empty, Set.empty)
  m.copy(definitions = m.definitions.map {
    case Toplevel.Def(id, block) => Toplevel.Def(id, specializing.rewrite(block)(using empty))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, specializing.rewrite(binding)(using empty))
  })
}
