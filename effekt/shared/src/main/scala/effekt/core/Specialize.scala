package effekt
package core

import effekt.context.Context
import effekt.PhaseResult.CoreTransformed
import effekt.core.substitutions.{ Substitution, substitute }
import effekt.util.DB

import scala.collection.mutable

/**
 * Specializes definitions to the local definitions passed to them as blocks, placing each copy right after the last
 * of these definitions, where it sees everything all of them see:
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

/** A callee and, for each of its blocks, the local definition passed, if it is one. */
type Key = (Id, List[Option[Id]])

/**
 * What is in scope: the local definitions of literals and objects, the aliases `def x = y`, the copies, and the
 * keys of the copies we are in, by where their names come from.
 */
case class Scope(
  definitions: Map[Id, Block],
  aliases: Map[Id, Id],
  copies: Map[Key, Id],
  within: Set[Key]
) {
  def resolve(id: Id): Id = aliases.get(id).map(resolve).getOrElse(id)
}

/**
 * One copy per callee and local definitions passed to it, right after the last of them:
 *
 *   [[ def g = b; s ]] = def g = [[ b ]]; def f_g(x){h} = [[ body of f ]][k := g]; [[ s ]]   for each `f(…){g, h}` in `s`
 *   [[ f(e){g, h} ]] = f_g(e){h}   if the copy is in scope
 *
 * where `g` is a local definition and `h` is not.
 * A copy asking for another copy with blocks defined outside of it, which is not in scope, gets it at its beginning,
 * so that mutual recursion `ping(…){g}`, `pong(…){g}` ends up nested.
 * A copy of `f` for blocks defined at the same places as the blocks of a copy of `f` we are in is built up
 * recursively, so we do not make it.
 */
def specialize(m: ModuleDecl): ModuleDecl = {
  val toplevel: Map[Id, BlockLit] = m.definitions.collect {
    case Toplevel.Def(id, block: BlockLit) => id -> block
  }.toMap

  val origin = mutable.Map.empty[Id, Id]
  def originOf(id: Id): Id = origin.get(id).map(originOf).getOrElse(id)
  def originsOf(key: Key): Key = key match {
    case (f, arguments) => (originOf(f), arguments.map(_.map(originOf)))
  }

  def fresh(block: BlockLit): BlockLit = {
    val (renamed, renaming) = Renamer.rename(block)
    renaming.foreach { case (fresh, old) => origin(fresh) = old }
    renamed
  }

  /** The definition taking blocks that [[f]] stands for. */
  def callee(f: Id, scope: Scope): Option[BlockLit] =
    scope.definitions.get(f).orElse(toplevel.get(f)).collect {
      case block: BlockLit if block.bparams.nonEmpty => block
    }

  /** The keys of the calls in [[s]], by what the names stand for; we only know which names are definitions later. */
  def calls(s: Stmt, scope: Scope): Set[(Id, List[Option[Id]])] = {
    object collecting extends Tree.Query[Map[Id, Id], Set[(Id, List[Option[Id]])]] {
      def empty = Set.empty
      def combine = _ ++ _
      def resolve(id: Id)(using aliases: Map[Id, Id]): Id = aliases.get(id).map(resolve).getOrElse(scope.resolve(id))
      override def stmt(using aliases: Map[Id, Id]) = {
        case Stmt.Def(id, Block.BlockVar(x, _, _), body) => query(body)(using aliases + (id -> x))
        case Stmt.App(Block.BlockVar(f, _, _), targs, vargs, bargs) if bargs.nonEmpty =>
          val arguments = bargs.map {
            case Block.BlockVar(x, _, _) => Some(resolve(x))
            case _ => None
          }
          combine(Set(resolve(f) -> arguments), combine(all(vargs, query), all(bargs, query)))
      }
    }
    collecting.query(s)(using Map.empty)
  }

  /** The key of a call, given what is defined: arguments that are not local definitions are unknown. */
  def key(f: Id, arguments: List[Option[Id]], scope: Scope): Option[Key] = {
    val known = arguments.map(_.filter(scope.definitions.contains))
    Option.when(known.exists(_.isDefined) && callee(f, scope).exists(_.bparams.size == arguments.size))((f, known))
  }

  /** The local definitions of literals and objects in [[s]]. */
  def definitions(s: Stmt): Set[Id] = {
    object collecting extends Tree.Query[Unit, Set[Id]] {
      def empty = Set.empty
      def combine = _ ++ _
      override def stmt(using Unit) = {
        case Stmt.Def(id, block @ (_: BlockLit | _: Block.New), body) => Set(id) ++ query(block) ++ query(body)
      }
    }
    collecting.query(s)(using ())
  }

  /**
   * The copies [[s]] asks for that [[wanted]] decides to make here, named, and the scope with them.
   * A copy for a block defined later in [[s]] is made there.
   */
  def provide(s: Stmt, scope: Scope)(wanted: Key => Boolean): (List[(Key, Id)], Scope) = {
    val later = definitions(s)
    val requests = calls(s, scope).toList.collect {
      case (f, arguments) if !arguments.flatten.exists(later.contains) => key(f, arguments, scope)
    }.flatten.distinct.filter { k =>
      !scope.copies.contains(k) && wanted(k) && !scope.within.contains(originsOf(k))
    }
    val named = requests.map { case k @ (f, arguments) =>
      k -> Id((f.name.name :: arguments.flatten.map(_.name.name)).mkString("_"))
    }
    (named, scope.copy(copies = scope.copies ++ named))
  }

  def copy(k: Key, scope: Scope): BlockLit = {
    val (f, arguments) = k
    val block = fresh(callee(f, scope).get)
    val passed = (block.bparams zip block.cparams zip arguments).collect {
      case ((param, capture), Some(g)) =>
        val argument = scope.definitions(g)
        (param, capture, Block.BlockVar(g, argument.tpe, argument.capt))
    }
    val body = substitute(block.body)(using Substitution(DB.empty,
      DB.from(passed.map { case (_, capture, variable) => capture -> variable.capt }), DB.empty,
      DB.from(passed.map { case (param, _, variable) => param.id -> variable })))
    val kept = (block.bparams zip block.cparams zip arguments).collect { case ((param, capture), None) => (param, capture) }
    val inner = scope.copy(within = scope.within + originsOf(k))
    val (outside, withOutside) = provide(body, inner) { _ => true }
    BlockLit(block.tparams, kept.map(_._2), block.vparams, kept.map(_._1),
      bind(outside, withOutside, specializing.rewrite(body)(using withOutside)))
  }

  /** Binds the copies [[named]] in order, each seeing itself and the ones before it. */
  def bind(named: List[(Key, Id)], scope: Scope, rest: Stmt): Stmt = {
    val before = scope.copy(copies = scope.copies -- named.map(_._1))
    def go(named: List[(Key, Id)], scope: Scope): Stmt = named match {
      case Nil => rest
      case (k, c) :: more =>
        val seeing = scope.copy(copies = scope.copies + (k -> c))
        Stmt.Def(c, copy(k, seeing), go(more, seeing))
    }
    go(named, before)
  }

  object specializing extends Tree.RewriteWithContext[Scope] {
    override def rewrite(s: Stmt)(using scope: Scope): Stmt = s match {
      case Stmt.Def(id, alias @ Block.BlockVar(x, _, _), body) =>
        Stmt.Def(id, alias, rewrite(body)(using scope.copy(aliases = scope.aliases + (id -> x))))

      case Stmt.Def(id, block @ (_: BlockLit | _: Block.New), body) =>
        val defined = scope.copy(definitions = scope.definitions + (id -> block))
        val (named, withCopies) = provide(body, defined) { case (f, arguments) => f == id || arguments.contains(Some(id)) }
        Stmt.Def(id, rewrite(block)(using defined), bind(named, withCopies, rewrite(body)(using withCopies)))

      case Stmt.App(callee @ Block.BlockVar(f, _, capt), targs, vargs, bargs) if bargs.nonEmpty =>
        val arguments = bargs.map {
          case Block.BlockVar(x, _, _) => Some(scope.resolve(x))
          case _ => None
        }
        key(scope.resolve(f), arguments, scope).flatMap { k => scope.copies.get(k).map(k -> _) } match {
          case Some(((_, known), c)) =>
            val unknown = (bargs zip known).collect { case (b, None) => rewrite(b) }
            val tpe = callee.tpe match {
              case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
                BlockType.Function(tparams, (cparams zip known).collect { case (cp, None) => cp }, vparams,
                  (bparams zip known).collect { case (b, None) => b }, result)
              case other => other
            }
            val captures = capt ++ (bargs zip known).collect { case (b, Some(_)) => b.capt }.flatten
            Stmt.App(Block.BlockVar(c, tpe, captures), targs, vargs.map(rewrite), unknown)
          case None => super.rewrite(s)
        }

      case other => super.rewrite(other)
    }
  }

  val empty = Scope(Map.empty, Map.empty, Map.empty, Set.empty)
  m.copy(definitions = m.definitions.map {
    case Toplevel.Def(id, block) => Toplevel.Def(id, specializing.rewrite(block)(using empty))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, specializing.rewrite(binding)(using empty))
  })
}
