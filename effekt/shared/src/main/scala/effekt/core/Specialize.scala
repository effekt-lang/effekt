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
 * [[names]] gives every literal block argument a name, [[growing]] finds the recursive closures, [[specialize]] makes
 * the copies, and [[optimizer.cleanup]] removes what is left unused and inlines what is called once.
 */
def specialization(m: ModuleDecl): ModuleDecl = {
  val functions = m.definitions.collect { case Toplevel.Def(id, _: BlockLit) => id }.toSet
  val named = names(m, functions)
  optimizer.cleanup(specialize(named, functions, growing(named, functions)))
}

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
 * [[ f(…){ b } ]] = def g = [[ b ]]; f(…){ g }   for every block argument `b` that is a literal, an object, or one
 * of the toplevel [[functions]]
 */
def names(m: ModuleDecl, functions: Set[Id]): ModuleDecl = {
  object naming extends Tree.Rewrite {
    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.App(callee, targs, vargs, bargs) =>
        val named = bargs.map {
          case block @ (_: BlockLit | _: Block.New) =>
            val id = Id("f")
            (Some(id -> rewrite(block)), Block.BlockVar(id, block.tpe, block.capt))
          case block @ Block.BlockVar(f, tpe, capt) if functions.contains(f) =>
            val id = Id(f.name.name)
            (Some(id -> block), Block.BlockVar(id, tpe, capt))
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


// Recursive closures
// ------------------

/** The local definitions of literals, objects, and toplevel [[functions]] in [[s]]. */
def definitions(s: Stmt, functions: Set[Id]): Map[Id, Block] = {
  object collecting extends Tree.Query[Unit, Map[Id, Block]] {
    def empty = Map.empty
    def combine = _ ++ _
    override def stmt(using Unit) = {
      case Stmt.Def(id, block @ (_: BlockLit | _: Block.New), body) => Map(id -> block) ++ query(block) ++ query(body)
      case Stmt.Def(id, block @ Block.BlockVar(f, _, _), body) if functions.contains(f) => Map(id -> block) ++ query(body)
    }
  }
  collecting.query(s)(using ())
}

/**
 * The strongly connected components of a graph, each after the components it reaches.
 */
def components[A](nodes: Iterable[A], successors: A => Iterable[A]): List[List[A]] = {
  val index = mutable.Map.empty[A, Int]
  val lowest = mutable.Map.empty[A, Int]
  val stack = mutable.Stack.empty[A]
  val found = mutable.ListBuffer.empty[List[A]]
  def connect(node: A): Unit = {
    index(node) = index.size
    lowest(node) = index(node)
    stack.push(node)
    successors(node).foreach { next =>
      if !index.contains(next) then {
        connect(next)
        lowest(node) = lowest(node) min lowest(next)
      } else if stack.contains(next) then lowest(node) = lowest(node) min index(next)
    }
    if lowest(node) == index(node) then {
      val members = mutable.ListBuffer(stack.pop())
      while members.last != node do members += stack.pop()
      found += members.toList
    }
  }
  nodes.foreach { node => if !index.contains(node) then connect(node) }
  found.toList
}

/**
 * The blocks of definitions that get passed, within their recursive group, a block defined there, by definition
 * and position:
 *
 *   def loop(b){k} = … loop(c){ x => k(x) } …
 *
 * Specializing `loop` to a block would ask for a copy for the next block, which captures the one before, without end,
 * so we leave these blocks unknown from the start.
 */
def growing(m: ModuleDecl, toplevel: Set[Id]): Set[(Id, Int)] = {
  val functions = mutable.LinkedHashMap.empty[Id, BlockLit]
  object collecting extends Tree.Query[Unit, Unit] {
    def empty = ()
    def combine = (_, _) => ()
    override def stmt(using Unit) = {
      case Stmt.Def(id, block: BlockLit, body) =>
        functions(id) = block
        query(block); query(body)
    }
  }
  m.definitions.foreach {
    case Toplevel.Def(id, block: BlockLit) =>
      functions(id) = block
      collecting.query(block)(using ())
    case other => collecting.query(other)(using ())
  }

  def mentioned(s: Stmt): Set[Id] = {
    object mentions extends Tree.Query[Unit, Set[Id]] {
      def empty = Set.empty
      def combine = _ ++ _
      override def block(using Unit) = {
        case Block.BlockVar(id, _, _) if functions.contains(id) => Set(id)
      }
    }
    mentions.query(s)(using ())
  }
  val successors = functions.map { case (f, block) => f -> mentioned(block.body) }

  val group = components(functions.keys, successors).zipWithIndex.flatMap {
    case (members, number) => members.map(_ -> number)
  }.toMap

  functions.toList.flatMap { case (f, block) =>
    val defined = definitions(block.body, toplevel)
    object passing extends Tree.Query[Unit, Set[(Id, Int)]] {
      def empty = Set.empty
      def combine = _ ++ _
      override def stmt(using Unit) = {
        case Stmt.App(Block.BlockVar(g, _, _), targs, vargs, bargs) if group.get(g) == group.get(f) =>
          val passed = bargs.zipWithIndex.collect {
            case (Block.BlockVar(x, _, _), position) if defined.contains(x) => (g, position)
            case (_: BlockLit | _: Block.New, position) => (g, position)
          }.toSet
          combine(passed, combine(all(vargs, query), all(bargs, query)))
      }
    }
    passing.query(block.body)(using ())
  }.toSet
}


// Specialize
// ----------

/** A callee and, for each of its blocks, the local definition passed, if it is one. */
type Key = (Id, List[Option[Id]])

/**
 * What is in scope: the local definitions of literals and objects, the aliases `def x = y`, the copies, and the
 * callees of the copies we are in, by where their names come from.
 */
case class Scope(
  definitions: Map[Id, Block],
  aliases: Map[Id, Id],
  copies: Map[Key, Id],
  within: Set[Id]
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
 * The copies that copies ask for with blocks defined there are made there too, each after the copies it calls,
 * so that all calls share them. Mutually recursive copies cannot come after each other, so each copy entering such
 * a group gets the copies of the rest of the group at its beginning, where they see it:
 *
 *   def ping_g(x) = { def pong_g(y) = … ping_g(…) …; … pong_g(…) … }
 *
 * A copy of `f` within a copy of `f` would be built up recursively, so we do not make it.
 */
def specialize(m: ModuleDecl, functions: Set[Id], growing: Set[(Id, Int)]): ModuleDecl = {
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

  /** The definition taking blocks that [[f]] stands for. */
  def callee(f: Id, defined: Map[Id, Block]): Option[BlockLit] =
    defined.get(f).orElse(toplevel.get(f)).collect {
      case block: BlockLit if block.bparams.nonEmpty => block
    }

  /** The keys of the calls in [[s]], by what the names stand for; we only know which names are definitions later. */
  def calls(s: Stmt, scope: Scope): Set[(Id, List[Option[Id]])] = {
    object collecting extends Tree.Query[Map[Id, Id], Set[(Id, List[Option[Id]])]] {
      def empty = Set.empty
      def combine = _ ++ _
      def resolve(id: Id)(using aliases: Map[Id, Id]): Id = aliases.get(id).map(resolve).getOrElse(scope.resolve(id))
      override def stmt(using aliases: Map[Id, Id]) = {
        case Stmt.Def(id, Block.BlockVar(x, _, _), body) if !functions.contains(x) => query(body)(using aliases + (id -> x))
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
  def key(f: Id, arguments: List[Option[Id]], defined: Map[Id, Block]): Option[Key] = {
    val known = arguments.zipWithIndex.map { case (argument, position) =>
      argument.filter { g => defined.contains(g) && !growing.contains((originOf(f), position)) }
    }
    Option.when(known.exists(_.isDefined) && callee(f, defined).exists(_.bparams.size == arguments.size))((f, known))
  }

  /**
   * The copies asked for in [[s]], also by the copies themselves, with the copies each asks for in turn; we look
   * through the copies for blocks defined later in [[s]] too, since they may ask for copies for blocks defined here.
   */
  val asked = mutable.Map.empty[Key, List[Key]]
  def requests(s: Stmt, defined: Map[Id, Block], scope: Scope): List[Key] =
    calls(s, scope).toList.flatMap { case (f, arguments) => key(f, arguments, defined) }.distinct

  /**
   * The copies to make here, of those [[s]] asks for: the ones whose callee and blocks are all in scope and that
   * [[here]] decides for, each after the ones it calls, named, and the scope with them.
   * Of a group of mutually recursive copies, only the entries, which are asked for from outside of the group, are
   * made here; each gets the rest at its beginning.
   */
  def provide(s: Stmt, scope: Scope)(here: Key => Boolean): (List[(Key, Id)], Scope) = {
    def fresh(k: Key): Boolean = !scope.copies.contains(k) && !scope.within.contains(originOf(k._1))
    val successors = mutable.LinkedHashMap.empty[Key, List[Key]]
    def known(k: Key, defined: Map[Id, Block]): Boolean =
      callee(k._1, defined).isDefined && k._2.flatten.forall(defined.contains)
    def explore(k: Key, defined: Map[Id, Block]): Unit =
      if !successors.contains(k) then {
        successors(k) = Nil
        val body = substituted(k, defined)
        val inner = defined ++ definitions(body, functions)
        val next = asked.getOrElseUpdate(k, requests(body, inner, scope)).filter { k => fresh(k) && known(k, inner) }
        successors(k) = next
        next.foreach(explore(_, inner))
      }
    val defined = scope.definitions ++ definitions(s, functions)
    val roots = requests(s, defined, scope).filter(fresh)
    roots.foreach(explore(_, defined))
    def visible(k: Key): Boolean = known(k, scope.definitions)
    val groups = components(roots, successors)
    val group = groups.flatMap { members => members.map(_ -> members) }.toMap
    def entry(k: Key): Boolean = roots.contains(k) || successors.exists {
      case (caller, called) => called.contains(k) && !group(k).contains(caller)
    }
    val ordered = groups.flatMap { members =>
      (if members.size == 1 then members else members.filter(entry)).filter { k => visible(k) && here(k) }
    }
    val named = ordered.map { case k @ (f, arguments) =>
      k -> Id((f.name.name :: arguments.flatten.map(_.name.name)).mkString("_"))
    }
    (named, scope.copy(copies = scope.copies ++ named))
  }

  /** The body of the callee of [[k]] with the blocks of [[k]] put in. */
  def substituted(k: Key, defined: Map[Id, Block]): Stmt = {
    val (f, arguments) = k
    val block = callee(f, defined).get
    val passed = (block.bparams zip arguments).collect {
      case (param, Some(g)) =>
        val argument = defined(g)
        param.id -> Block.BlockVar(g, argument.tpe, argument.capt)
    }
    substitute(block.body)(using Substitution(DB.empty, DB.empty, DB.empty, DB.from(passed)))
  }

  def copy(k: Key, scope: Scope): BlockLit = {
    val (f, arguments) = k
    val block = fresh(callee(f, scope.definitions).get)
    val passed = (block.bparams zip block.cparams zip arguments).collect {
      case ((param, capture), Some(g)) =>
        val argument = scope.definitions(g)
        (param, capture, Block.BlockVar(g, argument.tpe, argument.capt))
    }
    val body = substitute(block.body)(using Substitution(DB.empty,
      DB.from(passed.map { case (_, capture, variable) => capture -> variable.capt }), DB.empty,
      DB.from(passed.map { case (param, _, variable) => param.id -> variable })))
    val kept = (block.bparams zip block.cparams zip arguments).collect { case ((param, capture), None) => (param, capture) }
    val inner = scope.copy(within = scope.within + originOf(f))
    val (group, withGroup) = provide(body, inner) { _ => true }
    BlockLit(block.tparams, kept.map(_._2), block.vparams, kept.map(_._1),
      bind(group, withGroup, specializing.rewrite(body)(using withGroup)))
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
      case Stmt.Def(id, alias @ Block.BlockVar(x, _, _), body) if !functions.contains(x) =>
        Stmt.Def(id, alias, rewrite(body)(using scope.copy(aliases = scope.aliases + (id -> x))))

      case Stmt.Def(id, block @ (_: BlockLit | _: Block.New | _: Block.BlockVar), body) =>
        val defined = scope.copy(definitions = scope.definitions + (id -> block))
        val (named, withCopies) = provide(body, defined) { case (f, arguments) => f == id || arguments.contains(Some(id)) }
        Stmt.Def(id, rewrite(block)(using defined), bind(named, withCopies, rewrite(body)(using withCopies)))

      case Stmt.App(callee @ Block.BlockVar(f, _, capt), targs, vargs, bargs) if bargs.nonEmpty =>
        val arguments = bargs.map {
          case Block.BlockVar(x, _, _) => Some(scope.resolve(x))
          case _ => None
        }
        key(scope.resolve(f), arguments, scope.definitions).flatMap { k => scope.copies.get(k).map(k -> _) } match {
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
