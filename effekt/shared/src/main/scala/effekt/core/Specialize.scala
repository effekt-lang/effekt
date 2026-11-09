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

/** A callee and, for each of its blocks, the local definition passed, if it is one. */
type Key = (Id, List[Option[Id]])

/**
 * What a copy's callee or one of its blocks can be made anew with every round of a recursion: the [[Made]] copy is
 * new when its callee or one of its blocks is.
 */
enum Node {
  case Callee(copy: Key)
  case Argument(copy: Key, position: Int)
  case Made(copy: Key)
}

/** A definition that is not specialized, or not to the block at a position, because the copies would grow. */
enum Growing {
  case Callee(definition: Id)
  case Argument(definition: Id, position: Int)
}

/** The key that is left of [[k]] after leaving the [[growing]] positions unknown, if any is left known. */
def withoutGrowing(k: Key, growing: Set[Growing]): Option[Key] = k match {
  case (f, arguments) =>
    val kept = arguments.zipWithIndex.map { case (argument, position) =>
      argument.filter { _ => !growing.contains(Growing.Argument(f, position)) }
    }
    Option.when(!growing.contains(Growing.Callee(f)) && kept.exists(_.isDefined))((f, kept))
}

/**
 * The positions of the copies, by the definitions they are copied from, that would be made anew without end:
 *
 *   def loop(b){k} = … loop(c){ x => k(x) } …
 *
 * The copy of `loop` for a block `f` defines a new block `x => f(x)` and asks for a copy for it, which defines yet
 * another one. We follow the copies by origin: a callee and the definitions passed to it. A position of a copy is new
 * when the definition there is made fresh in the copy calling it and that copy is new itself, or when it is passed on
 * from such a position. Copies are made without end exactly when this dependency is cyclic through a fresh definition.
 * Blocks also travel into definitions and are called there:
 *
 *   def scope{action} = { def handler = new …; action(){handler} }
 *   def eval(e) = … scope{ {h} => eval(…){h} } …
 *
 * The definition at the target of such a fresh definition is then not specialized at that position anywhere, since a
 * copy for any other block would only unroll the recursion once, and we follow the copies again until nothing grows.
 */
def growing(m: ModuleDecl, functions: Set[Id]): Set[Growing] = {
  val toplevel: Map[Id, BlockLit] = m.definitions.collect { case Toplevel.Def(id, block: BlockLit) => id -> block }.toMap
  val local: Map[Id, Block] = m.definitions.flatMap {
    case Toplevel.Def(_, block) => definitionsIn(block, functions)
    case Toplevel.Val(_, binding) => definitions(binding, functions)
  }.toMap

  val aliases = mutable.Map.empty[Id, Id]
  object aliasing extends Tree.Query[Unit, Unit] {
    def empty = ()
    def combine = (_, _) => ()
    override def stmt(using Unit) = {
      case Stmt.Def(id, Block.BlockVar(x, _, _), body) if !functions.contains(x) =>
        aliases(id) = x
        query(body)
    }
  }
  m.definitions.foreach { definition => aliasing.query(definition)(using ()) }
  def resolve(id: Id): Id = aliases.get(id).map(resolve).getOrElse(id)

  def callable(f: Id, arity: Int): Option[BlockLit] = toplevel.get(f).orElse(local.get(f)).collect {
    case block: BlockLit if block.bparams.nonEmpty && block.bparams.size == arity => block
  }

  val calls = mutable.Map.empty[Id, List[(Id, List[Option[Id]])]]
  def callsIn(origin: Id, block: BlockLit): List[(Id, List[Option[Id]])] = calls.getOrElseUpdate(origin, {
    object calling extends Tree.Query[Unit, List[(Id, List[Option[Id]])]] {
      def empty = Nil
      def combine = _ ++ _
      override def stmt(using Unit) = {
        case Stmt.App(Block.BlockVar(f, _, _), targs, vargs, bargs) if bargs.nonEmpty =>
          val arguments = bargs.map {
            case Block.BlockVar(x, _, _) => Some(resolve(x))
            case _ => None
          }
          (resolve(f) -> arguments) :: combine(all(vargs, query), all(bargs, query))
      }
    }
    calling.query(block.body)(using ())
  })

  val inside = mutable.Map.empty[Id, Set[Id]]
  def insideOf(origin: Id, block: BlockLit): Set[Id] = inside.getOrElseUpdate(origin, definitions(block.body, functions).keySet)

  enum Source {
    case Fresh
    case Parameter(position: Int)
    case Around
  }

  val closures = mutable.Map.empty[Id, Map[Id, Set[Id]]].withDefaultValue(Map.empty)
  var growing = Set.empty[Growing]
  var changed = true
  while changed do {
    changed = false
    val made = mutable.LinkedHashSet.empty[Key]
    val successors = mutable.Map.empty[Node, Set[Node]].withDefaultValue(Set.empty)
    val fresh = mutable.ListBuffer.empty[(Node, Node)]
    val pending = mutable.Queue.empty[(Option[Key], Id, BlockLit, Map[Id, Set[Id]])]

    def environment(k: Key, block: BlockLit): Map[Id, Set[Id]] =
      closures(k._1) ++ (block.bparams zip k._2).collect { case (param, Some(g)) => param.id -> Set(g) }

    def visit(calling: Option[Key], origin: Id, block: BlockLit, values: Map[Id, Set[Id]]): Unit = {
      insideOf(origin, block).foreach { d =>
        val merged = (closures(d).keySet ++ values.keySet).map { x =>
          x -> (closures(d).getOrElse(x, Set.empty) ++ values.getOrElse(x, Set.empty))
        }.toMap
        if merged != closures(d) then { closures(d) = merged; changed = true }
      }
      def source(x: Id): Source = calling match {
        case Some((_, arguments)) =>
          block.bparams.zip(arguments).indexWhere { case (param, argument) => param.id == x && argument.isDefined } match {
            case -1 => if insideOf(origin, block).contains(x) then Source.Fresh else Source.Around
            case position => Source.Parameter(position)
          }
        case None => Source.Around
      }
      def support(target: Node, x: Id): Unit = calling.foreach { k =>
        source(x) match {
          case Source.Fresh => fresh += (Node.Made(k) -> target); successors(Node.Made(k)) += target
          case Source.Parameter(position) => successors(Node.Argument(k, position)) += target
          case Source.Around => successors(Node.Callee(k)) += target
        }
      }
      callsIn(origin, block).foreach { case (f, arguments) =>
        val callees = values.getOrElse(f, if toplevel.contains(f) || local.contains(f) then Set(f) else Set.empty)
        val known = arguments.map {
          case Some(x) => values.getOrElse(x, if local.contains(x) then Set(x) else Set.empty).toList.map(Some(_)) match {
            case Nil => List(None)
            case origins => origins
          }
          case None => List(None)
        }
        val combinations = known.foldRight(List(List.empty[Option[Id]])) { (choices, rest) =>
          for { choice <- choices; others <- rest } yield choice :: others
        }
        for {
          callee <- callees.toList
          target <- callable(callee, arguments.size).toList
          combination <- combinations
          k <- withoutGrowing((callee, combination), growing)
        } {
          support(Node.Callee(k), f)
          (arguments zip k._2).zipWithIndex.foreach {
            case ((Some(x), Some(_)), position) => support(Node.Argument(k, position), x)
            case _ => ()
          }
          if made.add(k) then pending.enqueue((Some(k), callee, target, environment(k, target)))
        }
      }
    }

    toplevel.foreach { case (id, block) => pending.enqueue((None, id, block, Map.empty)) }
    m.definitions.foreach {
      case Toplevel.Val(id, binding) => pending.enqueue((None, id, BlockLit(Nil, Nil, Nil, Nil, binding), Map.empty))
      case _ => ()
    }
    while pending.nonEmpty do {
      val (calling, origin, block, values) = pending.dequeue()
      visit(calling, origin, block, values)
    }

    made.foreach { k =>
      successors(Node.Callee(k)) += Node.Made(k)
      k._2.zipWithIndex.foreach { case (argument, position) =>
        if argument.isDefined then successors(Node.Argument(k, position)) += Node.Made(k)
      }
    }
    val nodes = successors.keySet ++ successors.values.flatten
    val component = components(nodes, successors).zipWithIndex.flatMap {
      case (members, number) => members.map(_ -> number)
    }.toMap
    val cyclic = fresh.collect {
      case (made, target @ Node.Callee((f, _))) if component(made) == component(target) => Growing.Callee(f)
      case (made, target @ Node.Argument((f, _), position)) if component(made) == component(target) => Growing.Argument(f, position)
    }.toSet
    if !cyclic.subsetOf(growing) then { growing = growing ++ cyclic; changed = true }
  }
  growing
}

/** The local definitions inside [[block]]. */
def definitionsIn(block: Block, functions: Set[Id]): Map[Id, Block] = block match {
  case BlockLit(_, _, _, _, body) => definitions(body, functions)
  case Block.New(impl) => impl.operations.flatMap { operation => definitions(operation.body, functions) }.toMap
  case _ => Map.empty
}

// Specialize
// ----------

/**
 * What is in scope: the local definitions of literals, objects, and toplevel functions, the aliases `def x = y`,
 * and where to report the copies the calls ask for.
 */
case class Scope(
  definitions: Map[Id, Block],
  aliases: Map[Id, Id],
  reports: mutable.ListBuffer[Key]
) {
  def resolve(id: Id): Id = aliases.get(id).map(resolve).getOrElse(id)
}

/**
 * One copy per callee and local definitions passed to it, right after the last of them:
 *
 *   [[ f(e){g, h} ]] = f_g(e){h}   reporting the copy `f_g`
 *   [[ def g = b; s ]] = def g = [[ b ]]; def f_g(x){h} = [[ body of f ]][k := g]; [[ s ]]   if `[[ s ]]` reports `f_g`
 *
 * where `g` is a local definition and `h` is not. Reports travel outwards to the definition of the last of the blocks
 * (or the callee, if it is local), where the copy is made. The body of a copy reports further copies: those made at the
 * same definition are made before it, unless they are still being made themselves, which closes a cycle of mutual
 * recursion; then the copies made since go around, at the beginning of the copy the cycle returns to.
 */
def specialize(m: ModuleDecl, functions: Set[Id], growing: Set[Growing]): ModuleDecl = {
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

  /** The key of a call, given what is defined: arguments that are not local definitions are unknown. */
  def key(f: Id, arguments: List[Option[Id]], defined: Map[Id, Block]): Option[Key] = {
    val known = arguments.map(_.filter(defined.contains))
    if !callee(f, defined).exists(_.bparams.size == arguments.size) then None
    else withoutGrowing((originOf(f), known.map(_.map(originOf))), growing).map { case (_, kept) =>
      (f, (known zip kept).map { case (argument, origin) => argument.filter(_ => origin.isDefined) })
    }
  }

  /** The local definitions the copy for [[k]] needs to see. */
  def needs(k: Key): Set[Id] = k match {
    case (f, arguments) => arguments.flatten.toSet ++ Option.when(!toplevel.contains(f))(f)
  }

  val names = mutable.Map.empty[Key, Id]
  def name(k: Key): Id = names.getOrElseUpdate(k, k match {
    case (f, arguments) => Id((f.name.name :: arguments.flatten.map(_.name.name)).mkString("_"))
  })

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
    BlockLit(block.tparams, kept.map(_._2), block.vparams, kept.map(_._1), specializing.rewrite(body)(using scope))
  }

  /**
   * Makes the copies [[reported]] at the definition of [[id]] asks for there, and the ones they ask for in turn,
   * each before the copies calling it, or around them if they call it back. The other reports travel on.
   */
  def make(id: Id, scope: Scope, reported: List[Key]): (Stmt => Stmt, List[Key]) = {
    def here(k: Key): Boolean = needs(k).contains(id) && needs(k).forall(scope.definitions.contains)

    val made = mutable.LinkedHashMap.empty[Key, BlockLit]
    val around = mutable.Map.empty[Key, Key]
    val inside = mutable.Map.empty[Key, mutable.ListBuffer[(Key, BlockLit)]]
    val making = mutable.ArrayBuffer.empty[Key]
    val outside = mutable.ListBuffer.empty[Key]

    def build(k: Key): Unit = {
      making += k
      val reports = mutable.ListBuffer.empty[Key]
      val block = copy(k, scope.copy(reports = reports))
      reports.distinct.foreach { r =>
        if !here(r) then outside += r
        else if r != k && !made.contains(r) then making.indexOf(r) match {
          case -1 => build(r)
          case cycle => making.drop(cycle + 1).foreach { member => if !around.contains(member) then around(member) = r }
        }
      }
      making.remove(making.size - 1)
      around.get(k) match {
        case Some(enclosing) => inside.getOrElseUpdate(enclosing, mutable.ListBuffer.empty) += (k -> block)
        case None => made(k) = block
      }
    }
    reported.distinct.foreach { k => if !here(k) then outside += k else if !made.contains(k) then build(k) }

    def enclose(k: Key, block: BlockLit): BlockLit = inside.get(k) match {
      case None => block
      case Some(members) =>
        val renamed = members.toList.map { case (member, body) => (member, Id(name(member)), enclose(member, body)) }
        val renaming = renamed.map { case (member, renamed, body) => name(member) -> Block.BlockVar(renamed, body.tpe, body.capt) }
        val body = renamed.foldRight(block.body) { case ((_, renamed, body), rest) => Stmt.Def(renamed, body, rest) }
        block.copy(body = substitute(body)(using Substitution(DB.empty, DB.empty, DB.empty, DB.from(renaming))))
    }
    val bind = (rest: Stmt) => made.toList.foldRight(rest) { case ((k, block), rest) => Stmt.Def(name(k), enclose(k, block), rest) }
    (bind, outside.toList)
  }

  object specializing extends Tree.RewriteWithContext[Scope] {
    override def rewrite(s: Stmt)(using scope: Scope): Stmt = s match {
      case Stmt.Def(id, alias @ Block.BlockVar(x, _, _), body) if !functions.contains(x) =>
        Stmt.Def(id, alias, rewrite(body)(using scope.copy(aliases = scope.aliases + (id -> x))))

      case Stmt.Def(id, block @ (_: BlockLit | _: Block.New | _: Block.BlockVar), body) =>
        val defined = Scope(scope.definitions + (id -> block), scope.aliases, mutable.ListBuffer.empty)
        val rewrittenBlock = rewrite(block)
        val rewrittenBody = rewrite(body)(using defined)
        val (bind, outside) = make(id, defined, defined.reports.toList)
        scope.reports ++= outside
        Stmt.Def(id, rewrittenBlock, bind(rewrittenBody))

      case Stmt.App(callee @ Block.BlockVar(f, _, capt), targs, vargs, bargs) if bargs.nonEmpty =>
        val arguments = bargs.map {
          case Block.BlockVar(x, _, _) => Some(scope.resolve(x))
          case _ => None
        }
        key(scope.resolve(f), arguments, scope.definitions) match {
          case Some(k @ (_, known)) =>
            scope.reports += k
            val unknown = (bargs zip known).collect { case (b, None) => rewrite(b) }
            val tpe = callee.tpe match {
              case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
                BlockType.Function(tparams, (cparams zip known).collect { case (cp, None) => cp }, vparams,
                  (bparams zip known).collect { case (b, None) => b }, result)
              case other => other
            }
            val captures = capt ++ (bargs zip known).collect { case (b, Some(_)) => b.capt }.flatten
            Stmt.App(Block.BlockVar(name(k), tpe, captures), targs, vargs.map(rewrite), unknown)
          case None => super.rewrite(s)
        }

      case other => super.rewrite(other)
    }
  }

  def scope = Scope(Map.empty, Map.empty, mutable.ListBuffer.empty)
  m.copy(definitions = m.definitions.map {
    case Toplevel.Def(id, block) => Toplevel.Def(id, specializing.rewrite(block)(using scope))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, specializing.rewrite(binding)(using scope))
  })
}
