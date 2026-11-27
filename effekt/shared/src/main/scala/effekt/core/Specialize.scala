package effekt
package core

import effekt.context.Context
import effekt.PhaseResult.CoreTransformed
import effekt.core.substitutions.{ Substitution, substitute }
import effekt.util.DB

import scala.collection.mutable

/**
 * Specializes every definition taking blocks to the blocks passed to it, like [[Mono]] does for types:
 * [[names]] gives every block argument a name, [[recursive]] finds the definitions that are recursively built, which we
 * do not specialize to, and [[specialize]] makes one copy per code called and passed.
 */
def specialization(m: ModuleDecl): ModuleDecl = {
  val named = names(m)
  specialize(named, recursive(named))
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
 *   [[ f(…){ b } ]]  =  def g = [[ b ]]; f(…){ g }    for every block argument `b` that is not a variable
 *   [[ b.m(…){ c } ]]  =  def g = [[ b ]]; def h = [[ c ]]; g.m(…){ h }    likewise, and so for calls
 */
def names(m: ModuleDecl): ModuleDecl = {
  object naming extends Tree.Rewrite {
    def named(block: Block): (Option[(Id, Block)], Block) = block match {
      case variable: Block.BlockVar => (None, variable)
      case other =>
        val id = Id("b")
        (Some(id -> rewrite(other)), Block.BlockVar(id, other.tpe, other.capt))
    }
    def bind(definitions: List[Option[(Id, Block)]], body: Stmt): Stmt =
      definitions.flatten.foldRight(body) { case ((id, block), rest) => Stmt.Def(id, block, rest) }

    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.App(callee, targs, vargs, bargs) =>
        val (definition, variable) = named(callee)
        val arguments = bargs.map(named)
        bind(definition :: arguments.map(_._1), Stmt.App(variable, targs, vargs.map(rewrite), arguments.map(_._2)))
      case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
        val (definition, variable) = named(callee)
        val arguments = bargs.map(named)
        bind(definition :: arguments.map(_._1), Stmt.Invoke(variable, method, methodTpe, targs, vargs.map(rewrite), arguments.map(_._2)))
      case other => super.rewrite(other)
    }
  }
  naming.rewrite(m)
}


// Recursive
// ---------

/** The function called: a definition, or the operation of the object it defines. */
def implementation(block: Block, method: Option[Id]): Option[BlockLit] = (block, method) match {
  case (literal: BlockLit, None) => Some(literal)
  case (Block.New(Implementation(_, operations)), Some(name)) => operations.collectFirst {
    case Operation(operation, tparams, cparams, vparams, bparams, body) if operation == name =>
      BlockLit(tparams, cparams, vparams, bparams, body)
  }
  case _ => None
}

/**
 * The definitions that are recursively built. A block argument flows into the parameter of each definition its callee
 * can be. A definition closes over the definitions that flow into the blocks it refers to, and it is recursively built
 * when it closes over itself, possibly through others. A copy for it would make it anew within itself without end.
 */
def recursive(m: ModuleDecl): Set[Id] = {
  val defined = mutable.Map.empty[Id, Block]
  val calls = mutable.ListBuffer.empty[(Id, Option[Id], List[Block])]

  object collecting extends Tree.Query[Unit, Unit] {
    def empty = ()
    def combine = (_, _) => ()
    override def stmt(using Unit) = {
      case Stmt.Def(id, block, rest) =>
        defined(id) = block
        query(block)
        query(rest)
      case Stmt.App(Block.BlockVar(callee, _, _), targs, vargs, bargs) =>
        calls += ((callee, None, bargs))
        all(vargs, query)
        all(bargs, query)
      case Stmt.Invoke(Block.BlockVar(receiver, _, _), method, methodTpe, targs, vargs, bargs) =>
        calls += ((receiver, Some(method), bargs))
        all(vargs, query)
        all(bargs, query)
    }
  }
  m.definitions.foreach {
    case Toplevel.Def(id, block) =>
      defined(id) = block
      collecting.query(block)(using ())
    case Toplevel.Val(id, binding) => collecting.query(binding)(using ())
  }

  val flowing = mutable.Map.empty[Id, Set[Id]].withDefaultValue(Set.empty)
  def value(x: Id): Set[Id] = defined.get(x) match {
    case Some(Block.BlockVar(target, _, _)) => value(target)
    case Some(definition) => Set(x)
    case None => flowing(x)
  }

  var changed = true
  while changed do {
    changed = false
    calls.foreach { case (callee, method, arguments) =>
      value(callee).foreach { definition =>
        implementation(defined(definition), method).foreach { literal =>
          if literal.bparams.size == arguments.size then (literal.bparams zip arguments).foreach {
            case (param, Block.BlockVar(argument, _, _)) if !value(argument).subsetOf(flowing(param.id)) =>
              flowing(param.id) = flowing(param.id) ++ value(argument)
              changed = true
            case _ => ()
          }
        }
      }
    }
  }

  val toplevel = m.definitions.map(_.id).toSet
  def closedOver(definition: Id): Set[Id] =
    defined(definition).free.blocks.iterator.filter { case (reference, _) => reference != definition }.flatMap {
      case (reference, _) => value(reference)
    }.filterNot(toplevel.contains).toSet
  components(defined.keySet.filterNot(toplevel.contains), closedOver).flatMap {
    case List(single) => Option.when(closedOver(single).contains(single))(single).toList
    case members => members
  }.toSet
}

/** The strongly connected components of a graph. */
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


// Specialize
// ----------

/** The code of a block: the definition it stems from, and the code of the blocks it closes over that we know. */
case class Code(source: Id, closed: List[(Id, Code)])

/**
 * The code called, the operation if it is an object, for each block parameter the code passed, or none when it stays
 * a parameter, and which of the values and blocks all this code closes over are the same.
 */
case class Key(callee: Code, method: Option[Id], arguments: List[Option[Code]], values: List[Int], blocks: List[Int])

/** For each element the position of its first occurrence, which tells apart what is passed once from what is passed again. */
def firsts[A](elements: List[A]): List[Int] = elements.map(elements.indexOf)

/**
 * A block and what it closes over: the values and the blocks we do not know, which a copy takes as parameters, and the
 * blocks we know, which a copy makes anew. All are ordered by where they stem from, which is the same wherever this
 * code occurs.
 */
case class Closure(variable: Id, source: Id, block: Block, values: List[Expr.ValueVar], blocks: List[Block.BlockVar], closed: List[(Block.BlockVar, Closure)]) {
  def code(slot: Id => Id): Code = Code(source, closed.map { case (free, closure) => slot(free.id) -> closure.code(slot) })
  def passedValues: List[Expr.ValueVar] = values ++ closed.flatMap { case (_, closure) => closure.passedValues }
  def passedBlocks: List[Block.BlockVar] = blocks ++ closed.flatMap { case (_, closure) => closure.passedBlocks }
}

/** The code of a closure made anew, abstracted over what it closes over, and how its block refers to all of this. */
case class Recreated(definitions: List[(Id, Block)], variable: Block.BlockVar, vparams: List[ValueParam], cparams: List[Id], bparams: List[BlockParam], renewing: Substitution)

/**
 * Walks the program and replaces every call that passes blocks we know by a call to a copy, like Rust does for
 * closures. Callee and arguments are treated alike. The copy is at the top level, makes their code anew, and takes
 * what they close over as parameters:
 *
 *   def a = { y => x + y }; f(e){a, h}  =  f_a(e, x, v){h, u}    where we do not know `h`
 *
 *   def f_a(z, x, v){h, u} = { def f = …v…u…; def a = { y => x + y }; [[ body of f ]] }
 *
 * The code of a block is the definition it stems from together with the code of the blocks it closes over, so the
 * copy makes those anew as well, and only takes the values and the blocks we do not know. There is one copy per code,
 * however many blocks have it. We do not know a block that is recursively built. The definitions themselves stay.
 */
def specialize(m: ModuleDecl, recursive: Set[Id]): ModuleDecl = {
  val origin = mutable.Map.empty[Id, Id]
  def originOf(id: Id): Id = origin.get(id).map(originOf).getOrElse(id)
  def fresh(id: Id): Id = {
    val renamed = Id(id)
    origin(renamed) = id
    renamed
  }

  val sources = mutable.Map.empty[Id, Id]
  def sourceOf(id: Id): Id = sources.getOrElse(id, origin.get(id).map(sourceOf).getOrElse(id))

  val global: Set[Id] = m.definitions.map(_.id).toSet ++ m.externs.collect { case extern: Extern.Def => extern.id }

  val defined = mutable.Map.empty[Id, Block]
  m.definitions.foreach {
    case Toplevel.Def(id, block) => defined(id) = block
    case Toplevel.Val(id, binding) => ()
  }

  val copies = mutable.Map.empty[Key, Id]
  val finished = mutable.ListBuffer.empty[Toplevel.Def]

  val nothing = Substitution(DB.empty, DB.empty, DB.empty, DB.empty)

  def resolve(id: Id): Id = defined.get(id) match {
    case Some(Block.BlockVar(target, _, _)) => resolve(target)
    case _ => id
  }

  def closure(variable: Id): Option[Closure] = defined.get(variable) match {
    case Some(block: (BlockLit | Block.New)) if !recursive.contains(sourceOf(variable)) =>
      if global.contains(variable) then Some(Closure(variable, variable, block, Nil, Nil, Nil)) else {
        val free = block.free
        val values: List[Expr.ValueVar] = free.values.iterator.toList.collect { case (id, tpe) if !global.contains(id) => Expr.ValueVar(id, tpe) }
        val references: List[Block.BlockVar] = free.blocks.iterator.toList.collect {
          case (id, (tpe, capt)) if id != variable => Block.BlockVar(id, tpe, capt)
        }
        val closures = references.map { reference => reference -> closure(resolve(reference.id)) }
        val closed = closures.collect { case (reference, Some(known)) => reference -> known }
        val blocks = closures.collect { case (reference, None) if !global.contains(reference.id) => reference }
        val slots = (values.map(_.id) ++ blocks.map(_.id) ++ closed.map { case (reference, _) => reference.id }).map(originOf)
        Option.when(slots.distinct.size == slots.size) {
          Closure(variable, sourceOf(variable), block,
            values.sortBy { value => originOf(value.id).id },
            blocks.sortBy { block => originOf(block.id).id },
            closed.sortBy { case (reference, _) => originOf(reference.id).id })
        }
      }
    case _ => None
  }

  def renewed(block: Block, renewing: Substitution): Block = {
    val renamer = Renamer()
    val renamed = renamer.rewrite(block)
    renamer.renamed.foreach { case (renamed, old) => origin(renamed) = old }
    substitute(renamed)(using renewing)
  }

  def recreate(closure: Closure): Recreated =
    if global.contains(closure.variable) then
      Recreated(Nil, Block.BlockVar(closure.variable, closure.block.tpe, closure.block.capt), Nil, Nil, Nil, nothing)
    else {
      val vparams = closure.values.map { value => ValueParam(fresh(value.id), value.annotatedType) }
      val bparams = closure.blocks.map { block =>
        val id = fresh(block.id)
        (id, BlockParam(id, block.annotatedTpe, Set(id)))
      }
      val children = closure.closed.map { case (reference, child) => reference -> recreate(child) }
      val recaptured: List[(Captures, Captures)] =
        (closure.blocks zip bparams).map { case (block, (capture, _)) => block.annotatedCapt -> Set(capture) } ++
          children.map { case (reference, child) => reference.annotatedCapt -> child.variable.annotatedCapt }
      val captureSubstitution = DB.from(recaptured.flatMap(_._1).distinct.map { capture =>
        capture -> recaptured.collect { case (old, renewed) if old.contains(capture) => renewed }.flatten.toSet
      })
      val self = fresh(closure.variable)
      sources(self) = closure.source
      val selfCaptures = substitute(closure.block.capt)(using Substitution(DB.empty, captureSubstitution, DB.empty, DB.empty))
      val renewing = Substitution(DB.empty, captureSubstitution,
        DB.from((closure.values zip vparams).map { case (value, param) => value.id -> Expr.ValueVar(param.id, param.tpe) }),
        DB.from(
          (closure.blocks zip bparams).map { case (block, (_, param)) => block.id -> Block.BlockVar(param.id, param.tpe, param.capt) } ++
            children.map { case (reference, child) => reference.id -> child.variable } ++
            List(closure.variable -> Block.BlockVar(self, closure.block.tpe, selfCaptures))))
      Recreated(
        children.flatMap { case (_, child) => child.definitions } :+ (self -> renewed(closure.block, renewing)),
        Block.BlockVar(self, closure.block.tpe, selfCaptures),
        vparams ++ children.flatMap { case (_, child) => child.vparams },
        bparams.map(_._1) ++ children.flatMap { case (_, child) => child.cparams },
        bparams.map(_._2) ++ children.flatMap { case (_, child) => child.bparams },
        renewing)
    }

  def copy(callee: Closure, method: Option[Id], arguments: List[Option[Closure]]): BlockLit = {
    val target = recreate(callee)
    val literal = implementation(renewed(callee.block, target.renewing), method).get
    val parameters = literal.cparams zip literal.bparams zip arguments
    val passed = parameters.collect { case ((capture, param), Some(closure)) =>
      val recreated = recreate(closure)
      if !global.contains(closure.variable) then origin(recreated.variable.id) = param.id
      (capture, param, recreated)
    }
    val kept = parameters.collect { case (param, None) => param }
    val body = substitute(literal.body)(using Substitution(DB.empty,
      DB.from(passed.map { case (capture, _, recreated) => capture -> recreated.variable.capt }), DB.empty,
      DB.from(passed.map { case (_, param, recreated) => param.id -> recreated.variable })))
    val recreations = target :: passed.map { case (_, _, recreated) => recreated }
    val closures = callee :: arguments.flatten
    val vparams = recreations.flatMap(_.vparams)
    val bparams = recreations.flatMap(_.bparams)
    val sameValues = firsts(closures.flatMap(_.passedValues).map(_.id))
    val sameBlocks = firsts(closures.flatMap(_.passedBlocks).map(_.id))
    def once[A](params: List[A], same: List[Int]): List[A] = (params zip same).zipWithIndex.collect { case ((param, first), index) if first == index => param }
    def again[A](params: List[A], same: List[Int]): List[(A, A)] = (params zip same).zipWithIndex.collect { case ((param, first), index) if first != index => param -> params(first) }
    val merged = substitute(recreations.flatMap(_.definitions).foldRight(body) { case ((id, block), rest) => Stmt.Def(id, block, rest) })(using Substitution(DB.empty,
      DB.from(again(bparams, sameBlocks).map { case (param, first) => param.id -> Set(first.id) }),
      DB.from(again(vparams, sameValues).map { case (param, first) => param.id -> Expr.ValueVar(first.id, first.tpe) }),
      DB.from(again(bparams, sameBlocks).map { case (param, first) => param.id -> Block.BlockVar(first.id, first.tpe, first.capt) })))
    BlockLit(literal.tparams,
      kept.map(_._1) ++ once(bparams, sameBlocks).map(_.id),
      literal.vparams ++ once(vparams, sameValues),
      kept.map(_._2) ++ once(bparams, sameBlocks),
      merged)
  }

  object specializing extends Tree.Rewrite {

    def specialized(called: Id, method: Option[Id], targs: List[ValueType], vargs: List[Expr], bargs: List[Block]): Option[Stmt] = {
      val variables = bargs.collect { case Block.BlockVar(x, _, _) => x }
      val arguments = variables.map { x => closure(resolve(x)) }
      for {
        callee <- closure(resolve(called))
        literal <- implementation(callee.block, method)
        if literal.bparams.size == bargs.size && variables.size == bargs.size && arguments.exists(_.nonEmpty)
      } yield {
        val closures = callee :: arguments.flatten
        val values = closures.flatMap(_.passedValues)
        val blocks = closures.flatMap(_.passedBlocks)
        val key = Key(callee.code(originOf), method, arguments.map(_.map(_.code(originOf))), firsts(values.map(_.id)), firsts(blocks.map(_.id)))
        val id = copies.getOrElse(key, {
          val id = Id((callee.variable.name.name :: method.map(_.name.name).toList ++ arguments.flatten.map(_.variable.name.name)).mkString("_"))
          copies(key) = id
          finished += Toplevel.Def(id, rewrite(copy(callee, method, arguments)))
          id
        })
        val kept = (literal.cparams zip literal.bparams zip bargs zip arguments).collect { case (param, None) => param }
        val tpe = BlockType.Function(literal.tparams,
          kept.map { case ((capture, _), _) => capture } ++ blocks.distinctBy(_.id).map { block => Id(block.id) },
          literal.vparams.map(_.tpe) ++ values.distinctBy(_.id).map(_.annotatedType),
          kept.map { case ((_, param), _) => param.tpe } ++ blocks.distinctBy(_.id).map(_.annotatedTpe),
          literal.body.tpe)
        Stmt.App(Block.BlockVar(id, tpe, closures.flatMap(_.block.capt).toSet), targs,
          vargs.map(rewrite) ++ values.distinctBy(_.id),
          kept.map { case (_, argument) => rewrite(argument) } ++ blocks.distinctBy(_.id))
      }
    }

    override def rewrite(s: Stmt): Stmt = s match {
      case Stmt.Def(id, block, rest) =>
        defined(id) = block
        Stmt.Def(id, rewrite(block), rewrite(rest))
      case Stmt.App(Block.BlockVar(callee, _, _), targs, vargs, bargs) if bargs.nonEmpty =>
        specialized(callee, None, targs, vargs, bargs).getOrElse(super.rewrite(s))
      case Stmt.Invoke(Block.BlockVar(receiver, _, _), method, _, targs, vargs, bargs) if bargs.nonEmpty =>
        specialized(receiver, Some(method), targs, vargs, bargs).getOrElse(super.rewrite(s))
      case other => super.rewrite(other)
    }
  }

  val definitions = m.definitions.map {
    case Toplevel.Def(id, block) => Toplevel.Def(id, specializing.rewrite(block))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, specializing.rewrite(binding))
  }
  m.copy(definitions = definitions ++ finished)
}
