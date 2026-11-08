package effekt
package core

import effekt.context.Context
import effekt.PhaseResult.CoreTransformed
import effekt.core.substitutions.{ Substitution, substitute }
import effekt.util.DB

import scala.collection.mutable

/**
 * Specializes definitions to the blocks passed to them, like Rust monomorphizes closures:
 *
 *   def apply(x: Int){f: Int => Int} = f(x)
 *   def main() = { var s = 0; def g(y: Int) = { s = s + y; return y }; apply(1){g} }
 *
 * becomes
 *
 *   def apply_g(x: Int){s: Ref[Int]} = { def f(y: Int) = { s = s + y; return y }; f(x) }
 *   def main() = { var s = 0; apply_g(1){s} }
 *
 * [[names]] gives every block argument a name, [[instances]] finds, starting from every definition, the shapes of the
 * blocks passed to known definitions, and [[specialize]] makes a copy for each of them.
 * Toplevel and local definitions are treated alike; the definitions themselves stay for every other use.
 */
def specialization(m: ModuleDecl): ModuleDecl = {
  val named = names(m)
  val known = sites(named)
  specialize(named, known, instances(named, known))
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


// Sites
// -----

/** A definition of a block: what it is, and the values and blocks it captures, in a fixed order. */
case class Site(block: Block, values: List[ValueParam], blocks: List[BlockParam])

/**
 * Every definition whose block is a literal or an object. Toplevel definitions capture nothing, and no definition
 * captures itself.
 * A definition capturing a continuation or a prompt is none: a shift to a prompt passed as a parameter cannot be
 * resolved statically anymore.
 */
def sites(m: ModuleDecl): Map[Id, Site] = {
  val globals: Set[Id] = m.definitions.map(_.id).toSet ++ m.externs.collect { case d: Extern.Def => d.id }

  def local(self: Id, block: Block): Option[Site] = {
    val free = block.free
    val values = free.values.toMap.toList.sortBy(_._1.id).map { case (id, tpe) => ValueParam(id, tpe) }
    val blocks = free.blocks.toMap.toList.filterNot { case (id, _) => globals.contains(id) || id == self }.sortBy(_._1.id)
      .map { case (id, (tpe, capt)) => BlockParam(id, tpe, capt) }
    Option.unless(blocks.exists { b => b.tpe.isInstanceOf[BlockType.Resume] || b.tpe.isInstanceOf[BlockType.Prompt] })(Site(block, values, blocks))
  }

  object collecting extends Tree.Query[Unit, Map[Id, Site]] {
    def empty = Map.empty
    def combine = _ ++ _
    override def stmt(using Unit) = {
      case Stmt.Def(id, block @ (_: BlockLit | _: Block.New), body) =>
        local(id, block).map { site => Map(id -> site) }.getOrElse(Map.empty) ++ query(block) ++ query(body)
    }
  }
  m.definitions.collect { case Toplevel.Def(id, block: BlockLit) => id -> Site(block, Nil, Nil) }.toMap ++
    collecting.query(m)(using ())
}


// Shapes
// ------

/** A known block: the definition it comes from, and for each block it captures whether that is known. */
case class Shape(site: Id, captures: List[Option[Shape]]) {
  def contains(other: Id): Boolean = site == other || captures.exists(_.exists(_.contains(other)))
}

/** A call to a known definition with some known block arguments. */
case class Instance(callee: Shape, arguments: List[Option[Shape]])

/**
 * The shape of the block defined as [[site]], where [[known]] gives the shapes of the blocks it captures under their
 * names here. A block capturing a block from the same definition is built up recursively, and captures it as unknown.
 */
def shape(site: Id, known: Id => Option[Shape])(using sites: Map[Id, Site]): Option[Shape] =
  sites.get(site).map { s => Shape(site, s.blocks.map { b => known(b.id).filterNot(_.contains(site)) }) }

/** The instance a call `callee(…){bargs}` asks for, if the callee and some of its block arguments are known. */
def instance(callee: Id, bargs: List[Block], known: Id => Option[Shape])(using sites: Map[Id, Site]): Option[Instance] = {
  val arguments = bargs.map {
    case Block.BlockVar(id, _, _) => known(id)
    case _ => None
  }
  known(callee).filter { c => literal(c).exists(_.bparams.nonEmpty) && arguments.exists(_.isDefined) }
    .map { c => Instance(c, arguments) }
}

def literal(shape: Shape)(using sites: Map[Id, Site]): Option[BlockLit] = sites(shape.site).block match {
  case block: BlockLit => Some(block)
  case _ => None
}

/** The captured values and unknown captured blocks of a known block, depth first. */
def leaves(shape: Shape)(using sites: Map[Id, Site]): List[ValueParam | BlockParam] = {
  val site = sites(shape.site)
  site.values ++ (site.blocks zip shape.captures).flatMap {
    case (b, Some(nested)) => leaves(nested)
    case (b, None) => List(b)
  }
}


// Instances
// ---------

/**
 * All instances, starting from every definition and propagating the shapes into the instances found:
 * in an instance, the known block arguments and the blocks its callee captures are known.
 */
def instances(m: ModuleDecl, known: Map[Id, Site]): List[Instance] = {
  given Map[Id, Site] = known
  val found = mutable.LinkedHashSet.empty[Instance]
  val pending = mutable.Queue.empty[Instance]

  def lookup(env: Map[Id, Shape])(id: Id): Option[Shape] =
    env.get(id).orElse(Option.when(known.get(id).exists(_.values.isEmpty) && isToplevel(id))(Shape(id, Nil)))

  lazy val toplevel: Set[Id] = m.definitions.map(_.id).toSet
  def isToplevel(id: Id): Boolean = toplevel.contains(id)

  object collecting extends Tree.Query[Map[Id, Shape], Unit] {
    def empty = ()
    def combine = (_, _) => ()
    override def stmt(using env: Map[Id, Shape]) = {
      case Stmt.Def(id, block, body) =>
        val bound = block match {
          case Block.BlockVar(x, _, _) => lookup(env)(x)
          case _ => shape(id, lookup(env))
        }
        val inner = bound.map { s => env + (id -> s) }.getOrElse(env - id)
        query(block)(using inner)
        query(body)(using inner)
      case Stmt.App(callee @ Block.BlockVar(f, _, _), targs, vargs, bargs) =>
        instance(f, bargs, lookup(env)).foreach { i => if found.add(i) then pending.enqueue(i) }
        all(vargs, query)
        all(bargs, query)
    }
  }

  m.definitions.foreach { d => collecting.query(d)(using Map.empty) }
  while (pending.nonEmpty) {
    val i = pending.dequeue()
    val block = literal(i.callee).get
    val captured = (known(i.callee.site).blocks zip i.callee.captures).collect { case (b, Some(s)) => b.id -> s }
    val arguments = (block.bparams zip i.arguments).collect { case (p, Some(s)) => p.id -> s }
    val self = Option.unless(isToplevel(i.callee.site))(i.callee.site -> i.callee)
    collecting.query(block.body)(using (captured ++ arguments ++ self).toMap)
  }
  found.toList
}


// Specialize
// ----------

/**
 * One copy per instance, and every call asking for an instance calls its copy instead:
 *
 *   [[ f(…){ g } ]] = f_g(…, values g captures){ unknown blocks g captures }   if the instance is known
 *
 * The copy binds the known blocks it needs at its beginning, each capturing fresh parameters, so that a block
 * captured from different places does not get confused:
 *
 *   def f_g(…, v'){b'} = def g = { … v' … b' … }; [[ body of f ]]
 *
 * A local definition refers to itself in the copy as a definition bound at its beginning, known by the instance's
 * callee, so that a recursive call with the same blocks calls the copy again.
 * A call to a copy keeps the captures of the blocks it no longer passes, since they are still used in the copy.
 */
def specialize(m: ModuleDecl, known: Map[Id, Site], all: List[Instance]): ModuleDecl = {
  given Map[Id, Site] = known
  val toplevel: Set[Id] = m.definitions.map(_.id).toSet
  val copies: Map[Instance, Id] = all.map { i =>
    i -> Id(i.callee.site.name.name + "_specialized")
  }.toMap

  /** Where a renamed name comes from, so that the copies agree on what a definition is. */
  val origin = mutable.Map.empty[Id, Id]
  def originOf(id: Id): Id = origin.getOrElse(id, id)

  /** A known block in scope: its shape, and the block, to find what it captures under the names here. */
  case class Known(shape: Shape, block: Block)

  def lookup(env: Map[Id, Known])(id: Id): Option[Shape] =
    env.get(id).map(_.shape).orElse(Option.when(toplevel.contains(id) && known.contains(id))(Shape(id, Nil)))

  /** What a known block captures, under the names it has here, in the order of [[leaves]], not counting itself. */
  def arguments(k: Known, env: Map[Id, Known]): List[Expr | Block] = {
    val site = known(k.shape.site)
    val free = k.block.free
    val values = free.values.toMap.map { case (id, tpe) => originOf(id) -> (Expr.ValueVar(id, tpe): Expr) }
    val blocks = free.blocks.toMap.filterNot { case (id, _) => env.get(id).exists(_.block eq k.block) }
      .map { case (id, (tpe, capt)) => originOf(id) -> (id, Block.BlockVar(id, tpe, capt)) }
    site.values.map { v => values(v.id) } ++ (site.blocks zip k.shape.captures).flatMap {
      case (b, Some(nested)) => arguments(Known(nested, env(blocks(b.id)._1).block), env)
      case (b, None) => List(blocks(b.id)._2)
    }
  }

  def signature(i: Instance): BlockType.Function = {
    val block = literal(i.callee).get
    val extra = (i.callee :: i.arguments.flatten).flatMap(leaves)
    val kept = (block.bparams zip i.arguments).collect { case (p, None) => p }
    val keptCaptures = (block.cparams zip i.arguments).collect { case (c, None) => c }
    val extraBlocks = extra.collect { case b: BlockParam => b }
    BlockType.Function(block.tparams, keptCaptures ++ extraBlocks.map(_.id),
      block.vparams.map(_.tpe) ++ extra.collect { case v: ValueParam => v.tpe },
      kept.map(_.tpe) ++ extraBlocks.map(_.tpe),
      block.body.tpe)
  }

  object specializing extends Tree.RewriteWithContext[Map[Id, Known]] {
    override def rewrite(s: Stmt)(using env: Map[Id, Known]): Stmt = s match {
      case Stmt.Def(id, block, body) =>
        val bound = env.get(id).orElse {
          block match {
            case Block.BlockVar(x, _, _) => env.get(x).orElse(lookup(env)(x).map { s => Known(s, block) })
            case _ => shape(originOf(id), name => lookup(env)(current(block, name))).map { s => Known(s, block) }
          }
        }
        val inner = bound.map { k => env + (id -> k) }.getOrElse(env)
        Stmt.Def(id, rewrite(block)(using inner), rewrite(body)(using inner))

      case Stmt.App(callee @ Block.BlockVar(f, _, capt), targs, vargs, bargs) =>
        instance(f, bargs, lookup(env)).filter(copies.contains) match {
          case Some(i) =>
            val calleeKnown = env.getOrElse(f, Known(i.callee, callee))
            val passed = (bargs zip i.arguments).collect { case (b @ Block.BlockVar(x, _, _), Some(_)) => env.getOrElse(x, Known(lookup(env)(x).get, b)) }
            val extra = (calleeKnown :: passed).flatMap(arguments(_, env))
            val unknown = (bargs zip i.arguments).collect { case (b, None) => rewrite(b) }
            val captures = capt ++ (bargs zip i.arguments).collect { case (b, Some(_)) => b.capt }.flatten
            Stmt.App(Block.BlockVar(copies(i), signature(i), captures), targs,
              vargs.map(rewrite) ++ extra.collect { case e: Expr => e },
              unknown ++ extra.collect { case b: Block => b })
          case None => super.rewrite(s)
        }

      case other => super.rewrite(other)
    }
  }

  /** The name a block captured by the definition [[block]] has here, given its name where it was defined. */
  def current(block: Block, name: Id): Id =
    block.free.blocks.toMap.keys.find { id => originOf(id) == name }.getOrElse(name)

  /** [[block]] with fresh names for everything it binds, remembering the names they replace. */
  def fresh[B <: Block](block: B): B = {
    val renamer = Renamer()
    val renamed = renamer.rewrite(block).asInstanceOf[B]
    renamer.renamed.foreach { case (fresh, old) => origin(fresh) = originOf(old) }
    renamed
  }

  /**
   * The parameters for what a block of [[shape]] captures, the bindings of the known blocks it captures,
   * named fresh and known by their shapes, and the substitution for its free variables.
   */
  def captured(shape: Shape): (List[ValueParam | BlockParam], List[Binding], Map[Id, Known], Substitution) = {
    val site = known(shape.site)
    val values = site.values.map { v =>
      val param = ValueParam(Id(v.id), v.tpe)
      origin(param.id) = v.id
      param
    }
    val blocks = (site.blocks zip shape.captures).map {
      case (b, Some(s)) =>
        val name = Id(b.id)
        origin(name) = b.id
        val (params, bindings, seeds) = instantiate(name, s)
        (params, Block.BlockVar(name, b.tpe, seeds(name).block.capt), bindings, seeds)
      case (b, None) =>
        val name = Id(b.id)
        origin(name) = b.id
        (List(BlockParam(name, b.tpe, Set(name))), Block.BlockVar(name, b.tpe, Set(name)), Nil, Map.empty)
    }
    val substitution = Substitution(DB.empty,
      DB.from((site.blocks zip blocks).flatMap { case (b, (_, block, _, _)) => b.capt.map { c => c -> block.capt } }),
      DB.from((site.values zip values).map { case (v, param) => v.id -> Expr.ValueVar(param.id, param.tpe) }),
      DB.from((site.blocks zip blocks).map { case (b, (_, block, _, _)) => b.id -> block }))
    (values ++ blocks.flatMap(_._1), blocks.flatMap(_._3), blocks.map(_._4).foldLeft(Map.empty)(_ ++ _), substitution)
  }

  /** Binds a block of [[shape]] as [[name]], see [[captured]]. */
  def instantiate(name: Id, shape: Shape): (List[ValueParam | BlockParam], List[Binding], Map[Id, Known]) =
    if (toplevel.contains(shape.site)) {
      val site = known(shape.site)
      val alias = Block.BlockVar(shape.site, site.block.tpe, site.block.capt)
      (Nil, List(Binding.Def(name, alias)), Map(name -> Known(shape, alias)))
    } else {
      val (params, bindings, seeds, substitution) = captured(shape)
      val block = fresh(substitute(known(shape.site).block)(using itself(substitution, shape.site, name)))
      (params, bindings :+ Binding.Def(name, block), seeds + (name -> Known(shape, block)))
    }

  /** [[substitution]], and the definition [[site]] refers to itself as [[name]]. */
  def itself(substitution: Substitution, site: Id, name: Id): Substitution = {
    val block = known(site).block
    substitution.copy(blocks = substitution.blocks + (site -> Block.BlockVar(name, block.tpe, block.capt)))
  }

  def copy(i: Instance): Toplevel.Def = {
    val callee = fresh(literal(i.callee).get)
    val (calleeParams, capturedBindings, capturedSeeds, capturedSubstitution) = captured(i.callee)
    val (calleeBindings, calleeSeeds, calleeSubstitution) =
      if (toplevel.contains(i.callee.site)) (capturedBindings, capturedSeeds, capturedSubstitution) else {
        val name = Id(i.callee.site)
        val substitution = itself(capturedSubstitution, i.callee.site, name)
        val block = fresh(substitute(known(i.callee.site).block)(using substitution))
        (capturedBindings :+ Binding.Def(name, block), capturedSeeds + (name -> Known(i.callee, block)), substitution)
      }
    val (argumentParams, argumentBindings, argumentSeeds) = (callee.bparams zip i.arguments).collect {
      case (p, Some(s)) => instantiate(p.id, s)
    }.unzip3
    val extra = calleeParams ++ argumentParams.flatten
    val extraBlocks = extra.collect { case b: BlockParam => b }
    val kept = (callee.bparams zip i.arguments).collect { case (p, None) => p }
    val keptCaptures = (callee.cparams zip i.arguments).collect { case (c, None) => c }
    val body = Binding(calleeBindings ++ argumentBindings.flatten, substitute(callee.body)(using calleeSubstitution))
    val copied = BlockLit(callee.tparams, keptCaptures ++ extraBlocks.map(_.id),
      callee.vparams ++ extra.collect { case v: ValueParam => v }, kept ++ extraBlocks, body)
    Toplevel.Def(copies(i), specializing.rewrite(copied)(using argumentSeeds.foldLeft(calleeSeeds)(_ ++ _)))
  }

  val rewritten = m.definitions.map {
    case Toplevel.Def(id, block) => Toplevel.Def(id, specializing.rewrite(block)(using Map.empty))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, specializing.rewrite(binding)(using Map.empty))
  }
  m.copy(definitions = rewritten ++ all.map(copy))
}
