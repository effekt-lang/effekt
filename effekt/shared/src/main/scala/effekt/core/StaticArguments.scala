package effekt
package core

import effekt.core.normal.*
import scala.collection.mutable

/**
 * Static argument transformation
 *
 * 1. First gathers recursive calls (using [[Recursive.apply]])
 * 2. Transforms them using a Worker/Wrapper strategy
 */
object StaticArguments {

  case class StaticArgumentsContext(
    staticC: Map[Id, List[Boolean]],
    staticV: Map[Id, List[Boolean]],
    staticB: Map[Id, List[Boolean]],
    workers: mutable.Map[Id, Block],
    var stack: List[Id]
  )

  def dropStatic[A](staticArgumentsOpt: Option[List[Boolean]], list: List[A]) = staticArgumentsOpt match
    case Some(staticArguments) => list.zip(staticArguments).filter((_, bool) => !bool).map(_._1)
    case None => list
  
  /**
   * Wraps the definition in another function, abstracting arguments along the way.
   * For example:
   * 
   *   def foo(a, b, c) =
   *     if (a == b) println(c)
   *     else foo(a - 2, b - 1, c)
   *   foo(4, 2, 0)
   *
   * becomes:
   *
   *   def foo(a_fresh, b_fresh, c) =
   *     def foo_worker(a, b) =
   *       if (a == b) println(c)
   *       else foo_worker(a - 2, b - 1)
   *     foo_worker(a_fresh, b_fresh)
   *   foo(4, 2, 0)
   */
  def wrapDefinition(definition: Definition.Def)(using ctx: StaticArgumentsContext): Definition.Def =
    // This is guaranteed by Recursive()!
    val blockLit = definition.block.asInstanceOf[BlockLit]

    val staticC = ctx.staticC.get(definition.id)
    val staticV = ctx.staticV.get(definition.id)
    val staticB = ctx.staticB.get(definition.id)

    val calleeType = BlockType.Function(
      blockLit.tparams,
      dropStatic(staticC, blockLit.cparams),
      dropStatic(staticV, blockLit.vparams).map(_.tpe),
      dropStatic(staticB, blockLit.bparams).map(_.tpe),
      blockLit.tpe match
        case BlockType.Function(_, _, _, _, result) => result
        case _ => ??? // impossible!
    )

    // TODO: Where do we get the capture set?
    val worker: Block.BlockVar = BlockVar(Id(definition.id.name.name + "_worker"), calleeType, Set())
    ctx.workers(definition.id) = worker

    // push to the stack to detect recursion, since recursive calls will need to call the worker
    def rewriteBody(body: Stmt) =
      val before = ctx.stack
      ctx.stack = definition.id :: ctx.stack
      val newBody = rewrite(body)
      ctx.stack = before
      newBody
    
    // fresh params for the wrapper function and its invocation
    // note: only freshen params if not static to prevent duplicates
    val freshCparams = staticC.map { static =>
      blockLit.cparams.zip(static).map((c, bool) => if (bool) c else Id(c.name.name))
    }.getOrElse(blockLit.cparams)
    val freshVparams = staticV.map { static =>
      blockLit.vparams.zip(static).map((v, bool) => ValueParam(if (bool) v.id else Id(v.id.name.name), v.tpe)).asInstanceOf[List[Param.ValueParam]]
    }.getOrElse(blockLit.vparams)
    val freshBparams = staticB.map { static =>
      blockLit.bparams.zip(static).map((b, bool) => BlockParam(if (bool) b.id else Id(b.id.name.name), b.tpe, b.capt)).asInstanceOf[List[Param.BlockParam]]
    }.getOrElse(blockLit.bparams)

    Definition.Def(definition.id, BlockLit(
      blockLit.tparams, // TODO: Do we need to freshen these as well?
      freshCparams,
      freshVparams,
      freshBparams,
      Scope(List(Definition.Def(worker.id, BlockLit(
        blockLit.tparams,
        dropStatic(staticC, blockLit.cparams),
        dropStatic(staticV, blockLit.vparams),
        dropStatic(staticB, blockLit.bparams),
        rewriteBody(blockLit.body)
      ))), App(
        worker,
        // TODO: These conversions to types are a bit hacky, are there better ways?
        blockLit.tparams.map(t => ValueType.Var(t)),
        dropStatic(staticV, freshVparams.map(v => ValueVar(v.id, v.tpe))),
        dropStatic(staticB, freshBparams.map(b => BlockVar(b.id, b.tpe, b.capt)))
      ))
    ))
  
  def rewrite(definitions: List[Definition])(using ctx: StaticArgumentsContext): List[Definition] =
    definitions.collect {
      case d @ Definition.Def(id, block) if ctx.staticC.contains(id) || ctx.staticV.contains(id) || ctx.staticB.contains(id)
        => wrapDefinition(d)
      case Definition.Def(id, block) => Definition.Def(id, rewrite(block))
      case Definition.Let(id, tpe, binding) => Definition.Let(id, tpe, rewrite(binding))
    }

  def rewrite(d: Definition)(using StaticArgumentsContext): Definition = d match {
    case Definition.Def(id, block) => Definition.Def(id, rewrite(block))
    case Definition.Let(id, tpe, binding) => Definition.Let(id, tpe, rewrite(binding))
  }

  def rewrite(s: Stmt)(using C: StaticArgumentsContext): Stmt = s match {
    case Stmt.Scope(definitions, body) =>
      scope(rewrite(definitions), rewrite(body))

    case Stmt.App(b, targs, vargs, bargs) =>
      b match {
        // if arguments are static && recursive call: call worker with reduced arguments
        case BlockVar(id, annotatedTpe, annotatedCapt) if (C.staticC.contains(id) || C.staticV.contains(id) || C.staticB.contains(id)) && C.stack.contains(id) => 
          app(C.workers(id), targs, dropStatic(C.staticV.get(id), vargs).map(rewrite), dropStatic(C.staticB.get(id), bargs).map(rewrite))
        case _ => app(rewrite(b), targs, vargs.map(rewrite), bargs.map(rewrite))
      }

    case Stmt.Invoke(b, method, methodTpe, targs, vargs, bargs) =>
      invoke(rewrite(b), method, methodTpe, targs, vargs.map(rewrite), bargs.map(rewrite))

    case Stmt.Reset(body) =>
      rewrite(body) match {
        case b => Stmt.Reset(b)
      }

    // congruences
    case Stmt.Return(expr) => Return(rewrite(expr))
    case Stmt.Val(id, tpe, binding, body) => valDef(id, tpe, rewrite(binding), rewrite(body))
    case Stmt.If(cond, thn, els) => If(rewrite(cond), rewrite(thn), rewrite(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      patternMatch(rewrite(scrutinee), clauses.map { case (id, value) => id -> rewrite(value) }, default.map(rewrite))
    case Stmt.Alloc(id, init, region, body) => Alloc(id, rewrite(init), region, rewrite(body))
    case Stmt.Shift(prompt, body) => Shift(prompt, rewrite(body))
    case Stmt.Resume(k, body) => Resume(k, rewrite(body))
    case Stmt.Region(body) => Region(rewrite(body))
    case Stmt.Var(id, init, capture, body) => Stmt.Var(id, rewrite(init), capture, rewrite(body))
    case Stmt.Get(id, capt, tpe) => Stmt.Get(id, capt, tpe)
    case Stmt.Put(id, capt, value) => Stmt.Put(id, capt, rewrite(value))
    case Stmt.Hole() => s
  }
  def rewrite(b: BlockLit)(using StaticArgumentsContext): BlockLit =
    b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams, cparams, vparams, bparams, rewrite(body))
    }

  def rewrite(b: Block)(using C: StaticArgumentsContext): Block = b match {
    case b @ Block.BlockVar(id, _, _) => b

    // congruences
    case b @ Block.BlockLit(tparams, cparams, vparams, bparams, body) => rewrite(b)
    case Block.Unbox(pure) => unbox(rewrite(pure))
    case Block.New(impl) => New(rewrite(impl))
  }

  def rewrite(s: Implementation)(using StaticArgumentsContext): Implementation =
    s match {
      case Implementation(interface, operations) => Implementation(interface, operations.map { op =>
        op.copy(body = rewrite(op.body))
      })
    }

  def rewrite(p: Pure)(using StaticArgumentsContext): Pure = p match {
    case Pure.PureApp(b, targs, vargs) => pureApp(rewrite(b), targs, vargs.map(rewrite))
    case Pure.Make(data, tag, vargs) => make(data, tag, vargs.map(rewrite))
    case x @ Pure.ValueVar(id, annotatedType) => x

    // congruences
    case Pure.Literal(value, annotatedType) => p
    case Pure.Select(target, field, annotatedType) => select(rewrite(target), field, annotatedType)
    case Pure.Box(b, annotatedCapture) => box(rewrite(b), annotatedCapture)
  }

  def rewrite(e: Expr)(using StaticArgumentsContext): Expr = e match {
    case DirectApp(b, targs, vargs, bargs) => directApp(rewrite(b), targs, vargs.map(rewrite), bargs.map(rewrite))

    // congruences
    case Run(s) => run(rewrite(s))
    case pure: Pure => rewrite(pure)
  }

  def analyzeBargs(bargs: List[Block]) =
    bargs.map {
      case BlockVar(id, annotatedType, annotatedCapt) => ArgumentType.Static(id.name.name)
      case _ => ArgumentType.NonStatic
    }

  def analyzeVargs(vargs: List[Pure]) =
    vargs.map {
      case ValueVar(id, annotatedType) => ArgumentType.Static(id.name.name)
      case _ => ArgumentType.NonStatic
    }
  
  def transform(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    val recursiveFunctions = Recursive(m)

    // returns a map of function names to a list of booleans, indicating whether
    //   the respective argument can be statically transformed
    def fullyStatic[A](params: List[Id], set: mutable.Set[List[A]], f: List[A] => List[ArgumentType]) =
      val static = params.map(arg => ArgumentType.Static(arg.name.name))
      set.map(f).map(_.zip(static).map((a, b) => a == b))
        .toList.transpose.map(_.forall(identity)) // the column-wise AND of all lists

    val staticC = recursiveFunctions.view.mapValues { gathering => fullyStatic(gathering.cparams, gathering.cargs, analyzeBargs) }
      .toMap.filter(_._2.contains(true))

    val staticV = recursiveFunctions.view.mapValues { gathering => fullyStatic(gathering.vparams, gathering.vargs, analyzeVargs) }
      .toMap.filter(_._2.contains(true))

    val staticB = recursiveFunctions.view.mapValues { gathering => fullyStatic(gathering.bparams, gathering.bargs, analyzeBargs) }
      .toMap.filter(_._2.contains(true))

    given ctx: StaticArgumentsContext = StaticArgumentsContext(
      staticC,
      staticV,
      staticB,
      mutable.Map(),
      List()
    )

    val updatedDefs = rewrite(m.definitions)
    m.copy(definitions = updatedDefs)
}

enum ArgumentType {
  case Static(name: String)
  case NonStatic
}