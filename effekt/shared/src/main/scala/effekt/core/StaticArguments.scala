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
    staticArguments: Map[Id, List[Boolean]],
    workers: mutable.Map[Id, Id],
    var stack: List[Id]
  )

  def dropStatic[A](staticArguments: List[Boolean], list: List[A]) =
    list.zip(staticArguments).filter((_, bool) => !bool).map(_._1)
  
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

    val staticArguments = ctx.staticArguments(definition.id)

    val worker = Id(definition.id.name.name + "_worker")
    ctx.workers(definition.id) = worker

    val calleeType = BlockType.Function(
      blockLit.tparams,
      blockLit.cparams,
      dropStatic(staticArguments, blockLit.vparams.map(_.tpe)),
      blockLit.bparams.map(_.tpe),
      blockLit.tpe match
        case BlockType.Function(_, _, _, _, result) => result
        case _ => ??? // impossible!
    )

    // push to the stack to detect recursion, since recursive calls will need to call the worker
    def rewriteBody(body: Stmt) =
      val before = ctx.stack
      ctx.stack = definition.id :: ctx.stack
      val newBody = rewrite(body)
      ctx.stack = before
      newBody
    
    // fresh params for the wrapper function and its invocation
    val freshCparams = blockLit.cparams.map(c => Id(c.name.name))
    val freshBparams: List[Param.BlockParam] = blockLit.bparams.map(b => BlockParam(Id(b.id.name.name), b.tpe, b.capt))
    val freshVparams: List[Param.ValueParam] = // here: only freshen params if not static to prevent duplicates
      blockLit.vparams.zip(staticArguments).map((v, bool) => ValueParam(if (bool) v.id else Id(v.id.name.name), v.tpe))

    Definition.Def(definition.id, BlockLit(
      blockLit.tparams, // TODO: Do we need to freshen these as well?
      blockLit.cparams, // TODO: Do we need to freshen these as well?
      freshVparams,
      freshBparams,
      Scope(List(Definition.Def(worker, BlockLit(
        blockLit.tparams,
        blockLit.cparams,
        dropStatic(staticArguments, blockLit.vparams),
        blockLit.bparams,
        rewriteBody(blockLit.body)
      ))), App(
        // TODO: Where do we get the capture set?
        BlockVar(worker, calleeType, Set()),
        // TODO: These conversions to types are a bit hacky, are there better ways?
        blockLit.tparams.map(t => ValueType.Var(t)),
        dropStatic(staticArguments, freshVparams.map(v => ValueVar(v.id, v.tpe))),
        freshBparams.map(b => BlockVar(b.id, b.tpe, b.capt))
      ))
    ))
  
  def rewrite(definitions: List[Definition])(using ctx: StaticArgumentsContext): List[Definition] =
    val lifted = definitions.collect {
      case d @ Definition.Def(id, block) if ctx.staticArguments.contains(id) => wrapDefinition(d)
      case Definition.Def(id, block) => Definition.Def(id, rewrite(block))
      case Definition.Let(id, tpe, binding) => Definition.Let(id, tpe, rewrite(binding))
    }
    lifted

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
        case BlockVar(id, annotatedTpe, annotatedCapt) if C.staticArguments.contains(id) && C.stack.contains(id) => 
          app(BlockVar(C.workers(id), annotatedTpe, annotatedCapt),
            targs, dropStatic(C.staticArguments(id), vargs.map(rewrite)), bargs.map(rewrite))
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

  def analyzeParameters(vargs: List[Pure]) =
    vargs.map(p => p match {
      case ValueVar(id, annotatedType) => ArgumentType.Static(id.name.name)
      case _ => ArgumentType.NonStatic
    })
  
  def transform(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    val recursiveFunctions = Recursive(m)

    // a map of function names to a list of booleans, indicating whether
    //   the respective argument can be statically transformed
    val staticArguments = recursiveFunctions.view.mapValues { (callee, set) =>
      val calleeParams = callee.map(arg => ArgumentType.Static(arg.name.name))
      set.map(analyzeParameters).map(_.zip(calleeParams).map((a, b) => a == b))
        .toList.transpose.map(_.forall(identity)) // the column-wise AND of all lists
    }.toMap

    val staticArgumentsFiltered = staticArguments.filter(_._2.contains(true))

    given ctx: StaticArgumentsContext = StaticArgumentsContext(
      staticArgumentsFiltered,
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