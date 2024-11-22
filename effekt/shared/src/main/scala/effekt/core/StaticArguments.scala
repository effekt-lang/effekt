package effekt
package core

import effekt.core.normal.*
import scala.collection.mutable
import effekt.core.Type.returnType

/**
 * Static argument transformation
 *
 * 1. First gathers recursive calls (using [[Recursive.apply]])
 * 2. Transforms them using a Worker/Wrapper strategy
 */
object StaticArguments {

  case class IsStatic(
    types: List[Boolean],
    values: List[Boolean],
    blocks: List[Boolean])

  case class StaticArgumentsContext(
    statics: Map[Id, IsStatic],
    workers: mutable.Map[Id, Block],
    stack: List[Id]
  )

  def enterFunction(id: Id)(using ctx: StaticArgumentsContext): StaticArgumentsContext = ctx.copy(stack = id :: ctx.stack)

  def within(id: Id)(using ctx: StaticArgumentsContext): Boolean = ctx.stack.contains(id)

  def hasStatics(id: Id)(using ctx: StaticArgumentsContext): Boolean = ctx.statics.get(id).exists {
    // types ALL need to be static
    case IsStatic(types, values, blocks) => types.forall(x => x) && (values.exists(x => x) || blocks.exists(x => x))
  }

  def dropStatic[A](isStatic: List[Boolean], arguments: List[A]): List[A] =
    (isStatic zip arguments).collect { case (false, arg) => arg }

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
  def wrapDefinition(id: Id, blockLit: BlockLit)(using ctx: StaticArgumentsContext): Definition.Def =
    val IsStatic(staticT, staticV, staticB) = ctx.statics(id)

    assert(staticT.forall(x => x), "Can only apply the worker-wrapper translation, if all type arguments are static.")

    val workerType = BlockType.Function(
      dropStatic(staticT, blockLit.tparams), // should always be empty!
      // here we drop those capture params where the block param is static
      dropStatic(staticB, blockLit.cparams),
      dropStatic(staticV, blockLit.vparams).map(_.tpe),
      dropStatic(staticB, blockLit.bparams).map(_.tpe),
      blockLit.returnType
    )

    val workerVar: Block.BlockVar = BlockVar(Id(id.name.name + "_worker"), workerType, blockLit.capt)
    ctx.workers(id) = workerVar

    // fresh params for the wrapper function and its invocation
    // note: only freshen params if not static to prevent duplicates
    val freshCparams: List[Id] = (staticB zip blockLit.cparams).map {
      case (true, param) => param
      case (false, param) => Id(param)
    }
    val freshVparams: List[ValueParam] = (staticV zip blockLit.vparams).map {
      case (true, param) => param
      case (false, ValueParam(id, tpe)) => ValueParam(Id(id), tpe)
    }
    val freshBparams: List[BlockParam] = (staticB zip blockLit.bparams).map {
      case (true, param) => param
      case (false, BlockParam(id, tpe, capt)) => BlockParam(Id(id), tpe, capt)
    }

    Definition.Def(id, BlockLit(
      blockLit.tparams,
      freshCparams,
      freshVparams,
      freshBparams,
      Scope(List(Definition.Def(workerVar.id, BlockLit(
        dropStatic(staticT, blockLit.tparams),
        dropStatic(staticB, blockLit.cparams),
        dropStatic(staticV, blockLit.vparams),
        dropStatic(staticB, blockLit.bparams),
        rewrite(blockLit.body)
      ))), App(
        workerVar,
        dropStatic(staticT, blockLit.tparams.map(t => ValueType.Var(t))),
        dropStatic(staticV, freshVparams.map(v => ValueVar(v.id, v.tpe))),
        dropStatic(staticB, freshBparams.map(b => BlockVar(b.id, b.tpe, b.capt)))
      ))
    ))

  def rewrite(definitions: List[Definition])(using ctx: StaticArgumentsContext): List[Definition] =
    definitions.collect {
      case Definition.Def(id, block: BlockLit) if hasStatics(id) => wrapDefinition(id, block)
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
        case BlockVar(id, annotatedTpe, annotatedCapt) if hasStatics(id) && within(id) =>
          val IsStatic(staticT, staticV, staticB) = C.statics(id)
          app(C.workers(id),
            dropStatic(staticT, targs),
            dropStatic(staticV, vargs).map(rewrite),
            dropStatic(staticB, bargs).map(rewrite))
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

  def transform(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    val recursiveFunctions = Recursive(m)

    val statics: Map[Id, IsStatic] = recursiveFunctions.map {
      case (id, RecursiveFunction(BlockLit(tparams, cparams, vparams, bparams, body), targs, vargs, bargs)) =>
        val isTypeStatic = tparams.zipWithIndex.collect {
          case (param, index) => targs.map(args => args(index)).forall {
            case ValueType.Var(other) => param == other
            case _ => false
          }
        }
        val isValueStatic = vparams.zipWithIndex.collect {
          case (param, index) => vargs.map(args => args(index)).forall {
            case ValueVar(other, _) => param == other
            case _ => false
          }
        }
        val isBlockStatic = bparams.zipWithIndex.collect {
          case (param, index) => bargs.map(args => args(index)).forall {
            case BlockVar(other, _, _) => param == other
            case _ => false
          }
        }
        id -> IsStatic(isTypeStatic, isValueStatic, isBlockStatic)
    }.toMap

    given ctx: StaticArgumentsContext = StaticArgumentsContext(
      statics,
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
