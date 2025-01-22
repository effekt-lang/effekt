package effekt
package core
package optimizer

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
    case s  @ IsStatic(types, values, blocks) =>
      types.forall(x => x) && (values.exists(x => x) || blocks.exists(x => x))
  }

  private def dropStatic[A](isStatic: List[Boolean], arguments: List[A]): List[A] =
    (isStatic zip arguments).collect { case (false, arg) => arg }

  private def selectStatic[A](isStatic: List[Boolean], arguments: List[A]): List[A] =
    (isStatic zip arguments).collect { case (true, arg) => arg }

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
  def wrapDefinition(id: Id, blockLit: BlockLit)(using ctx: StaticArgumentsContext): BlockLit = {
    val IsStatic(staticT, staticV, staticB) = ctx.statics(id)

    assert(staticT.forall(x => x), "Can only apply the static arguments translation, if all type arguments are static.")

    val workerType = BlockType.Function(
      dropStatic(staticT, blockLit.tparams), // should always be empty!
      // here we drop those capture params where the block param is static
      dropStatic(staticB, blockLit.cparams),
      dropStatic(staticV, blockLit.vparams).map(_.tpe),
      dropStatic(staticB, blockLit.bparams).map(_.tpe),
      blockLit.returnType
    )

    // fresh params for the wrapper function and its invocation
    // note: only freshen non-static params to prevent duplicates
    val freshCparams: List[Id] = (staticB zip blockLit.cparams).map {
      case (true, param) => param
      case (false, param) => Id(param)
    }
    val freshVparams: List[ValueParam] = (staticV zip blockLit.vparams).map {
      case (true, param) => param
      case (false, ValueParam(id, tpe)) => ValueParam(Id(id), tpe)
    }
    val freshBparams: List[BlockParam] = (staticB zip blockLit.bparams zip freshCparams).map {
      case ((true, param), capt) => param
      case ((false, BlockParam(id, tpe, _)), capt) => BlockParam(Id(id), tpe, Set(capt))
    }

    // the worker now closes over the static block arguments (`c` in the example above):
    val newCapture = blockLit.capt ++ selectStatic(staticB, freshCparams).toSet

    val workerVar: Block.BlockVar = BlockVar(Id(id.name.name + "_worker"), workerType, newCapture)
    ctx.workers(id) = workerVar

    BlockLit(
      blockLit.tparams,
      freshCparams,
      freshVparams,
      freshBparams,
      Stmt.Def(workerVar.id, BlockLit(
        dropStatic(staticT, blockLit.tparams),
        dropStatic(staticB, blockLit.cparams),
        dropStatic(staticV, blockLit.vparams),
        dropStatic(staticB, blockLit.bparams),
        rewrite(blockLit.body)(using enterFunction(id))
      ), App(
        workerVar,
        dropStatic(staticT, blockLit.tparams.map(t => ValueType.Var(t))),
        dropStatic(staticV, freshVparams.map(v => ValueVar(v.id, v.tpe))),
        dropStatic(staticB, freshBparams.map(b => BlockVar(b.id, b.tpe, b.capt)))
      ))
    )
  }

  def rewrite(d: Toplevel)(using StaticArgumentsContext): Toplevel = d match {
    case Toplevel.Def(id, block: BlockLit) if hasStatics(id) => Toplevel.Def(id, wrapDefinition(id, block))
    case Toplevel.Def(id, block) => Toplevel.Def(id, rewrite(block))
    case Toplevel.Val(id, tpe, binding) => Toplevel.Val(id, tpe, rewrite(binding))
  }

  def rewrite(s: Stmt)(using C: StaticArgumentsContext): Stmt = s match {

    case Stmt.Def(id, block: BlockLit, body) if hasStatics(id) => Stmt.Def(id, wrapDefinition(id, block), rewrite(body))
    case Stmt.Def(id, block, body) => Stmt.Def(id, rewrite(block), rewrite(body))

    case Stmt.Let(id, tpe, binding, body) => Stmt.Let(id, tpe, rewrite(binding), rewrite(body))

    case Stmt.App(b, targs, vargs, bargs) =>
      b match {
        // if arguments are static && recursive call: call worker with reduced arguments
        case BlockVar(id, annotatedTpe, annotatedCapt) if hasStatics(id) && within(id) =>
          val IsStatic(staticT, staticV, staticB) = C.statics(id)
          Stmt.App(C.workers(id),
            dropStatic(staticT, targs),
            dropStatic(staticV, vargs).map(rewrite),
            dropStatic(staticB, bargs).map(rewrite))
        case _ => Stmt.App(rewrite(b), targs, vargs.map(rewrite), bargs.map(rewrite))
      }

    case Stmt.Invoke(b, method, methodTpe, targs, vargs, bargs) =>
      Stmt.Invoke(rewrite(b), method, methodTpe, targs, vargs.map(rewrite), bargs.map(rewrite))

    case Stmt.Reset(body) =>
      rewrite(body) match {
        case b => Stmt.Reset(b)
      }

    // congruences
    case Stmt.Return(expr) => Return(rewrite(expr))
    case Stmt.Val(id, tpe, binding, body) => Stmt.Val(id, tpe, rewrite(binding), rewrite(body))
    case Stmt.If(cond, thn, els) => If(rewrite(cond), rewrite(thn), rewrite(els))
    case Stmt.Match(scrutinee, clauses, default) => Stmt.Match(rewrite(scrutinee), clauses.map { case (id, value) => id -> rewrite(value) }, default.map(rewrite))
    case Stmt.Alloc(id, init, region, body) => Alloc(id, rewrite(init), region, rewrite(body))
    case Stmt.Shift(prompt, body) => Shift(prompt, rewrite(body))
    case Stmt.Resume(k, body) => Resume(k, rewrite(body))
    case Stmt.Region(body) => Region(rewrite(body))
    case Stmt.Var(id, init, capture, body) => Stmt.Var(id, rewrite(init), capture, rewrite(body))
    case Stmt.Get(id, capt, tpe) => Stmt.Get(id, capt, tpe)
    case Stmt.Put(id, capt, value) => Stmt.Put(id, capt, rewrite(value))
    case Stmt.Hole() => Stmt.Hole()
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
    case Block.Unbox(pure) => Block.Unbox(rewrite(pure))
    case Block.New(impl) => Block.New(rewrite(impl))
  }

  def rewrite(s: Implementation)(using StaticArgumentsContext): Implementation =
    s match {
      case Implementation(interface, operations) => Implementation(interface, operations.map { op =>
        op.copy(body = rewrite(op.body))
      })
    }

  def rewrite(p: Pure)(using StaticArgumentsContext): Pure = p match {
    case Pure.PureApp(f, targs, vargs) => Pure.PureApp(f, targs, vargs.map(rewrite))
    case Pure.Make(data, tag, vargs) => Pure.Make(data, tag, vargs.map(rewrite))
    case x @ Pure.ValueVar(id, annotatedType) => x

    // congruences
    case Pure.Literal(value, annotatedType) => p
    case Pure.Box(b, annotatedCapture) => Pure.Box(rewrite(b), annotatedCapture)
  }

  def rewrite(e: Expr)(using StaticArgumentsContext): Expr = e match {
    case DirectApp(b, targs, vargs, bargs) => DirectApp(b, targs, vargs.map(rewrite), bargs.map(rewrite))

    // congruences
    case pure: Pure => rewrite(pure)
  }

  def transform(entrypoint: Id, m: ModuleDecl): ModuleDecl =
    val recursiveFunctions = Recursive(m)

    val statics: Map[Id, IsStatic] = recursiveFunctions.map {
      case (id, RecursiveFunction(BlockLit(tparams, cparams, vparams, bparams, body), targs, vargs, bargs)) =>
        val isTypeStatic = tparams.zipWithIndex.collect {
          case (param, index) => targs.nonEmpty && targs.map(args => args(index)).forall {
            case ValueType.Var(other) => param == other
            case _ => false
          }
        }
        val isValueStatic = vparams.zipWithIndex.collect {
          case (param, index) => vargs.nonEmpty && vargs.map(args => args(index)).forall {
            case ValueVar(other, _) => param.id == other
            case _ => false
          }
        }
        val isBlockStatic = bparams.zipWithIndex.collect {
          case (param, index) => bargs.nonEmpty && bargs.map(args => args(index)).forall {
            case BlockVar(other, _, _) => param.id == other
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

    val updatedDefs = m.definitions.map(rewrite)
    m.copy(definitions = updatedDefs)
}

enum ArgumentType {
  case Static(name: String)
  case NonStatic
}
