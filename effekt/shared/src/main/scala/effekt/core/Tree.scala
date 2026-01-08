package effekt
package core

import effekt.source.FeatureFlag
import effekt.util.{ Structural, Trampoline }
import effekt.util.messages.INTERNAL_ERROR
import effekt.util.messages.ErrorReporter

import scala.annotation.{ tailrec }

/**
 * Tree structure of programs in our internal core representation.
 *
 * Core uses [[effekt.symbols.Symbol]] as names. The structure of symbols and the contents
 * in the DB should not be used after translation to core.
 *
 * ----------[[ effekt.core.Tree ]]----------
 *
 *   ─ [[ Tree ]]
 *     │─ [[ ModuleDecl ]]
 *     │─ [[ Declaration ]]
 *     │  │─ [[ Data ]]
 *     │  │─ [[ Interface ]]
 *     │
 *     │─ [[ Constructor ]]
 *     │─ [[ Field ]]
 *     │─ [[ Property ]]
 *     │─ [[ Extern ]]
 *     │  │─ [[ Def ]]
 *     │  │─ [[ Include ]]
 *     │
 *     │─ [[ ExternBody ]]
 *     │  │─ [[ StringExternBody ]]
 *     │  │─ [[ Unsupported ]]
 *     │
 *     │─ [[ Block ]]
 *     │  │─ [[ BlockVar ]]
 *     │  │─ [[ BlockLit ]]
 *     │  │─ [[ Unbox ]]
 *     │  │─ [[ New ]]
 *     │
 *     │─ [[ Stmt ]]
 *     │  │─ [[ Def ]]
 *     │  │─ [[ Let ]]
 *     │  │─ [[ Return ]]
 *     │  │─ [[ Val ]]
 *     │  │─ [[ App ]]
 *     │  │─ [[ Invoke ]]
 *     │  │─ [[ If ]]
 *     │  │─ [[ Match ]]
 *     │  │─ [[ Region ]]
 *     │  │─ [[ Alloc ]]
 *     │  │─ [[ Var ]]
 *     │  │─ [[ Get ]]
 *     │  │─ [[ Put ]]
 *     │  │─ [[ Reset ]]
 *     │  │─ [[ Shift ]]
 *     │  │─ [[ Resume ]]
 *     │  │─ [[ Hole ]]
 *     │
 *     │─ [[ Implementation ]]
 *
 * -------------------------------------------
 */
sealed trait Tree extends Product {
  /**
   * The number of nodes of this tree (potentially used by inlining heuristics)
   */
  lazy val size: Int = {
    var nodeCount = 1

    def all(t: IterableOnce[_]): Unit = t.iterator.foreach(one)
    def one(obj: Any): Unit = obj match {
      case t: Tree => nodeCount += t.size
      case s: effekt.symbols.Symbol => ()
      case p: Product => all(p.productIterator)
      case t: Iterable[t] => all(t)
      case leaf           => ()
    }
    this.productIterator.foreach(one)
    nodeCount
  }
}

/**
 * In core, all symbols are supposed to be "just" names.
 */
type Id = symbols.Symbol
object Id {
  def apply(n: symbols.Name): Id = new symbols.Symbol {
    val name = n
  }
  def apply(n: String): Id = apply(symbols.Name.local(n))
  def apply(n: Id): Id = apply(n.name)
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  path: String,
  includes: List[String],
  declarations: List[Declaration],
  externs: List[Extern],
  definitions: List[Toplevel],
  exports: List[Id]
) extends Tree {
  def show: String = util.show(this)

  /**
   * Since core programs have free variables before aggregation, this check is not performed automatically
   */
  def typecheck()(using ErrorReporter): Unit = Type.typecheck(this)
}

/**
 * Toplevel data and interface declarations
 */
enum Declaration extends Tree {
  def id: Id

  case Data(id: Id, tparams: List[Id], constructors: List[Constructor])
  case Interface(id: Id, tparams: List[Id], properties: List[Property])
}
export Declaration.*

case class Constructor(id: Id, tparams: List[Id], fields: List[Field]) extends Tree
case class Field(id: Id, tpe: ValueType) extends Tree
case class Property(id: Id, tpe: BlockType) extends Tree

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  case Def(id: Id, tparams: List[Id], cparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: ValueType, annotatedCapture: Captures, body: ExternBody)
  case Include(featureFlag: FeatureFlag, contents: String)
}
sealed trait ExternBody extends Tree
object ExternBody {
  case class StringExternBody(featureFlag: FeatureFlag, contents: Template[Expr]) extends ExternBody
  case class Unsupported(err: util.messages.EffektError) extends ExternBody {
    def report(using E: ErrorReporter): Unit = E.report(err)
  }
}

enum Toplevel {
  def id: Id

  case Def(id: Id, block: Block)
  case Val(id: Id, binding: core.Stmt)
}


/**
 * Pure Expressions (no IO effects, or control effects)
 *
 * ----------[[ effekt.core.Expr ]]----------
 *
 *   ─ [[ Expr ]]
 *     │─ [[ ValueVar ]]
 *     │─ [[ Literal ]]
 *     │─ [[ PureApp ]]
 *     │─ [[ Make ]]
 *     │─ [[ Box ]]
 *
 * -------------------------------------------
 */
enum Expr extends Tree {

  case ValueVar(id: Id, annotatedType: ValueType)

  case Literal(value: Any, annotatedType: ValueType)

  /**
   * Pure FFI calls. Invariant, block b is pure.
   */
  case PureApp(b: Block.BlockVar, targs: List[ValueType], vargs: List[Expr])

  /**
   * Constructor calls
   *
   * Note: the structure mirrors interface implementation
   */
  case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], vargs: List[Expr])

  case Box(b: Block, annotatedCapture: Captures)

  lazy val typing: Typing[ValueType] = Type.typecheck(this)
  lazy val tpe: ValueType = typing.tpe
  lazy val capt: Captures = typing.capt
  lazy val free: Free = typing.free

  // This is to register custom type renderers in IntelliJ -- yes, it has to be a method!
  def show: String = util.show(this)
}
export Expr.*

/**
 * Blocks
 *
 * ----------[[ effekt.core.Block ]]----------
 *
 *   ─ [[ Block ]]
 *     │─ [[ BlockVar ]]
 *     │─ [[ BlockLit ]]
 *     │─ [[ Unbox ]]
 *     │─ [[ New ]]
 *
 * -------------------------------------------
 */
enum Block extends Tree {
  case BlockVar(id: Id, annotatedTpe: BlockType, annotatedCapt: Captures)
  case BlockLit(tparams: List[Id], cparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt)
  case Unbox(pure: Expr)
  case New(impl: Implementation)

  lazy val typing: Typing[BlockType] = Type.typecheck(this)
  lazy val tpe: BlockType = typing.tpe
  lazy val capt: Captures = typing.capt
  lazy val free: Free = typing.free

  def show: String = util.show(this)
}
export Block.*

case class ValueParam(id: Id, tpe: ValueType)
case class BlockParam(id: Id, tpe: BlockType, capt: Captures)


/**
 * Statements
 *
 * ----------[[ effekt.core.Stmt ]]----------
 *
 *   ─ [[ Stmt ]]
 *     │─ [[ Def ]]
 *     │─ [[ Let ]]
 *     │─ [[ Return ]]
 *     │─ [[ Val ]]
 *     │─ [[ App ]]
 *     │─ [[ Invoke ]]
 *     │─ [[ If ]]
 *     │─ [[ Match ]]
 *     │─ [[ Region ]]
 *     │─ [[ Alloc ]]
 *     │─ [[ Var ]]
 *     │─ [[ Get ]]
 *     │─ [[ Put ]]
 *     │─ [[ Reset ]]
 *     │─ [[ Shift ]]
 *     │─ [[ Resume ]]
 *     │─ [[ Hole ]]
 *
 * -------------------------------------------
 */
enum Stmt extends Tree {

  // Definitions
  case Def(id: Id, block: Block, body: Stmt)
  case Let(id: Id, binding: Expr, body: Stmt)
  case ImpureApp(id: Id, callee: Block.BlockVar, targs: List[ValueType], vargs: List[Expr], bargs: List[Block], body: Stmt)

  // Fine-grain CBV
  case Return(expr: Expr)
  case Val(id: Id, binding: Stmt, body: Stmt)
  case App(callee: Block, targs: List[ValueType], vargs: List[Expr], bargs: List[Block])
  case Invoke(callee: Block, method: Id, methodTpe: BlockType, targs: List[ValueType], vargs: List[Expr], bargs: List[Block])

  // Local Control Flow
  case If(cond: Expr, thn: Stmt, els: Stmt)
  case Match(scrutinee: Expr, annotatedTpe: ValueType, clauses: List[(Id, BlockLit)], default: Option[Stmt])

  // (Type-monomorphic?) Regions
  case Region(body: BlockLit)
  case Alloc(id: Id, init: Expr, region: Id, body: Stmt)

  // creates a fresh state handler to model local (backtrackable) state.
  // [[capture]] is a binding occurrence.
  // e.g. state(init) { [x]{x: Ref} => ... }
  case Var(ref: Id, init: Expr, capture: Id, body: Stmt)

  // e.g. let x: T = !ref @ r; body
  case Get(id: Id, annotatedTpe: ValueType, ref: Id, annotatedCapt: Captures, body: Stmt)

  // e.g. ref @ r := value; body
  case Put(ref: Id, annotatedCapt: Captures, value: Expr, body: Stmt)

  // binds a fresh prompt as [[id]] in [[body]] and delimits the scope of captured continuations
  //  Reset({ [cap]{p: Prompt[answer] at cap} => stmt: answer}): answer
  case Reset(body: Block.BlockLit)

  // captures the continuation up to the given prompt
  case Shift(prompt: BlockVar, k: BlockParam, body: Stmt)

  // bidirectional resume: runs the given statement in the original context
  //  Resume(k: Resume[result, answer], stmt: result): answer
  case Resume(k: BlockVar, body: Stmt)

  // Others
  case Hole(annotatedTpe: ValueType, span: effekt.source.Span)

  lazy val typing: Typing[ValueType] = Type.typecheck(this)
  lazy val tpe: ValueType = typing.tpe
  lazy val capt: Captures = typing.capt
  lazy val free: Free = typing.free

  def show: String = util.show(this)
}
export Stmt.*


/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree {
  lazy val typing: Typing[BlockType.Interface] = Type.typecheck(this)
  lazy val tpe: BlockType.Interface = typing.tpe
  lazy val capt: Captures = typing.capt
}

/**
 * Implementation of a method / effect operation.
 */
case class Operation(name: Id, tparams: List[Id], cparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt) extends Tree {
  lazy val typing: Typing[BlockType.Function] = Type.typecheck(this)
  lazy val tpe: BlockType.Function = typing.tpe
  lazy val capt: Captures = typing.capt
}

/**
 * Bindings are not part of the tree but used in transformations
 */
private[core] enum Binding {
  case Val(id: Id, binding: Stmt)
  case Let(id: Id, binding: Expr)
  case ImpureApp(id: Id, callee: Block.BlockVar, targs: List[ValueType], vargs: List[Expr], bargs: List[Block])
  case Def(id: Id, binding: Block)

  def id: Id
}
private[core] object Binding {
  def apply(bindings: List[Binding], body: Stmt): Stmt = bindings match {
    case Nil => body
    case Binding.Val(name, binding) :: rest => Stmt.Val(name, binding, Binding(rest, body))
    case Binding.Let(name, binding) :: rest => Stmt.Let(name, binding, Binding(rest, body))
    case Binding.ImpureApp(name, callee, targs, vargs, bargs) :: rest => Stmt.ImpureApp(name, callee, targs, vargs, bargs, Binding(rest, body))
    case Binding.Def(name, binding) :: rest => Stmt.Def(name, binding, Binding(rest, body))
  }

  def toToplevel(b: Binding): Toplevel = b match {
    case Binding.Val(name, binding) => Toplevel.Val(name, binding)
    case Binding.Let(name, binding) => ??? //Toplevel.Val(name, tpe, Stmt.Return(binding))
    case Binding.ImpureApp(name, callee, targs, vargs, bargs) => ??? //Toplevel.Val(name, tpe, ???)
    case Binding.Def(name, binding) => Toplevel.Def(name, binding)
  }
}

// Binding Monad
// -------------
case class Bind[+A](value: A, bindings: List[Binding]) {
  def run(f: A => Stmt): Stmt = Binding(bindings, f(value))
  def map[B](f: A => B): Bind[B] = Bind(f(value), bindings)
  def flatMap[B](f: A => Bind[B]): Bind[B] =
    val Bind(result, other) = f(value)
    Bind(result, bindings ++ other)
  def apply[B](f: A => Bind[B]): Bind[B] = flatMap(f)
}
object Bind {
  def pure[A](value: A): Bind[A] = Bind(value, Nil)
  def bind[A](expr: Expr): Bind[ValueVar] =
    val id = Id("tmp")
    Bind(ValueVar(id, expr.tpe), List(Binding.Let(id, expr)))

  def bind[A](b: Block.BlockVar, targs: List[ValueType], vargs: List[Expr], bargs: List[Block]): Bind[ValueVar] =
    val id = Id("tmp")
    val binding: Binding.ImpureApp = Binding.ImpureApp(id, b, targs, vargs, bargs)
    Bind(ValueVar(id, Type.bindingType(binding)), List(Binding.ImpureApp(id, b, targs, vargs, bargs)))

  def bind[A](block: Block): Bind[BlockVar] =
    val id = Id("tmp")
    Bind(BlockVar(id, block.tpe, block.capt), List(Binding.Def(id, block)))

  def delimit(b: Bind[Stmt]): Stmt = b.run(a => a)

  def traverse[S, T](l: List[S])(f: S => Bind[T]): Bind[List[T]] =
    l match {
      case Nil => pure(Nil)
      case head :: tail => for { x <- f(head); xs <- traverse(tail)(f) } yield x :: xs
    }
}


object Tree {

  // Generic traversal of trees, applying the partial function `f` to every contained
  // element of type Tree.
  def visit(obj: Any)(f: PartialFunction[Tree, Unit]): Unit = obj match {
    case t: Tree if f.isDefinedAt(t) => f(t)
    case s: symbols.Symbol => ()
    case t: Iterable[t] => t.foreach { t => visit(t)(f) }
    case p: Product => p.productIterator.foreach {
      case t => visit(t)(f)
    }
    case leaf => ()
  }

  trait Query[Ctx, Res] extends Structural {

    def empty: Res
    def combine: (r1: Res, r2: Res) => Res

    def all[T](t: IterableOnce[T], f: T => Res): Res =
      t.iterator.foldLeft(empty) { case (xs, t) => combine(f(t), xs) }

    /**
     * Hook that can be overridden to perform an action at every node in the tree
     */
    def visit[T](t: T)(visitor: Ctx ?=> T => Res)(using Ctx): Res = visitor(t)

    inline def structuralQuery[T](el: T)(using Ctx): Res = visit(el) { t =>
      queryStructurally(t, empty, combine)
    }

    def query(p: Expr)(using Ctx): Res = structuralQuery(p)
    def query(s: Stmt)(using Ctx): Res = structuralQuery(s)
    def query(b: Block)(using Ctx): Res = structuralQuery(b)
    def query(d: Toplevel)(using Ctx): Res = structuralQuery(d)
    def query(d: Implementation)(using Ctx): Res = structuralQuery(d)
    def query(d: Operation)(using Ctx): Res = structuralQuery(d)
    def query(matchClause: (Id, BlockLit))(using Ctx): Res = matchClause match {
        case (id, lit) => query(lit)
    }
    def query(b: ExternBody)(using Ctx): Res = structuralQuery(b)
    def query(m: ModuleDecl)(using Ctx) = structuralQuery(m)
  }

  class Rewrite extends Structural {
    def rewrite(x: Id): Id = x
    def rewrite(p: Expr): Expr = rewriteStructurally(p)
    def rewrite(s: Stmt): Stmt = rewriteStructurally(s)
    def rewrite(block: Block): Block = block match {
      case b : Block.BlockVar => rewrite(b)
      case b : Block.BlockLit => rewrite(b)
      case Block.Unbox(pure) => Block.Unbox(rewrite(pure))
      case Block.New(impl) => Block.New(rewrite(impl))
    }
    def rewrite(d: Toplevel): Toplevel = rewriteStructurally(d)
    def rewrite(e: Implementation): Implementation = rewriteStructurally(e)
    def rewrite(o: Operation): Operation = rewriteStructurally(o)
    def rewrite(p: ValueParam): ValueParam = rewriteStructurally(p)
    def rewrite(p: BlockParam): BlockParam = rewriteStructurally(p)
    def rewrite(b: ExternBody): ExternBody = rewriteStructurally(b)
    def rewrite(e: Extern): Extern = rewriteStructurally(e)
    def rewrite(d: Declaration): Declaration = rewriteStructurally(d)
    def rewrite(c: Constructor): Constructor = rewriteStructurally(c)
    def rewrite(f: Field): Field = rewriteStructurally(f)

    def rewrite(b: BlockLit): BlockLit = b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams map rewrite, cparams map rewrite, vparams map rewrite, bparams map rewrite, rewrite(body))
    }
    def rewrite(b: BlockVar): BlockVar = b match {
      case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(rewrite(id), rewrite(annotatedTpe), rewrite(annotatedCapt))
    }

    def rewrite(t: ValueType): ValueType = rewriteStructurally(t)
    def rewrite(t: ValueType.Data): ValueType.Data = rewriteStructurally(t)

    def rewrite(t: BlockType): BlockType = rewriteStructurally(t)
    def rewrite(t: BlockType.Interface): BlockType.Interface = rewriteStructurally(t)
    def rewrite(capt: Captures): Captures = capt.map(rewrite)
    def rewrite(p: Property): Property = rewriteStructurally(p)

    def rewrite(m: ModuleDecl): ModuleDecl =
      m match {
        case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
          ModuleDecl(path, includes, declarations, externs, definitions.map(rewrite), exports)
      }

    def rewrite(matchClause: (Id, BlockLit)): (Id, BlockLit) = matchClause match {
      case (p, b) => (p, rewrite(b))
    }
  }

  class RewriteTrampolined {

    import Trampoline.done

    final def all[T](xs: List[T], f: T => Trampoline[T]): Trampoline[List[T]] = xs match {
      case Nil => done(Nil)
      case x :: xs => f(x).flatMap(x => all(xs, f).map(x :: _))
    }

    final def opt[T](xs: Option[T], f: T => Trampoline[T]): Trampoline[Option[T]] = xs match {
      case Some(value) => f(value).map(v => Some(v))
      case None => done(None)
    }

    def rewrite(x: Id): Id = x

    def rewrite(e: Expr): Trampoline[Expr] = e match {
      case Expr.ValueVar(id, tpe) => done(Expr.ValueVar(rewrite(id), rewrite(tpe)))
      case Expr.Literal(value, tpe) => done(Expr.Literal(value, rewrite(tpe)))
      case Expr.PureApp(b, targs, vargs) => for {
        b2     <- done(rewrite(b))
        targs2 <- done(targs.map(rewrite))
        vargs2 <- all(vargs, rewrite)
      } yield Expr.PureApp(b2, targs2, vargs2)
      case Expr.Make(data, tag, targs, vargs) => for {
        data2  <- done(rewrite(data))
        tag2   <- done(rewrite(tag))
        targs2 <- done(targs.map(rewrite))
        vargs2 <- all(vargs, rewrite)
      } yield Expr.Make(data2, tag2, targs2, vargs2)
      case Expr.Box(b, capt) => for {
        b2 <- rewrite(b)
        capt2 <- done(rewrite(capt))
      } yield Expr.Box(b2, capt2)
    }

    def rewrite(s: Stmt): Trampoline[Stmt] = s match {
      case Stmt.Def(id, block, body) => for {
        id2    <- done(rewrite(id))
        block2 <- rewrite(block)
        body2  <- rewrite(body)
      } yield Stmt.Def(id2, block2, body2)

      case Stmt.Let(id, binding, body) => for {
        id2      <- done(rewrite(id))
        binding2 <- rewrite(binding)
        body2    <- rewrite(body)
      } yield Stmt.Let(id2, binding2, body2)

      case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => for {
        id2     <- done(rewrite(id))
        callee2 <- done(rewrite(callee))
        targs2  <- done(targs.map(rewrite))
        vargs2  <- all(vargs, rewrite)
        bargs2  <- all(bargs, rewrite)
        body2   <- rewrite(body)
      } yield Stmt.ImpureApp(id2, callee2, targs2, vargs2, bargs2, body2)

      case Stmt.Return(expr) => for {
        expr2 <- rewrite(expr)
      } yield Stmt.Return(expr2)

      case Stmt.Val(id, binding, body) => for {
        id2      <- done(rewrite(id))
        binding2 <- rewrite(binding)
        body2    <- rewrite(body)
      } yield Stmt.Val(id2, binding2, body2)

      case Stmt.App(callee, targs, vargs, bargs) => for {
        callee2 <- rewrite(callee)
        targs2  <- done(targs.map(rewrite))
        vargs2  <- all(vargs, rewrite)
        bargs2  <- all(bargs, rewrite)
      } yield Stmt.App(callee2, targs2, vargs2, bargs2)

      case Stmt.Invoke(callee, method, tpe, targs, vargs, bargs) => for {
        callee2 <- rewrite(callee)
        method2 <- done(rewrite(method))
        tpe2    <- done(rewrite(tpe))
        targs2  <- done(targs.map(rewrite))
        vargs2  <- all(vargs, rewrite)
        bargs2  <- all(bargs, rewrite)
      } yield Stmt.Invoke(callee2, method2, tpe2, targs2, vargs2, bargs2)

      case Stmt.If(cond, thn, els) => for {
        cond2 <- rewrite(cond)
        thn2  <- rewrite(thn)
        els2  <- rewrite(els)
      } yield Stmt.If(cond2, thn2, els2)

      case Stmt.Match(scrutinee, tpe, clauses, default) => for {
        scrutinee2 <- rewrite(scrutinee)
        tpe2       <- done(rewrite(tpe))
        clauses2   <- all(clauses, rewrite)
        default2   <- opt(default, rewrite)
      } yield Stmt.Match(scrutinee2, tpe2, clauses2, default2)

      case Stmt.Region(body) => for {
        body2 <- rewrite(body)
      } yield Stmt.Region(body2)

      case Stmt.Alloc(id, init, region, body) => for {
        id2     <- done(rewrite(id))
        init2   <- rewrite(init)
        region2 <- done(rewrite(region))
        body2   <- rewrite(body)
      } yield Stmt.Alloc(id2, init2, region2, body2)

      case Stmt.Var(ref, init, capture, body) => for {
        ref2     <- done(rewrite(ref))
        init2    <- rewrite(init)
        capture2 <- done(rewrite(capture))
        body2    <- rewrite(body)
      } yield Stmt.Var(ref2, init2, capture2, body2)

      case Stmt.Get(id, tpe, ref, annotatedCapt, body) => for {
        id2   <- done(rewrite(id))
        tpe2  <- done(rewrite(tpe))
        ref2  <- done(rewrite(ref))
        body2 <- rewrite(body)
      } yield Stmt.Get(id2, tpe2, ref2, annotatedCapt, body2)

      case Stmt.Put(ref, annotatedCapt, value, body) => for {
        ref2   <- done(rewrite(ref))
        value2 <- rewrite(value)
        body2  <- rewrite(body)
      } yield Stmt.Put(ref2, annotatedCapt, value2, body2)

      case Stmt.Reset(body) => for {
        body2 <- rewrite(body)
      } yield Stmt.Reset(body2)

      case Stmt.Shift(prompt, k, body) => for {
        prompt2 <- done(rewrite(prompt))
        k2      <- done(rewrite(k))
        body2   <- rewrite(body)
      } yield Stmt.Shift(prompt2, k2, body2)

      case Stmt.Resume(k, body) => for {
        k2    <- done(rewrite(k))
        body2 <- rewrite(body)
      } yield Stmt.Resume(k2, body2)

      case Stmt.Hole(tpe, span) =>
        done(Stmt.Hole(rewrite(tpe), span))
    }

    def rewrite(b: Block): Trampoline[Block] = b match {
      case Block.BlockVar(id, tpe, capt) =>
        done(Block.BlockVar(rewrite(id), rewrite(tpe), rewrite(capt)))
      case lit: Block.BlockLit => rewrite(lit: Block.BlockLit)
      case Block.Unbox(pure) => rewrite(pure).map(pure2 => Block.Unbox(pure2))
      case Block.New(impl) => rewrite(impl).map(impl2 => Block.New(impl2))
    }

    def rewrite(impl: Implementation): Trampoline[Implementation] = impl match {
      case Implementation(interface, operations) => for {
        interface2  <- done(rewrite(interface))
        operations2 <- all(operations, rewrite)
      } yield Implementation(interface2, operations2)
    }

    def rewrite(o: Operation): Trampoline[Operation] = o match {
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        val name2 = rewrite(name)
        val tparams2 = tparams map rewrite
        val cparams2 = cparams map rewrite
        val vparams2 = vparams map rewrite
        val bparams2 = bparams map rewrite
        rewrite(body).map { body2 => Operation(name2, tparams2, cparams2, vparams2, bparams2, body2) }
    }

    def rewrite(p: ValueParam): ValueParam = p match {
      case ValueParam(id, tpe) => ValueParam(rewrite(id), rewrite(tpe))
    }

    def rewrite(p: BlockParam): BlockParam = p match {
      case BlockParam(id, tpe, capt) => BlockParam(rewrite(id), rewrite(tpe), rewrite(capt))
    }

    def rewrite(b: ExternBody): Trampoline[ExternBody] = b match {
      case ExternBody.StringExternBody(featureFlag, Template(strings, args)) =>
        all(args, rewrite).map { args2 => ExternBody.StringExternBody(featureFlag, Template(strings, args2)) }
      case ExternBody.Unsupported(err) => done(b)
    }

    def rewrite(d: Toplevel): Toplevel = d match {
      case Toplevel.Def(id, block) => Toplevel.Def(rewrite(id), rewrite(block).run())
      case Toplevel.Val(id, binding) => Toplevel.Val(rewrite(id), rewrite(binding).run())
    }

    def rewrite(e: Extern): Extern = e match {
      case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
        Extern.Def(rewrite(id), tparams.map(rewrite), cparams.map(rewrite), vparams.map(rewrite), bparams.map(rewrite),
          rewrite(ret), rewrite(annotatedCapture), rewrite(body).run())
      case Extern.Include(featureFlag, contents) => e
    }

    def rewrite(d: Declaration): Declaration = d match {
      case Declaration.Data(id, tparams, constructors) => Declaration.Data(rewrite(id), tparams.map(rewrite), constructors.map(rewrite))
      case Declaration.Interface(id, tparams, properties) => Declaration.Interface(rewrite(id), tparams.map(rewrite), properties.map(rewrite))
    }

    def rewrite(c: Constructor): Constructor = c match {
      case Constructor(id, tparams, params) => Constructor(rewrite(id), tparams.map(rewrite), params.map(rewrite))
    }

    def rewrite(f: Field): Field = f match {
      case Field(id, tpe) => Field(rewrite(id), rewrite(tpe))
    }

    def rewrite(b: BlockLit): Trampoline[BlockLit] = b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        val tparams2 = tparams map rewrite
        val cparams2 = cparams map rewrite
        val vparams2 = vparams map rewrite
        val bparams2 = bparams map rewrite
        rewrite(body).map { body2 => BlockLit(tparams2, cparams2, vparams2, bparams2, body2) }
    }
    def rewrite(b: BlockVar): BlockVar = b match {
      case BlockVar(id, tpe, capt) => BlockVar(rewrite(id), rewrite(tpe), rewrite(capt))
    }

    def rewrite(tpe: ValueType): ValueType = tpe match {
      case ValueType.Var(name) => ValueType.Var(rewrite(name))
      case data: ValueType.Data => rewrite(data)
      case ValueType.Boxed(tpe, capt) => ValueType.Boxed(rewrite(tpe), rewrite(capt))
    }
    def rewrite(tpe: ValueType.Data): ValueType.Data = tpe match {
      case ValueType.Data(name, targs) => ValueType.Data(rewrite(name), targs.map(rewrite))
    }

    def rewrite(tpe: BlockType): BlockType = tpe match {
      case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
        BlockType.Function(tparams.map(rewrite), cparams.map(rewrite), vparams.map(rewrite), bparams.map(rewrite), rewrite(result))
      case interface: BlockType.Interface => rewrite(interface)
    }
    def rewrite(tpe: BlockType.Interface): BlockType.Interface = tpe match {
      case BlockType.Interface(name, targs) => BlockType.Interface(rewrite(name), targs.map(rewrite))
    }
    def rewrite(capt: Captures): Captures = capt.map(rewrite)
    def rewrite(prop: Property): Property = prop match {
      case Property(id, tpe) => Property(rewrite(id), rewrite(tpe))
    }

    def rewrite(m: ModuleDecl): ModuleDecl =
      m match {
        case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
          ModuleDecl(path, includes, declarations.map(rewrite), externs.map(rewrite), definitions.map(rewrite), exports)
      }

    def rewrite(matchClause: (Id, BlockLit)): Trampoline[(Id, BlockLit)] = matchClause match {
      case (p, b) => rewrite(b).map { b2 => (p, b2) }
    }
  }

  class RewriteWithContext[Ctx] extends Structural {

    def rewrite(x: Id)(using Ctx): Id = x
    def rewrite(p: Expr)(using Ctx): Expr = rewriteStructurally(p)
    def rewrite(s: Stmt)(using Ctx): Stmt = rewriteStructurally(s)
    def rewrite(b: Block)(using Ctx): Block = b match {
      case b : Block.BlockVar => rewrite(b)
      case b : Block.BlockLit => rewrite(b)
      case Block.Unbox(pure) => Block.Unbox(rewrite(pure))
      case Block.New(impl) => Block.New(rewrite(impl))
    }
    def rewrite(d: Toplevel)(using Ctx): Toplevel = rewriteStructurally(d)
    def rewrite(e: Implementation)(using Ctx): Implementation = rewriteStructurally(e)
    def rewrite(o: Operation)(using Ctx): Operation = rewriteStructurally(o)
    def rewrite(p: ValueParam)(using Ctx): ValueParam = rewriteStructurally(p)
    def rewrite(p: BlockParam)(using Ctx): BlockParam = rewriteStructurally(p)
    def rewrite(b: ExternBody)(using Ctx): ExternBody= rewrite(b)

    def rewrite(b: BlockLit)(using Ctx): BlockLit = b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams map rewrite, cparams map rewrite, vparams map rewrite, bparams map rewrite, rewrite(body))
    }
    def rewrite(b: BlockVar)(using Ctx): BlockVar = b match {
      case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(rewrite(id), rewrite(annotatedTpe), rewrite(annotatedCapt))
    }

    def rewrite(t: ValueType)(using Ctx): ValueType = rewriteStructurally(t)
    def rewrite(t: ValueType.Data)(using Ctx): ValueType.Data = rewriteStructurally(t)

    def rewrite(t: BlockType)(using Ctx): BlockType = rewriteStructurally(t)
    def rewrite(t: BlockType.Interface)(using Ctx): BlockType.Interface = rewriteStructurally(t)
    def rewrite(capt: Captures)(using Ctx): Captures = capt.map(rewrite)

    def rewrite(m: ModuleDecl)(using Ctx): ModuleDecl =
      m match {
        case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
          ModuleDecl(path, includes, declarations, externs, definitions.map(rewrite), exports)
      }

    def rewrite(matchClause: (Id, BlockLit))(using Ctx): (Id, BlockLit) = matchClause match {
      case (p, b) => (p, rewrite(b))
    }
  }
}

object substitutions {

  case class Substitution(
    vtypes: Map[Id, ValueType],
    captures: Map[Id, Captures],
    values: Map[Id, Expr],
    blocks: Map[Id, Block]
  ) {
    def shadowTypes(shadowed: IterableOnce[Id]): Substitution = copy(vtypes = vtypes -- shadowed)
    def shadowCaptures(shadowed: IterableOnce[Id]): Substitution = copy(captures = captures -- shadowed)
    def shadowValues(shadowed: IterableOnce[Id]): Substitution = copy(values = values -- shadowed)
    def shadowBlocks(shadowed: IterableOnce[Id]): Substitution = copy(blocks = blocks -- shadowed)

    def shadowParams(vparams: Seq[ValueParam], bparams: Seq[BlockParam]): Substitution =
      copy(values = values -- vparams.map(_.id), blocks = blocks -- bparams.map(_.id))
  }

  // Starting point for inlining, creates Maps(params -> args) and passes to normal substitute
  def substitute(block: BlockLit, targs: List[ValueType], vargs: List[Expr], bargs: List[Block]): Stmt =
    block match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        assert(tparams.size == targs.size, s"Wrong number of type arguments: ${tparams.map(util.show)} vs ${targs.map(util.show)}")
        assert(cparams.size == bargs.size, s"Wrong number of capture arguments: ${cparams} vs ${bargs}")
        assert(bparams.size == bargs.size, s"Wrong number of block arguments: ${bparams} vs ${bargs}")
        assert(vparams.size == vargs.size, s"Wrong number of value arguments: ${vparams} vs ${vargs}")
        val tSubst = (tparams zip targs).toMap
        val cSubst = (cparams zip bargs.map(_.capt)).toMap
        val vSubst = (vparams.map(_.id) zip vargs).toMap
        val bSubst = (bparams.map(_.id) zip bargs).toMap

        substitute(body)(using Substitution(tSubst, cSubst, vSubst, bSubst))
    }

  def substitute(statement: Stmt)(using subst: Substitution): Stmt =
    statement match {
      case Def(id, block, body) =>
        Def(id, substitute(block)(using subst shadowBlocks List(id)),
          substitute(body)(using subst shadowBlocks List(id)))

      case Let(id, binding, body) =>
        Let(id, substitute(binding),
          substitute(body)(using subst shadowValues List(id)))

      case ImpureApp(id, callee, targs, vargs, bargs, body) =>
        substitute(callee) match {
          case g : Block.BlockVar =>
            ImpureApp(id, g, targs.map(substitute), vargs.map(substitute), bargs.map(substitute),
              substitute(body)(using subst shadowValues List(id)))
          case _ => INTERNAL_ERROR("Should never substitute a concrete block for an FFI function.")
        }


      case Return(expr) =>
        Return(substitute(expr))

      case Val(id, binding, body) =>
        Val(id, substitute(binding),
          substitute(body)(using subst shadowValues List(id)))

      case App(callee, targs, vargs, bargs) =>
        App(substitute(callee), targs.map(substitute), vargs.map(substitute), bargs.map(substitute))

      case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
        Invoke(substitute(callee), method, substitute(methodTpe), targs.map(substitute), vargs.map(substitute), bargs.map(substitute))

      case If(cond, thn, els) =>
        If(substitute(cond), substitute(thn), substitute(els))

      case Match(scrutinee, tpe, clauses, default) =>
        Match(substitute(scrutinee), substitute(tpe), clauses.map {
          case (id, b) => (id, substitute(b))
        }, default.map(substitute))

      case Alloc(id, init, region, body) =>
        Alloc(id, substitute(init), substituteAsVar(region),
          substitute(body)(using subst shadowBlocks List(id)))

      case Var(ref, init, capture, body) =>
        Var(ref, substitute(init), capture, substitute(body)(using subst shadowBlocks List(ref)))

      case Get(id, tpe, ref, capt, body) =>
        Get(id, substitute(tpe), substituteAsVar(ref), substitute(capt), substitute(body)(using subst shadowBlocks List(id)))

      case Put(ref, capt, value, body) =>
        Put(substituteAsVar(ref), substitute(capt), substitute(value), substitute(body))

      // We annotate the answer type here since it needs to be the union of body.tpe and all shifts
      case Reset(body) =>
        Reset(substitute(body))

      case Shift(prompt, k, body) =>
        val after = substitute(body)(using subst shadowBlocks List(k.id))
        Shift(substitute(prompt).asInstanceOf[BlockVar], substitute(k), after)

      case Resume(k, body) =>
        Resume(substitute(k).asInstanceOf[BlockVar], substitute(body))

      case Region(body) =>
        Region(substitute(body))

      case Hole(tpe, span) => Hole(substitute(tpe), span)
    }

  def substitute(b: BlockLit)(using subst: Substitution): BlockLit = b match {
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      val shadowedTypelevel = subst shadowTypes tparams shadowCaptures cparams
      BlockLit(tparams, cparams,
        vparams.map(p => substitute(p)(using shadowedTypelevel)),
        bparams.map(p => substitute(p)(using shadowedTypelevel)),
        substitute(body)(using shadowedTypelevel.shadowParams(vparams, bparams)))
  }

  def substituteAsVar(id: Id)(using subst: Substitution): Id =
    subst.blocks.get(id) map {
      case BlockVar(x, _, _) => x
      case _ => INTERNAL_ERROR("Regions should always be variables")
    } getOrElse id

  def substitute(block: Block)(using subst: Substitution): Block =
    block match {
      case BlockVar(id, tpe, capt) if subst.blocks.isDefinedAt(id) => subst.blocks(id)
      case BlockVar(id, tpe, capt) => BlockVar(id, substitute(tpe), substitute(capt))
      case b: BlockLit => substitute(b)
      case Unbox(pure) => Unbox(substitute(pure))
      case New(impl) => New(substitute(impl))
    }

  def substitute(pure: Expr)(using subst: Substitution): Expr =
    pure match {
      case ValueVar(id, _) if subst.values.isDefinedAt(id) => subst.values(id)
      case ValueVar(id, annotatedType) => ValueVar(id, substitute(annotatedType))

      case Literal(value, annotatedType) =>
        Literal(value, substitute(annotatedType))

      case Make(tpe, tag, targs, vargs) =>
        Make(substitute(tpe).asInstanceOf, tag, targs.map(substitute), vargs.map(substitute))

      case PureApp(f, targs, vargs) => substitute(f) match {
        case g : Block.BlockVar => PureApp(g, targs.map(substitute), vargs.map(substitute))
        case _ => INTERNAL_ERROR("Should never substitute a concrete block for an FFI function.")
      }

      case Box(b, annotatedCapture) =>
        Box(substitute(b), substitute(annotatedCapture))
    }

  def substitute(impl: Implementation)(using Substitution): Implementation =
    impl match {
      case Implementation(interface, operations) =>
        Implementation(substitute(interface).asInstanceOf, operations.map(substitute))
    }

  def substitute(op: Operation)(using subst: Substitution): Operation =
    op match {
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        val shadowedTypelevel = subst shadowTypes tparams shadowCaptures cparams
        Operation(name, tparams, cparams,
          vparams.map(p => substitute(p)(using shadowedTypelevel)),
          bparams.map(p => substitute(p)(using shadowedTypelevel)),
          substitute(body)(using shadowedTypelevel.shadowParams(vparams, bparams)))
    }

  def substitute(param: ValueParam)(using Substitution): ValueParam =
    param match {
      case ValueParam(id, tpe) => ValueParam(id, substitute(tpe))
    }

  def substitute(param: BlockParam)(using Substitution): BlockParam =
    param match {
      case BlockParam(id, tpe, capt) => BlockParam(id, substitute(tpe), substitute(capt))
    }

  def substitute(tpe: ValueType)(using subst: Substitution): ValueType =
    Type.substitute(tpe, subst.vtypes, subst.captures)

  def substitute(tpe: BlockType)(using subst: Substitution): BlockType =
    Type.substitute(tpe, subst.vtypes, subst.captures)

  def substitute(capt: Captures)(using subst: Substitution): Captures =
    Type.substitute(capt, subst.captures)
}

