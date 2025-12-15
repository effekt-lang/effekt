package effekt
package core

import effekt.source.FeatureFlag
import effekt.util.Structural
import effekt.util.messages.INTERNAL_ERROR
import effekt.util.messages.ErrorReporter

import scala.annotation.{ tailrec, targetName }

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

  val typing: Typing[ValueType] = Type.typecheck(this)
  val tpe: ValueType = typing.tpe
  val capt: Captures = typing.capt
  val free: Free = typing.free

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

  val typing: Typing[BlockType] = Type.typecheck(this)
  val tpe: BlockType = typing.tpe
  val capt: Captures = typing.capt
  val free: Free = typing.free

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

  val typing: Typing[ValueType] = Type.typecheck(this)
  val tpe: ValueType = typing.tpe
  val capt: Captures = typing.capt
  val free: Free = typing.free

  def show: String = util.show(this)
}
export Stmt.*


/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree {
  val typing: Typing[BlockType.Interface] = Type.typecheck(this)
  val tpe: BlockType.Interface = typing.tpe
  val capt: Captures = typing.capt
}

/**
 * Implementation of a method / effect operation.
 *
 * TODO drop resume here since it is not needed anymore...
 */
case class Operation(name: Id, tparams: List[Id], cparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt) extends Tree {
  val typing: Typing[BlockType.Function] = Type.typecheck(this)
  val tpe: BlockType.Function = typing.tpe
  val capt: Captures = typing.capt
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

    def pure(using Ctx): PartialFunction[Expr, Res] = PartialFunction.empty
    def stmt(using Ctx): PartialFunction[Stmt, Res] = PartialFunction.empty
    def block(using Ctx): PartialFunction[Block, Res] = PartialFunction.empty
    def toplevel(using Ctx): PartialFunction[Toplevel, Res] = PartialFunction.empty
    def implementation(using Ctx): PartialFunction[Implementation, Res] = PartialFunction.empty
    def operation(using Ctx): PartialFunction[Operation, Res] = PartialFunction.empty
    def clause(using Ctx): PartialFunction[(Id, BlockLit), Res] = PartialFunction.empty
    def externBody(using Ctx): PartialFunction[ExternBody, Res] = PartialFunction.empty

    /**
     * Hook that can be overridden to perform an action at every node in the tree
     */
    def visit[T](t: T)(visitor: Ctx ?=> T => Res)(using Ctx): Res = visitor(t)

    inline def structuralQuery[T](el: T, pf: PartialFunction[T, Res])(using Ctx): Res = visit(el) { t =>
      if pf.isDefinedAt(el) then pf.apply(el) else queryStructurally(t, empty, combine)
    }

    def query(p: Expr)(using Ctx): Res = structuralQuery(p, pure)
    def query(s: Stmt)(using Ctx): Res = structuralQuery(s, stmt)
    def query(b: Block)(using Ctx): Res = structuralQuery(b, block)
    def query(d: Toplevel)(using Ctx): Res = structuralQuery(d, toplevel)
    def query(d: Implementation)(using Ctx): Res = structuralQuery(d, implementation)
    def query(d: Operation)(using Ctx): Res = structuralQuery(d, operation)
    def query(matchClause: (Id, BlockLit))(using Ctx): Res =
      if clause.isDefinedAt(matchClause) then clause.apply(matchClause) else matchClause match {
        case (id, lit) => query(lit)
    }
    def query(b: ExternBody)(using Ctx): Res = structuralQuery(b, externBody)
    def query(m: ModuleDecl)(using Ctx) = structuralQuery(m, PartialFunction.empty)
  }

  class Rewrite extends Structural {
    def id: PartialFunction[Id, Id] = PartialFunction.empty
    def pure: PartialFunction[Expr, Expr] = PartialFunction.empty
    def stmt: PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def toplevel: PartialFunction[Toplevel, Toplevel] = PartialFunction.empty
    def block: PartialFunction[Block, Block] = PartialFunction.empty
    def implementation: PartialFunction[Implementation, Implementation] = PartialFunction.empty

    def rewrite(x: Id): Id = if id.isDefinedAt(x) then id(x) else x
    def rewrite(p: Expr): Expr = rewriteStructurally(p, pure)
    def rewrite(s: Stmt): Stmt = rewriteStructurally(s, stmt)
    def rewrite(b: Block): Block = rewriteStructurally(b, block)
    def rewrite(d: Toplevel): Toplevel = rewriteStructurally(d, toplevel)
    def rewrite(e: Implementation): Implementation = rewriteStructurally(e, implementation)
    def rewrite(o: Operation): Operation = rewriteStructurally(o)
    def rewrite(p: ValueParam): ValueParam = rewriteStructurally(p)
    def rewrite(p: BlockParam): BlockParam = rewriteStructurally(p)
    def rewrite(b: ExternBody): ExternBody= rewriteStructurally(b)
    def rewrite(e: Extern): Extern= rewriteStructurally(e)
    def rewrite(d: Declaration): Declaration = rewriteStructurally(d)
    def rewrite(c: Constructor): Constructor = rewriteStructurally(c)
    def rewrite(f: Field): Field = rewriteStructurally(f)

    def rewrite(b: BlockLit): BlockLit = if block.isDefinedAt(b) then block(b).asInstanceOf else b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams map rewrite, cparams map rewrite, vparams map rewrite, bparams map rewrite, rewrite(body))
    }
    def rewrite(b: BlockVar): BlockVar = if block.isDefinedAt(b) then block(b).asInstanceOf else b match {
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

  class RewriteWithContext[Ctx] extends Structural {
    def id(using Ctx): PartialFunction[Id, Id] = PartialFunction.empty
    def expr(using Ctx): PartialFunction[Expr, Expr] = PartialFunction.empty
    def stmt(using Ctx): PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def toplevel(using Ctx): PartialFunction[Toplevel, Toplevel] = PartialFunction.empty
    def block(using Ctx): PartialFunction[Block, Block] = PartialFunction.empty
    def implementation(using Ctx): PartialFunction[Implementation, Implementation] = PartialFunction.empty

    def rewrite(x: Id)(using Ctx): Id = if id.isDefinedAt(x) then id(x) else x
    def rewrite(p: Expr)(using Ctx): Expr = rewriteStructurally(p, expr)
    def rewrite(s: Stmt)(using Ctx): Stmt = rewriteStructurally(s, stmt)
    def rewrite(b: Block)(using Ctx): Block = rewriteStructurally(b, block)
    def rewrite(d: Toplevel)(using Ctx): Toplevel = rewriteStructurally(d, toplevel)
    def rewrite(e: Implementation)(using Ctx): Implementation = rewriteStructurally(e, implementation)
    def rewrite(o: Operation)(using Ctx): Operation = rewriteStructurally(o)
    def rewrite(p: ValueParam)(using Ctx): ValueParam = rewriteStructurally(p)
    def rewrite(p: BlockParam)(using Ctx): BlockParam = rewriteStructurally(p)
    def rewrite(b: ExternBody)(using Ctx): ExternBody= rewrite(b)

    def rewrite(b: BlockLit)(using Ctx): BlockLit = if block.isDefinedAt(b) then block(b).asInstanceOf else b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(tparams map rewrite, cparams map rewrite, vparams map rewrite, bparams map rewrite, rewrite(body))
    }
    def rewrite(b: BlockVar)(using Ctx): BlockVar = if block.isDefinedAt(b) then block(b).asInstanceOf else b match {
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

@targetName("preserveTypesStmt")
inline def preserveTypes(before: Stmt)(inline f: Stmt => Stmt): Stmt = {
  val after = f(before)
  assert(Type.equals(before.tpe, after.tpe), s"Normalization doesn't preserve types.\nBefore: ${before.tpe}\nAfter:  ${after.tpe}\n\nTree before:\n${util.show(before)}\n\nTree after:\n${util.show(after)}")
  after
}

@targetName("preserveTypesExpr")
inline def preserveTypes(before: Expr)(inline f: Expr => Expr): Expr = {
  val after = f(before)
  assert(Type.equals(before.tpe, after.tpe), s"Normalization doesn't preserve types.\nBefore: ${before.tpe}\nAfter:  ${after.tpe}\n\nTree before:\n${util.show(before)}\n\nTree after:\n${util.show(after)}")
  after
}

@targetName("preserveTypesBlock")
inline def preserveTypes(before: Block)(inline f: Block => Block): Block = {
  val after = f(before)
  assert(Type.equals(before.tpe, after.tpe), s"Normalization doesn't preserve types.\nBefore: ${before.tpe}\nAfter:  ${after.tpe}\n\nTree before:\n${util.show(before)}\n\nTree after:\n${util.show(after)}")
  after
}
