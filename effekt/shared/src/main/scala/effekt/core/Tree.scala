package effekt
package core

import effekt.source.FeatureFlag
import effekt.util.Structural
import effekt.util.messages.INTERNAL_ERROR

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
 *     │─ [[ Definition ]]
 *     │  │─ [[ Def ]]
 *     │  │─ [[ Let ]]
 *     │
 *     │─ [[ Expr ]]
 *     │  │─ [[ DirectApp ]]
 *     │  │─ [[ Run ]]
 *     │  │─ [[ Pure ]]
 *     │
 *     │─ [[ Block ]]
 *     │  │─ [[ BlockVar ]]
 *     │  │─ [[ BlockLit ]]
 *     │  │─ [[ Member ]]
 *     │  │─ [[ Unbox ]]
 *     │  │─ [[ New ]]
 *     │
 *     │─ [[ Param ]]
 *     │  │─ [[ ValueParam ]]
 *     │  │─ [[ BlockParam ]]
 *     │
 *     │─ [[ Stmt ]]
 *     │  │─ [[ Scope ]]
 *     │  │─ [[ Return ]]
 *     │  │─ [[ Val ]]
 *     │  │─ [[ App ]]
 *     │  │─ [[ If ]]
 *     │  │─ [[ Match ]]
 *     │  │─ [[ Region ]]
 *     │  │─ [[ Alloc ]]
 *     │  │─ [[ Var ]]
 *     │  │─ [[ Get ]]
 *     │  │─ [[ Put ]]
 *     │  │─ [[ Try ]]
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
  def apply(n: String): Id = new symbols.Symbol {
    val name = symbols.Name.local(n)
  }
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  path: String,
  includes: List[String],
  declarations: List[Declaration],
  externs: List[Extern],
  definitions: List[Definition],
  exports: List[Id]
) extends Tree

/**
 * Toplevel data and interface declarations
 */
enum Declaration extends Tree {
  def id: Id

  case Data(id: Id, tparams: List[Id], constructors: List[Constructor])
  case Interface(id: Id, tparams: List[Id], properties: List[Property])
}
export Declaration.*

case class Constructor(id: Id, fields: List[Field]) extends Tree
case class Field(id: Id, tpe: ValueType) extends Tree
case class Property(id: Id, tpe: BlockType) extends Tree

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  case Def(id: Id, tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], ret: ValueType, annotatedCapture: Captures, body: ExternBody)
  case Include(featureFlag: FeatureFlag, contents: String)
}
case class ExternBody(featureFlag: FeatureFlag, contents: Template[Pure]) extends Tree

extension(self: List[ExternBody]) {
  def forFeatureFlags(flags: List[String]): Option[ExternBody] = flags match {
    case Nil => self.find( _.featureFlag.isDefault )
    case flag :: other =>
      self.find( _.featureFlag.matches(flag, false) ) orElse { self.forFeatureFlags(other) }
  }
}


enum Definition extends Tree {
  def id: Id

  case Def(id: Id, block: Block)
  case Let(id: Id, binding: Expr) // PURE on the toplevel?

  // TBD
  // case Var(id: Symbol,  region: Symbol, init: Pure) // TOPLEVEL could only be {global}, or not at all.

  // TDB
  // case Mutual(defs: List[Definition.Def])
  val capt: Captures = Type.inferCapt(this)
}

// Some smart constructors
private def addToScope(definition: Definition, body: Stmt): Stmt = body match {
  case Scope(definitions, body) => Scope(definition :: definitions, body)
  case other => Scope(List(definition), other)
}

def Def(id: Id, block: Block, rest: Stmt) =
  addToScope(Definition.Def(id, block), rest)

def Let(id: Id, binding: Expr, rest: Stmt) =
  addToScope(Definition.Let(id,  binding), rest)


/**
 * Expressions (with potential IO effects)
 *
 * - [[DirectApp]]
 * - [[Run]]
 * - [[Pure]]
 */
sealed trait Expr extends Tree {
  val tpe: ValueType = Type.inferType(this)
  val capt: Captures = Type.inferCapt(this)
}

// invariant, block b is {io}.
case class DirectApp(b: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]) extends Expr

// only inserted by the transformer if stmt is pure / io
case class Run(s: Stmt) extends Expr


/**
 * Pure Expressions (no IO effects, or control effects)
 *
 * ----------[[ effekt.core.Pure ]]----------
 *
 *   ─ [[ Pure ]]
 *     │─ [[ ValueVar ]]
 *     │─ [[ Literal ]]
 *     │─ [[ PureApp ]]
 *     │─ [[ Make ]]
 *     │─ [[ Select ]]
 *     │─ [[ Box ]]
 *
 * -------------------------------------------
 */
enum Pure extends Expr {

  case ValueVar(id: Id, annotatedType: ValueType)

  case Literal(value: Any, annotatedType: ValueType)

  /**
   * Pure FFI calls. Invariant, block b is pure.
   */
  case PureApp(b: Block, targs: List[ValueType], vargs: List[Pure])

  /**
   * Constructor calls
   *
   * Note: the structure mirrors interface implementation
   */
  case Make(data: ValueType.Data, tag: Id, vargs: List[Pure])

  /**
   * Record Selection
   */
  case Select(target: Pure, field: Id, annotatedType: ValueType)

  case Box(b: Block, annotatedCapture: Captures)
}
export Pure.*

/**
 * Blocks
 *
 * ----------[[ effekt.core.Block ]]----------
 *
 *   ─ [[ Block ]]
 *     │─ [[ BlockVar ]]
 *     │─ [[ BlockLit ]]
 *     │─ [[ Member ]]
 *     │─ [[ Unbox ]]
 *     │─ [[ New ]]
 *
 * -------------------------------------------
 */
enum Block extends Tree {
  case BlockVar(id: Id, annotatedTpe: BlockType, annotatedCapt: Captures)
  case BlockLit(tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], body: Stmt)
  case Member(block: Block, field: Id, annotatedTpe: BlockType)
  case Unbox(pure: Pure)
  case New(impl: Implementation)

  val tpe: BlockType = Type.inferType(this)
  val capt: Captures = Type.inferCapt(this)
}
export Block.*

enum Param extends Tree {
  def id: Id

  case ValueParam(id: Id, tpe: ValueType)
  case BlockParam(id: Id, tpe: BlockType, capt: Captures)
}
export Param.*

/**
 * Statements
 *
 * ----------[[ effekt.core.Stmt ]]----------
 *
 *   ─ [[ Stmt ]]
 *     │─ [[ Scope ]]
 *     │─ [[ Return ]]
 *     │─ [[ Val ]]
 *     │─ [[ App ]]
 *     │─ [[ If ]]
 *     │─ [[ Match ]]
 *     │─ [[ Region ]]
 *     │─ [[ Alloc ]]
 *     │─ [[ Var ]]
 *     │─ [[ Get ]]
 *     │─ [[ Put ]]
 *     │─ [[ Try ]]
 *     │─ [[ Hole ]]
 *
 * -------------------------------------------
 */
enum Stmt extends Tree {

  case Scope(definitions: List[Definition], body: Stmt)

  // Fine-grain CBV
  case Return(expr: Pure)
  case Val(id: Id, binding: Stmt, body: Stmt)
  case App(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])

  // Local Control Flow
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Id, BlockLit)], default: Option[Stmt])

  // (Type-monomorphic?) Regions
  case Region(body: Block)
  case Alloc(id: Id, init: Pure, region: Id, body: Stmt)

  // creates a fresh state handler to model local (backtrackable) state.
  // [[capture]] is a binding occurence.
  // e.g. state(init) { [x]{x: Ref} => ... }
  case Var(id: Id, init: Expr, capture: Id, body: Stmt)
  case Get(id: Id, annotatedCapt: Captures, annotatedTpe: ValueType)
  case Put(id: Id, annotatedCapt: Captures, value: Pure)

  case Try(body: Block, handlers: List[Implementation])


  // Others
  case Hole()

  val tpe: ValueType = Type.inferType(this)
  val capt: Captures = Type.inferCapt(this)
}
export Stmt.*

/**
 * Smart constructors to establish some normal form
 */
object normal {

  def valDef(id: Id, binding: Stmt, body: Stmt): Stmt =
    (binding, body) match {

      // [[ val x = STMT; return x ]] == STMT
      case (_, Stmt.Return(Pure.ValueVar(other, _))) if other == id =>
        binding

      //  [[ val x = return EXPR; STMT ]] = [[ let x = EXPR; STMT ]]
      //
      // This opt is too good for JS: it blows the stack on
      // recursive functions that are used to encode while...
      //
      // The solution to this problem is implemented in core.MakeStackSafe:
      //   all recursive functions that could blow the stack are trivially wrapped
      //   again, after optimizing.
      case (Stmt.Return(expr), body) =>
        scope(List(Definition.Let(id, expr)), body)

      // here we are flattening scopes; be aware that this extends
      // life-times of bindings!
      //
      // { val x = { def...; BODY }; REST }  =  { def ...; val x = BODY }
      case (Stmt.Scope(definitions, binding), body) =>
        scope(definitions, valDef(id, binding, body))

      case _ => Stmt.Val(id, binding, body)
    }

  // { def f=...; { def g=...; BODY } }  =  { def f=...; def g; BODY }
  def scope(definitions: List[Definition], body: Stmt): Stmt = body match {
    case Stmt.Scope(others, body) => scope(definitions ++ others, body)
    case _ => if (definitions.isEmpty) body else Stmt.Scope(definitions, body)
  }

  // new { def f = BLOCK }.f  =  BLOCK
  def member(b: Block, field: Id, annotatedTpe: BlockType): Block = b match {
    case Block.New(impl) =>
      val Operation(name, tps, cps, vps, bps, resume, body) =
        impl.operations.find(op => op.name == field).getOrElse {
          INTERNAL_ERROR("Should not happen")
        }
      assert(resume.isEmpty, "We do not inline effectful capabilities at that point")
      BlockLit(tps, cps, vps, bps, body)
    case _ => Block.Member(b, field, annotatedTpe)
  }

  // TODO perform record selection here, if known
  def select(target: Pure, field: Id, annotatedType: ValueType): Pure =
    Select(target, field, annotatedType)

  def app(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]): Stmt =
    callee match {
      case b : Block.BlockLit => reduce(b, targs, vargs, bargs)
      case other => Stmt.App(callee, targs, vargs, bargs)
    }

  def make(tpe: ValueType.Data, tag: Id, vargs: List[Pure]): Pure =
    Pure.Make(tpe, tag, vargs)

  def pureApp(callee: Block, targs: List[ValueType], vargs: List[Pure]): Pure =
    callee match {
      case b : Block.BlockLit =>
        INTERNAL_ERROR(
          """|This should not happen!
             |User defined functions always have to be called with App, not PureApp.
             |If this error does occur, this means this changed.
             |Check `core.Transformer.makeFunctionCall` for details.
             |""".stripMargin)
      case other =>
        Pure.PureApp(callee, targs, vargs)
    }

  // "match" is a keyword in Scala
  def patternMatch(scrutinee: Pure, clauses: List[(Id, BlockLit)], default: Option[Stmt]): Stmt =
    scrutinee match {
      case Pure.Make(dataType, ctorTag, vargs) =>
        clauses.collectFirst { case (tag, lit) if tag == ctorTag => lit }
          .map(body => app(body, Nil, vargs, Nil))
          .orElse { default }.getOrElse { sys error "Pattern not exhaustive. This should not happen" }
      case other =>
        Match(scrutinee, clauses, default)
    }


  def directApp(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]): Expr =
    callee match {
      case b : Block.BlockLit => run(reduce(b, targs, vargs, Nil))
      case other => DirectApp(callee, targs, vargs, bargs)
    }

  def reduce(b: BlockLit, targs: List[core.ValueType], vargs: List[Pure], bargs: List[Block]): Stmt = {

    // Only bind if not already a variable!!!
    var ids: Set[Id] = Set.empty
    var bindings: List[Definition.Def] = Nil
    var bvars: List[Block.BlockVar] = Nil

    // (1) first bind
    bargs foreach {
      case x: Block.BlockVar => bvars = bvars :+ x
      // introduce a binding
      case block =>
        val id = symbols.TmpBlock()
        bindings = bindings :+ Definition.Def(id, block)
        bvars = bvars :+ Block.BlockVar(id, block.tpe, block.capt)
        ids += id
    }

    // (2) substitute
    val body = substitutions.substitute(b, targs, vargs, bvars)

    scope(bindings, body)
  }

  def run(s: Stmt): Expr = s match {
    case Stmt.Return(expr) => expr
    case _ => Run(s)
  }

  def box(b: Block, capt: Captures): Pure = b match {
    case Block.Unbox(pure) => pure
    case b => Box(b, capt)
  }

  def unbox(p: Pure): Block = p match {
    case Pure.Box(b, _) => b
    case p => Unbox(p)
  }
}

/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree {
  val tpe = interface
  val capt = operations.flatMap(_.capt).toSet
}

/**
 * Implementation of a method / effect operation.
 *
 * TODO generalize from BlockLit to also allow block definitions
 *
 * TODO For handler implementations we cannot simply reuse BlockLit here...
 *   maybe we need to add PlainOperation | ControlOperation, where for now
 *   handlers always have control operations and New always has plain operations.
 */
case class Operation(name: Id, tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], resume: Option[Param.BlockParam], body: Stmt) {
  val capt = body.capt -- cparams.toSet
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

    def pure(using Ctx): PartialFunction[Pure, Res] = PartialFunction.empty
    def expr(using Ctx): PartialFunction[Expr, Res] = PartialFunction.empty
    def stmt(using Ctx): PartialFunction[Stmt, Res] = PartialFunction.empty
    def block(using Ctx): PartialFunction[Block, Res] = PartialFunction.empty
    def defn(using Ctx): PartialFunction[Definition, Res] = PartialFunction.empty
    def impl(using Ctx): PartialFunction[Implementation, Res] = PartialFunction.empty
    def operation(using Ctx): PartialFunction[Operation, Res] = PartialFunction.empty
    def param(using Ctx): PartialFunction[Param, Res] = PartialFunction.empty
    def clause(using Ctx): PartialFunction[(Id, BlockLit), Res] = PartialFunction.empty
    def externBody(using Ctx): PartialFunction[ExternBody, Res] = PartialFunction.empty

    /**
     * Hook that can be overridden to perform an action at every node in the tree
     */
    def visit[T](t: T)(visitor: Ctx ?=> T => Res)(using Ctx): Res = visitor(t)

    inline def structuralQuery[T](el: T, pf: PartialFunction[T, Res])(using Ctx): Res = visit(el) { t =>
      if pf.isDefinedAt(el) then pf.apply(el) else queryStructurally(t, empty, combine)
    }

    def query(p: Pure)(using Ctx): Res = structuralQuery(p, pure)
    def query(e: Expr)(using Ctx): Res = structuralQuery(e, expr)
    def query(s: Stmt)(using Ctx): Res = structuralQuery(s, stmt)
    def query(b: Block)(using Ctx): Res = structuralQuery(b, block)
    def query(d: Definition)(using Ctx): Res = structuralQuery(d, defn)
    def query(d: Implementation)(using Ctx): Res = structuralQuery(d, impl)
    def query(d: Operation)(using Ctx): Res = structuralQuery(d, operation)
    def query(matchClause: (Id, BlockLit))(using Ctx): Res =
      if clause.isDefinedAt(matchClause) then clause.apply(matchClause) else matchClause match {
        case (id, lit) => query(lit)
    }
    def query(b: ExternBody)(using Ctx): Res = structuralQuery(b, externBody)
  }

  class Rewrite extends Structural {
    def id: PartialFunction[Id, Id] = PartialFunction.empty
    def pure: PartialFunction[Pure, Pure] = PartialFunction.empty
    def expr: PartialFunction[Expr, Expr] = PartialFunction.empty
    def stmt: PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def defn: PartialFunction[Definition, Definition] = PartialFunction.empty
    def block: PartialFunction[Block, Block] = PartialFunction.empty
    def handler: PartialFunction[Implementation, Implementation] = PartialFunction.empty
    def param: PartialFunction[Param, Param] = PartialFunction.empty

    def rewrite(x: Id): Id = if id.isDefinedAt(x) then id(x) else x
    def rewrite(p: Pure): Pure = rewriteStructurally(p, pure)
    def rewrite(e: Expr): Expr = rewriteStructurally(e, expr)
    def rewrite(s: Stmt): Stmt = rewriteStructurally(s, stmt)
    def rewrite(b: Block): Block = rewriteStructurally(b, block)
    def rewrite(d: Definition): Definition = rewriteStructurally(d, defn)
    def rewrite(e: Implementation): Implementation = rewriteStructurally(e, handler)
    def rewrite(o: Operation): Operation = rewriteStructurally(o)
    def rewrite(p: Param): Param = rewriteStructurally(p, param)
    def rewrite(p: Param.ValueParam): Param.ValueParam = rewrite(p: Param).asInstanceOf[Param.ValueParam]
    def rewrite(p: Param.BlockParam): Param.BlockParam = rewrite(p: Param).asInstanceOf[Param.BlockParam]
    def rewrite(b: ExternBody): ExternBody= rewrite(b)

    def rewrite(t: ValueType): ValueType = rewriteStructurally(t)
    def rewrite(t: ValueType.Data): ValueType.Data = rewriteStructurally(t)

    def rewrite(t: BlockType): BlockType = rewriteStructurally(t)
    def rewrite(t: BlockType.Interface): BlockType.Interface = rewriteStructurally(t)
    def rewrite(capt: Captures): Captures = capt.map(rewrite)

    def rewrite(m: ModuleDecl): ModuleDecl =
      m match {
        case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
          ModuleDecl(path, includes, declarations, externs, definitions.map(rewrite), exports)
      }

    def rewrite(matchClause: (Id, BlockLit)): (Id, BlockLit) = matchClause match {
      case (p, b) => (p, rewrite(b).asInstanceOf[BlockLit])
    }
  }
}

enum Variable {
  case Value(id: Id, tpe: core.ValueType)
  case Block(id: Id, tpe: core.BlockType, capt: core.Captures)

  def id: Id

  // lookup and comparison should still be done per-id, not structurally
  override def equals(other: Any): Boolean = other match {
    case other: Variable => this.id == other.id
    case _ => false
  }
  override def hashCode(): Int = id.hashCode
}

type Variables = Set[Variable]

object Variables {

  import core.Type.{TState, TRegion}

  def value(id: Id, tpe: ValueType) = Set(Variable.Value(id, tpe))
  def block(id: Id, tpe: BlockType, capt: Captures) = Set(Variable.Block(id, tpe, capt))
  def empty: Variables = Set.empty

  def free(e: Expr): Variables = e match {
    case DirectApp(b, targs, vargs, bargs) => free(b) ++ all(vargs, free) ++ all(bargs, free)
    case Run(s) => free(s)
    case Pure.ValueVar(id, annotatedType) => Variables.value(id, annotatedType)
    case Pure.Literal(value, annotatedType) => Variables.empty
    case Pure.PureApp(b, targs, vargs) => free(b) ++ all(vargs, free)
    case Pure.Make(data, tag, vargs) => all(vargs, free)
    case Pure.Select(target, field, annotatedType) => free(target)
    case Pure.Box(b, annotatedCapture) => free(b)
  }

  def free(b: Block): Variables = b match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) => Variables.block(id, annotatedTpe, annotatedCapt)
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      free(body) -- all(vparams, bound) -- all(bparams, bound)

    case Block.Member(block, field, annotatedTpe) => free(block)
    case Block.Unbox(pure) => free(pure)
    case Block.New(impl) => free(impl)
  }

  def free(d: Definition): Variables = d match {
    case Definition.Def(id, block) => free(block)
    case Definition.Let(id, binding) => free(binding)
  }

  def all[T](t: IterableOnce[T], f: T => Variables): Variables =
    t.iterator.foldLeft(Variables.empty) { case (xs, t) => f(t) ++ xs }

  def free(impl: Implementation): Variables = all(impl.operations, free)

  def free(op: Operation): Variables = op match {
    case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      free(body) -- all(vparams, bound) -- all(bparams, bound) -- all(resume, bound)
  }
  def free(s: Stmt): Variables = s match {
    // currently local functions cannot be mutually recursive
    case Stmt.Scope(defs, body) =>
      var stillFree = Variables.empty
      var boundSoFar = Variables.empty
      defs.foreach { d =>
        stillFree = stillFree ++ (free(d) -- boundSoFar)
        boundSoFar = boundSoFar ++ bound(d)
      }
      stillFree ++ (free(body) -- boundSoFar)

    case Stmt.Return(expr) => free(expr)
    case Stmt.Val(id, binding, body) => free(binding) ++ (free(body) -- Variables.value(id, binding.tpe))
    case Stmt.App(callee, targs, vargs, bargs) => free(callee) ++ all(vargs, free) ++ all(bargs, free)
    case Stmt.If(cond, thn, els) => free(cond) ++ free(thn) ++ free(els)
    case Stmt.Match(scrutinee, clauses, default) => free(scrutinee) ++ all(default, free) ++ all(clauses, {
      case (id, lit) => free(lit)
    })
    case Stmt.Region(body) => free(body)
    // are mutable variables now block variables or not?
    case Stmt.Alloc(id, init, region, body) => free(init) ++ Variables.block(region, TRegion, Set(region)) ++ (free(body) -- Variables.block(id, TState(init.tpe), Set(region)))
    case Stmt.Var(id, init, capture, body) => free(init) ++ (free(body) -- Variables.block(id, TState(init.tpe), Set(capture)))
    case Stmt.Get(id, annotatedCapt, annotatedTpe) => Variables.block(id, core.Type.TState(annotatedTpe), annotatedCapt)
    case Stmt.Put(id, annotatedCapt, value) => Variables.block(id, core.Type.TState(value.tpe), annotatedCapt)

    case Stmt.Try(body, handlers) => free(body) ++ all(handlers, free)
    case Stmt.Hole() => Variables.empty
  }

  def bound(t: ValueParam): Variables = Variables.value(t.id, t.tpe)
  def bound(t: BlockParam): Variables = Variables.block(t.id, t.tpe, t.capt)

  def bound(d: Definition): Variables = d match {
    case Definition.Def(id, block) => Variables.block(id, block.tpe, block.capt)
    case Definition.Let(id, binding) => Variables.value(id, binding.tpe)
  }
}


object substitutions {

  case class Substitution(
    vtypes: Map[Id, ValueType],
    captures: Map[Id, Captures],
    values: Map[Id, Pure],
    blocks: Map[Id, Block]
  ) {
    def shadowTypes(shadowed: IterableOnce[Id]): Substitution = copy(vtypes = vtypes -- shadowed)
    def shadowCaptures(shadowed: IterableOnce[Id]): Substitution = copy(captures = captures -- shadowed)
    def shadowValues(shadowed: IterableOnce[Id]): Substitution = copy(values = values -- shadowed)
    def shadowBlocks(shadowed: IterableOnce[Id]): Substitution = copy(blocks = blocks -- shadowed)

    def shadowDefinitions(shadowed: Seq[Definition]): Substitution = copy(
      values = values -- shadowed.collect { case d: Definition.Let => d.id },
      blocks = blocks -- shadowed.collect { case d: Definition.Def => d.id }
    )

    def shadowParams(shadowed: Seq[Param]): Substitution = copy(
      values = values -- shadowed.collect { case d: Param.ValueParam => d.id },
      blocks = blocks -- shadowed.collect { case d: Param.BlockParam => d.id }
    )
  }

  // Starting point for inlining, creates Maps(params -> args) and passes to normal substitute
  def substitute(block: BlockLit, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]): Stmt =
    block match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        val tSubst = (tparams zip targs).toMap
        val cSubst = (cparams zip bargs.map(_.capt)).toMap
        val vSubst = (vparams.map(_.id) zip vargs).toMap
        val bSubst = (bparams.map(_.id) zip bargs).toMap

        substitute(body)(using Substitution(tSubst, cSubst, vSubst, bSubst))
    }

  //Replaces all variables contained in one of the Maps with their value
  def substitute(definition: Definition)(using Substitution): Definition =
    definition match {
      case Definition.Def(id, block) => Definition.Def(id, substitute(block))
      case Definition.Let(id, binding) => Definition.Let(id, substitute(binding))
    }

  def substitute(expression: Expr)(using Substitution): Expr =
    expression match {
      case DirectApp(b, targs, vargs, bargs) =>
        DirectApp(substitute(b), targs.map(substitute), vargs.map(substitute), bargs.map(substitute))

      case Run(s) =>
        Run(substitute(s))

      case p: Pure =>
        substitute(p)
    }

  def substitute(statement: Stmt)(using subst: Substitution): Stmt =
    statement match {
      case Scope(definitions, body) =>
        Scope(definitions.map(substitute),
          substitute(body)(using subst shadowDefinitions definitions))

      case Return(expr) =>
        Return(substitute(expr))

      case Val(id, binding, body) =>
        Val(id, substitute(binding),
          substitute(body)(using subst shadowValues List(id)))

      case App(callee, targs, vargs, bargs) =>
        App(substitute(callee), targs.map(substitute), vargs.map(substitute), bargs.map(substitute))

      case If(cond, thn, els) =>
        If(substitute(cond), substitute(thn), substitute(els))

      case Match(scrutinee, clauses, default) =>
        Match(substitute(scrutinee), clauses.map {
          case (id, b) => (id, substitute(b).asInstanceOf[BlockLit])
        }, default.map(substitute))

      case Alloc(id, init, region, body) =>
        Alloc(id, substitute(init), substituteAsVar(region),
          substitute(body)(using subst shadowBlocks List(id)))

      case Var(id, init, capture, body) =>
        Var(id, substitute(init), capture, substitute(body)(using subst shadowBlocks List(id)))

      case Get(id, capt, tpe) =>
        Get(substituteAsVar(id), substitute(capt), substitute(tpe))

      case Put(id, capt, value) =>
        Put(substituteAsVar(id), substitute(capt), substitute(value))

      case Try(body, handlers) =>
        Try(substitute(body), handlers.map(substitute))

      case Region(body) =>
        Region(substitute(body))

      case h : Hole => h
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

      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        val shadowedTypelevel = subst shadowTypes tparams shadowCaptures cparams
        BlockLit(tparams, cparams,
          vparams.map(p => substitute(p)(using shadowedTypelevel)),
          bparams.map(p => substitute(p)(using shadowedTypelevel)),
          substitute(body)(using shadowedTypelevel shadowParams (vparams ++ bparams)))

      case Member(block, field, annotatedTpe) =>
        Member(substitute(block), field, substitute(annotatedTpe))

      case Unbox(pure) =>
        Unbox(substitute(pure))

      case New(impl) =>
        New(substitute(impl))
    }

  def substitute(pure: Pure)(using subst: Substitution): Pure =
    pure match {
      case ValueVar(id, _) if subst.values.isDefinedAt(id) => subst.values(id)
      case ValueVar(id, annotatedType) => ValueVar(id, substitute(annotatedType))

      case Literal(value, annotatedType) =>
        Literal(value, substitute(annotatedType))

      case Make(tpe, tag, vargs) =>
        Make(substitute(tpe).asInstanceOf, tag, vargs.map(substitute))

      case PureApp(b, targs, vargs) =>
        PureApp(substitute(b), targs.map(substitute), vargs.map(substitute))

      case Select(target, field, annotatedType) =>
        Select(substitute(target), field, substitute(annotatedType))

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
      case Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
        val shadowedTypelevel = subst shadowTypes tparams shadowCaptures cparams
        Operation(name, tparams, cparams,
          vparams.map(p => substitute(p)(using shadowedTypelevel)),
          bparams.map(p => substitute(p)(using shadowedTypelevel)),
          resume.map(p => substitute(p)(using shadowedTypelevel)),
          substitute(body)(using shadowedTypelevel shadowParams (vparams ++ bparams)))
    }

  def substitute(param: Param.ValueParam)(using Substitution): Param.ValueParam =
    param match {
      case Param.ValueParam(id, tpe) => Param.ValueParam(id, substitute(tpe))
    }

  def substitute(param: Param.BlockParam)(using Substitution): Param.BlockParam =
    param match {
      case Param.BlockParam(id, tpe, capt) => Param.BlockParam(id, substitute(tpe), substitute(capt))
    }

  def substitute(tpe: ValueType)(using subst: Substitution): ValueType =
    Type.substitute(tpe, subst.vtypes, subst.captures)

  def substitute(tpe: BlockType)(using subst: Substitution): BlockType =
    Type.substitute(tpe, subst.vtypes, subst.captures)

  def substitute(capt: Captures)(using subst: Substitution): Captures =
    Type.substitute(capt, subst.captures)
}
