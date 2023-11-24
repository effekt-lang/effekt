package effekt
package core

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
sealed trait Tree

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
  imports: List[String],
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
  case Def(id: Id, tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], ret: ValueType, annotatedCapture: Captures, body: String)
  case Include(contents: String)
}


enum Definition {
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
 *     │─ [[ Select ]]
 *     │─ [[ Box ]]
 *
 * -------------------------------------------
 */
enum Pure extends Expr {
  case ValueVar(id: Id, annotatedType: ValueType)

  case Literal(value: Any, annotatedType: ValueType)

  // invariant, block b is pure.
  case PureApp(b: Block, targs: List[ValueType], vargs: List[Pure])
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
  case BlockParam(id: Id, tpe: BlockType)
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

  trait Query extends Structural {
    type M

    def empty: M
    def combine(r1: M, r2: M): M

    def query(p: Pure): M = queryStructurally(p, empty, combine)
    def query(e: Expr): M = queryStructurally(e, empty, combine)
    def query(s: Stmt): M = queryStructurally(s, empty, combine)
    def query(b: Block): M = queryStructurally(b, empty, combine)
    def query(d: Definition): M = queryStructurally(d, empty, combine)
    def query(d: Implementation): M = queryStructurally(d, empty, combine)
    def query(d: Operation): M = queryStructurally(d, empty, combine)
    def query(matchClause: (Id, BlockLit)): M = matchClause match {
      case (id, lit) => query(lit)
    }
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

    def rewrite(t: ValueType): ValueType = rewriteStructurally(t)
    def rewrite(t: BlockType): BlockType = rewriteStructurally(t)
    def rewrite(t: BlockType.Interface): BlockType.Interface = rewriteStructurally(t)
    def rewrite(capt: Captures): Captures = capt.map(rewrite)

    def rewrite(m: ModuleDecl): ModuleDecl =
      m match {
        case ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
          ModuleDecl(path, imports, declarations, externs, definitions.map(rewrite), exports)
      }

    def rewrite(matchClause: (Id, BlockLit)): (Id, BlockLit) = matchClause match {
      case (p, b) => (p, rewrite(b).asInstanceOf[BlockLit])
    }
  }
}

object freeVariables {

  case class Variables(values: Set[Id], blocks: Set[Id]) {
    def ++(other: Variables) = Variables(values ++ other.values, blocks ++ other.blocks)
    def --(other: Variables) = Variables(values -- other.values, blocks -- other.blocks)
  }
  object Variables {
    def value(id: Id) = Variables(Set(id), Set.empty)
    def block(id: Id) = Variables(Set.empty, Set(id))
    def empty = Variables(Set.empty, Set.empty)
  }

  def free(e: Expr): Variables = e match {
    case DirectApp(b, targs, vargs, bargs) => free(b) ++ all(vargs, free) ++ all(bargs, free)
    case Run(s) => free(s)
    case Pure.ValueVar(id, annotatedType) => Variables.value(id)
    case Pure.Literal(value, annotatedType) => Variables.empty
    case Pure.PureApp(b, targs, vargs) => free(b) ++ all(vargs, free)
    case Pure.Select(target, field, annotatedType) => free(target)
    case Pure.Box(b, annotatedCapture) => free(b)
  }

  def free(b: Block): Variables = b match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) => Variables.block(id)
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
    case Stmt.Val(id, binding, body) => free(binding) ++ (free(body) -- Variables.value(id))
    case Stmt.App(callee, targs, vargs, bargs) => free(callee) ++ all(vargs, free) ++ all(bargs, free)
    case Stmt.If(cond, thn, els) => free(cond) ++ free(thn) ++ free(els)
    case Stmt.Match(scrutinee, clauses, default) => free(scrutinee) ++ all(default, free) ++ all(clauses, {
      case (id, lit) => free(lit)
    })
    case Stmt.Region(body) => free(body)
    // are mutable variables now block variables or not?
    case Stmt.Alloc(id, init, region, body) => free(init) ++ Variables.block(region) ++ (free(body) -- Variables.block(id))
    case Stmt.Var(id, init, capture, body) => free(init) ++ (free(body) -- Variables.block(id))
    case Stmt.Get(id, annotatedCapt, annotatedTpe) => Variables.block(id)
    case Stmt.Put(id, annotatedCapt, value) => Variables.block(id)

    case Stmt.Try(body, handlers) => free(body) ++ all(handlers, free)
    case Stmt.Hole() => Variables.empty
  }

  def bound(t: ValueParam): Variables = Variables.value(t.id)
  def bound(t: BlockParam): Variables = Variables.block(t.id)

  def bound(d: Definition): Variables = d match {
    case Definition.Def(id, block) => Variables.block(id)
    case Definition.Let(id, binding) => Variables.value(id)
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
      case Param.BlockParam(id, tpe) => Param.BlockParam(id, substitute(tpe))
    }

  def substitute(tpe: ValueType)(using subst: Substitution): ValueType =
    Type.substitute(tpe, subst.vtypes, subst.captures)

  def substitute(tpe: BlockType)(using subst: Substitution): BlockType =
    Type.substitute(tpe, subst.vtypes, subst.captures)

  def substitute(capt: Captures)(using subst: Substitution): Captures =
    Type.substitute(capt, subst.captures)
}
