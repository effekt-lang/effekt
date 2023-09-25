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
 *     │  │─ [[ State ]]
 *     │  │─ [[ Try ]]
 *     │  │─ [[ Region ]]
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
enum Extern extends Tree { // TODO MRV 26
  case Def(id: Id, tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], ret: List[ValueType], annotatedCapture: Captures, body: String)
  case Include(contents: String)
}


enum Definition {

  case Def(id: Id, block: Block)
  case Let(ids: List[Id], binding: Expr) // PURE on the toplevel?

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

def Let(ids: List[Id], binding: Expr, rest: Stmt) =
  addToScope(Definition.Let(ids,  binding), rest)


/**
 * Expressions (with potential IO effects)
 *
 * - [[DirectApp]]
 * - [[Run]]
 * - [[Pure]]
 */
sealed trait Expr extends Tree {
  val tpe: List[ValueType] = Type.inferType(this)
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
 *     │─ [[ State ]]
 *     │─ [[ Try ]]
 *     │─ [[ Region ]]
 *     │─ [[ Hole ]]
 *
 * -------------------------------------------
 */
enum Stmt extends Tree {

  case Scope(definitions: List[Definition], body: Stmt)

  // Fine-grain CBV
  case Return(expr: List[Pure])
  case Val(ids: List[Id], binding: Stmt, body: Stmt) // TODO MRV: val x, y = swap(x, y)
  case App(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])

  // Local Control Flow
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Id, Block.BlockLit)], default: Option[Stmt])  // TODO

  // Effects
  case State(id: Id, init: Pure, region: Id, body: Stmt) // TODO maybe rename to Var?
  case Try(body: Block, handlers: List[Implementation])
  case Region(body: Block)

  // Others
  case Hole()

  val tpe: List[ValueType] = Type.inferType(this)
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

  class Query extends Structural {
    def empty: Set[Id] = Set.empty
    def combine(r1: Set[Id], r2: Set[Id]): Set[Id] = r1 ++ r2

    def query(p: Pure): Set[Id] = queryStructurally(p, empty, combine)
    def query(e: Expr): Set[Id] = queryStructurally(e, empty, combine)
    def query(s: Stmt): Set[Id] = queryStructurally(s, empty, combine)
    def query(b: Block): Set[Id] = queryStructurally(b, empty, combine)
    def query(d: Definition): Set[Id] = queryStructurally(d, empty, combine)
    def query(d: Implementation): Set[Id] = queryStructurally(d, empty, combine)
    def query(d: Operation): Set[Id] = queryStructurally(d, empty, combine)
    def query(matchClause: (Id, BlockLit)): Set[Id] = matchClause match {
      case (id, lit) => query(lit)
    }
  }

  class Rewrite extends Structural {
    def id: PartialFunction[Id, Id] = PartialFunction.empty
    def pure: PartialFunction[Pure, Pure] = PartialFunction.empty
    def expr: PartialFunction[Expr, Expr] = PartialFunction.empty
    def exprList: PartialFunction[List[Expr], List[Expr]] = PartialFunction.empty
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
      values = values -- shadowed.collect { case d: Definition.Let => extractIds(d) }.flatten,
      blocks = blocks -- shadowed.collect { case d: Definition.Def => extractIds(d) }.flatten
    )

    def extractIds(definition: Definition): List[Id] = definition match {
      case Definition.Def(id, _) => List(id)
      case Definition.Let(ids, _) => ids
    }

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

      case Return(exprs) =>
        Return(exprs.map(substitute))

      case Val(ids, binding, body) =>
        Val(ids, substitute(binding),
          substitute(body)(using subst shadowValues ids))

      case App(callee, targs, vargs, bargs) =>
        App(substitute(callee), targs.map(substitute), vargs.map(substitute), bargs.map(substitute))

      case If(cond, thn, els) =>
        If(substitute(cond), substitute(thn), substitute(els))

      case Match(scrutinee, clauses, default) =>
        Match(substitute(scrutinee), clauses.map {
          case (id, b) => (id, substitute(b).asInstanceOf[BlockLit])
        }, default.map(substitute))

      case State(id, init, region, body) =>
        val newRegion = subst.blocks.get(region) map {
          case BlockVar(x, _, _) => x
          case _ => INTERNAL_ERROR("Regions should always be variables")
        } getOrElse region

        State(id, substitute(init), newRegion,
          substitute(body)(using subst shadowBlocks List(id)))

      case Try(body, handlers) =>
        Try(substitute(body), handlers.map(substitute))

      case Region(body) =>
        Region(substitute(body))

      case h : Hole => h
    }

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
