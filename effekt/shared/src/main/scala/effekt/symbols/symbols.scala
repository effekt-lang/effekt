package effekt
package symbols

import effekt.source.{ DefDef, Def, FunDef, ModuleDecl, ValDef, VarDef }
import effekt.context.Context
import kiama.util.Source
import effekt.context.assertions.*
import effekt.util.messages.ErrorReporter

/**
 * The symbol table contains things that can be pointed to:
 * - function definitions
 * - type definitions
 * - effect definitions
 * - parameters
 * - value / variable binders
 * - ...
 */


sealed trait TermSymbol extends Symbol

// the two universes of values and blocks
sealed trait ValueSymbol extends TermSymbol
sealed trait BlockSymbol extends TermSymbol

/**
 * The result of running the frontend on a module.
 * Symbols and types are stored globally in CompilerContext.
 */
case class Module(
  decl: ModuleDecl,
  source: Source
) extends Symbol {

  val name = {
    val segments = decl.path.split("/")
    QualifiedName(segments.tail.toList, segments.head)
  }

  def path = decl.path

  private var _terms: Map[String, Set[TermSymbol]] = _
  def terms = _terms

  private var _types: Map[String, TypeSymbol] = _
  def types = _types

  private var _imports: List[Module] = _
  def imports = _imports

  // a topological ordering of all transitive dependencies
  // this is the order in which the modules need to be compiled / loaded
  lazy val dependencies: List[Module] = imports.flatMap { im => im.dependencies :+ im }.distinct

  // toplevel declared effects
  def effects: List[Interface] = types.values.toList.collect {
    case e: Interface => e
  }

  def findPrelude: Module = {
    dependencies.find(_.name.name == "effekt").get
  }

  /**
   * It is actually possible, that exports is invoked on a single module multiple times:
   * The dependencies of a module might change, which triggers frontend on the same module
   * again. It is the same, since the source and AST did not change.
   */
  def exports(
    imports: List[Module],
    terms: Map[String, Set[TermSymbol]],
    types: Map[String, TypeSymbol]
  ): this.type = {
    _imports = imports
    _terms = terms
    _types = types
    this
  }
}

sealed trait Param extends TermSymbol
case class ValueParam(name: Name, tpe: Option[ValueType]) extends Param, ValueSymbol

enum TrackedParam(val name: Name) extends Param, BlockSymbol {

  case BlockParam(n: Name, tpe: BlockType) extends TrackedParam(n)
  case ResumeParam(module: Module) extends TrackedParam(Name.local("resume"))
  case SelfParam(tree: source.Tree) extends TrackedParam(Name.local("this"))
  case ExternResource(n: Name, tpe: BlockType) extends TrackedParam(n)

  // Every tracked block gives rise to a capture parameter (except resumptions, they are transparent)
  lazy val capture: Capture = this match {
    case b: BlockParam => CaptureParam(b.name)
    case r: ResumeParam => ???
    case s: SelfParam => LexicalRegion(name, s.tree)
    case r: ExternResource => Resource(name)
  }
}
export TrackedParam.*


trait Callable extends BlockSymbol {
  def tparams: List[TypeParam]
  def vparams: List[ValueParam]
  def bparams: List[BlockParam]
  def annotatedResult: Option[List[ValueType]]
  def annotatedEffects: Option[Effects]
}

case class UserFunction(
  name: Name,
  tparams: List[TypeParam],
  vparams: List[ValueParam],
  bparams: List[BlockParam],
  annotatedResult: Option[List[ValueType]],
  annotatedEffects: Option[Effects],
  decl: FunDef
) extends Callable

/**
 * Anonymous symbols used to represent scopes / regions in the region checker
 */
sealed trait Anon extends TermSymbol {
  val name = NoName
  def decl: source.Tree
}

case class Lambda(vparams: List[ValueParam], bparams: List[BlockParam], decl: source.Tree) extends Callable, Anon {
  // Lambdas currently do not have an annotated return type
  def annotatedResult = None
  def annotatedEffects = None

  // Lambdas currently do not take type parameters
  def tparams = Nil
}

/**
 * Binders represent local value and variable binders
 *
 * They also store a reference to the original defition in the source code
 */
enum Binder extends TermSymbol {
  def tpe: Option[Type]
  def decl: Def

  case ValBinder(name: Name, tpe: Option[ValueType], decl: ValDef) extends Binder, ValueSymbol
  case VarBinder(name: Name, tpe: Option[ValueType], region: BlockSymbol, decl: VarDef) extends Binder, BlockSymbol
  case DefBinder(name: Name, tpe: Option[BlockType], decl: DefDef) extends Binder, BlockSymbol
}
export Binder.*


/**
 * Synthetic symbol representing potentially multiple call targets
 *
 * Refined by typer.
 */
case class CallTarget(name: Name, symbols: List[Set[BlockSymbol]]) extends BlockSymbol

/**
 * Introduced by Transformer
 */
case class Wildcard() extends ValueSymbol { val name = Name.local("_") }
case class TmpValue() extends ValueSymbol { val name = Name.local("tmp" + Symbol.fresh.next()) }
case class TmpBlock() extends BlockSymbol { val name = Name.local("tmp" + Symbol.fresh.next()) }

/**
 * Type Symbols
 * - [[ValueTypeSymbol]]
 * - [[BlockTypeSymbol]]
 * - [[Capture]]
 */
sealed trait TypeSymbol extends Symbol
sealed trait ValueTypeSymbol extends TypeSymbol
sealed trait BlockTypeSymbol extends TypeSymbol


/**
 * Value Type Symbols
 */

/**
 * Type variables are symbols that can be substituted for.
 * - [[TypeParam]] type variables in user programs
 * - [[UnificationVar]] type variables inserted by the type checker.
 */
enum TypeVar(val name: Name) extends ValueTypeSymbol {

  /**
   * Type parameters that show up in user programs
   */
  case TypeParam(n: Name) extends TypeVar(n)

  /**
   * Introduced when instantiating type schemes
   *
   * Should neither occur in source programs, nor in inferred types
   */
  case UnificationVar(underlying: TypeVar.TypeParam, call: source.Tree) extends TypeVar(underlying.name)
}
export TypeVar.*

case class TypeAlias(name: Name, tparams: List[TypeParam], tpe: ValueType) extends ValueTypeSymbol

/**
 * Types that _can_ be used in type constructor position. e.g. >>>List<<<[T]
 *
 * - [[DataType]]
 * - [[Record]]
 * - [[ExternType]]
 */
enum TypeConstructor extends TypeSymbol {
  def tparams: List[TypeParam]

  case DataType(name: Name, tparams: List[TypeParam], var constructors: List[Constructor] = Nil)
  case Record(name: Name, tparams: List[TypeParam], var constructor: Constructor)
  case ExternType(name: Name, tparams: List[TypeParam])
}
export TypeConstructor.*


case class Constructor(name: Name, tparams: List[TypeParam], var fields: List[Field], tpe: TypeConstructor) extends Callable {
  // Parameters and return type of the constructor
  lazy val vparams: List[ValueParam] = fields.map { f => f.param }
  val bparams: List[BlockParam] = Nil

  val returnType: ValueType = ValueTypeApp(tpe, tparams map ValueTypeRef.apply)
  def annotatedResult: Option[List[ValueType]] = Some(List(returnType))
  def annotatedEffects: Option[Effects] = Some(Effects.Pure)
}

// TODO maybe split into Field (the symbol) and Selector (the synthetic function)
case class Field(name: Name, param: ValueParam, constructor: Constructor) extends Callable {
  val tparams: List[TypeParam] = constructor.tparams
  val vparams = List(ValueParam(constructor.name, Some(constructor.returnType)))
  val bparams = List.empty[BlockParam]

  val returnType = param.tpe.get
  def annotatedResult = Some(List(returnType))
  def annotatedEffects = Some(Effects.Pure)
}


enum BlockTypeConstructor extends BlockTypeSymbol {
  def tparams: List[TypeParam]

  case Interface(name: Name, tparams: List[TypeParam], var operations: List[Operation] = Nil)
  case ExternInterface(name: Name, tparams: List[TypeParam])
}
export BlockTypeConstructor.*


case class Operation(name: Name, tparams: List[TypeParam], vparams: List[ValueParam], resultType: List[ValueType], effects: Effects, interface: BlockTypeConstructor.Interface) extends Callable {
  val bparams = List.empty[BlockParam]

  def annotatedResult: Option[List[ValueType]] = Some(resultType)
  def annotatedEffects: Option[Effects] = Some(Effects(effects.toList))
  def appliedInterface: InterfaceType = InterfaceType(interface, interface.tparams map ValueTypeRef.apply)
}

/**
 * Effect aliases are *not* block types, or block type constructors. They have to be dealiased by [[Namer]]
 * before usage.
 */
case class EffectAlias(name: Name, tparams: List[TypeParam], effs: Effects) extends BlockTypeSymbol


/**
 * Something that can be substituted by a capture set
 */
sealed trait CaptVar extends TypeSymbol

/**
 * "Tracked" capture parameters. Like [[TypeParam]] used to abstract
 * over capture. Also see [[BlockParam.capture]].
 *
 * Can be either
 * - [[LexicalRegion]] to model self regions of functions
 */
enum Capture extends CaptVar {

  /**
   * Capture parameters introduced by block parameters (they count as `control`, since they can close over arbitrary capabilities)
   */
  case CaptureParam(name: Name)

  /**
   * Self region of functions and handlers (they count in as `io` when considering direct style)
   */
  case LexicalRegion(name: Name, tree: source.Tree)

  /**
   * Represents external resources (they count in as `io` when considering direct style)
   */
  case Resource(name: Name)
}
export Capture.*

case class CaptUnificationVar(role: CaptUnificationVar.Role) extends Captures, CaptVar {
  val name = Name.local("?C")
  override def toString = role match {
    case CaptUnificationVar.VariableInstantiation(underlying, _) => "?" + underlying.toString + id
    case CaptUnificationVar.Subtraction(handled, underlying) => s"?filter" + id
    case CaptUnificationVar.FunctionRegion(fun) => s"?${fun.id.name}" + id
    case CaptUnificationVar.AnonymousFunctionRegion(fun) => s"?anon" + id
    case CaptUnificationVar.HandlerRegion(handler) => s"?Ck" + id
    case _ => "?" + id
  }
}
object CaptUnificationVar {
  sealed trait Role
  case class VariableInstantiation(underlying: Capture, call: source.Tree) extends Role
  case class HandlerRegion(handler: source.TryHandle) extends Role
  case class RegionRegion(handler: source.Region) extends Role
  case class FunctionRegion(fun: source.FunDef) extends Role
  case class BlockRegion(fun: source.DefDef) extends Role
  case class AnonymousFunctionRegion(fun: source.BlockLiteral) extends Role
  case class InferredBox(box: source.Box) extends Role
  case class InferredUnbox(unbox: source.Unbox) extends Role
  // underlying should be a UnificationVar
  case class Subtraction(handled: List[Capture], underlying: CaptUnificationVar) extends Role
  case class Substitution() extends Role
}

/**
 * Capture Sets
 */

sealed trait Captures

case class CaptureSet(captures: Set[Capture]) extends Captures {
  // This is a very simple form of subtraction, make sure that all constraints have been solved before using it!
  def --(other: CaptureSet): CaptureSet = CaptureSet(captures -- other.captures)
  def ++(other: CaptureSet): CaptureSet = CaptureSet(captures ++ other.captures)
  def +(c: Capture): CaptureSet = CaptureSet(captures + c)
  def flatMap(f: Capture => CaptureSet): CaptureSet = CaptureSet(captures.flatMap(x => f(x).captures))
}
object CaptureSet {
  def apply(captures: Capture*): CaptureSet = CaptureSet(captures.toSet)
  def apply(captures: List[Capture]): CaptureSet = CaptureSet(captures.toSet)
  def empty = CaptureSet()
}

/**
 * FFI
 */
case class ExternFunction(
  name: Name,
  tparams: List[TypeParam],
  vparams: List[ValueParam],
  bparams: List[BlockParam],
  result: List[ValueType],
  effects: Effects,
  capture: CaptureSet,
  body: String = ""
) extends Callable {
  def annotatedResult = Some(result)
  def annotatedEffects = Some(effects)
}

/**
 * Extension method for LSP to filter out synthetically generated symbols
 */
extension (s: Symbol) {
  def isSynthetic: Boolean = s match {
    case _: Field | _: Constructor | _: CallTarget | _: Wildcard | _: TmpValue | _: ResumeParam => true
    case s => s.synthetic
  }
}
