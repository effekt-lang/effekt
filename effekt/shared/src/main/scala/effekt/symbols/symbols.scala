package effekt
package symbols

import effekt.source.{ DefDef, Def, ExternFlag, FunDef, ModuleDecl, ValDef, VarDef }
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
trait ValueSymbol extends TermSymbol
trait BlockSymbol extends TermSymbol

sealed trait Synthetic extends Symbol {
  override def synthetic = true
}

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
  def effects: Effects = Effects(types.values.collect {
    case e: InterfaceType => e
  })

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
case class ValueParam(name: Name, tpe: Option[ValueType]) extends Param with ValueSymbol

// TODO everywhere else the two universes are called "value" and "block"

sealed trait TrackedParam extends Param with BlockSymbol {
  // every block parameter gives rise to a capture parameter
  lazy val capture: Capture = CaptureParameter(name)
}
case class BlockParam(name: Name, tpe: BlockType) extends TrackedParam
//  case class CapabilityParam(name: Name, tpe: CapabilityType) extends TrackedParam with Capability {
//    def effect = tpe.eff
//    override def toString = s"@${tpe.eff.name}"
//  }

// to be fair, resume is not tracked anymore, but transparent.
case class ResumeParam(module: Module) extends TrackedParam { val name = Name.local("resume") }

/**
 * Term-level representation of the current region.
 */
case class SelfParam(tree: source.Tree) extends TrackedParam {
  val name = Name.local("this")
  def tpe = builtins.TRegion
  override lazy val capture: Capture = LexicalRegion(name, tree)
}

// TODO rename to Callable
trait Fun extends BlockSymbol {
  def tparams: List[TypeVar]
  def vparams: List[ValueParam]
  def bparams: List[BlockParam]
  def annotatedResult: Option[ValueType]
  def annotatedEffects: Option[Effects]
}

case class UserFunction(
  name: Name,
  tparams: List[TypeVar],
  vparams: List[ValueParam],
  bparams: List[BlockParam],
  annotatedResult: Option[ValueType],
  annotatedEffects: Option[Effects],
  decl: FunDef
) extends Fun

/**
 * Anonymous symbols used to represent scopes / regions in the region checker
 */
sealed trait Anon extends TermSymbol {
  val name = NoName
  def decl: source.Tree
}

case class Lambda(vparams: List[ValueParam], bparams: List[BlockParam], decl: source.Tree) extends Fun with Anon {
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
sealed trait Binder extends TermSymbol {
  def tpe: Option[Type]
  def decl: Def
}
case class ValBinder(name: Name, tpe: Option[ValueType], decl: ValDef) extends Binder with ValueSymbol
case class VarBinder(name: Name, tpe: Option[ValueType], region: BlockSymbol, decl: VarDef) extends Binder with BlockSymbol
case class DefBinder(name: Name, tpe: Option[BlockType], decl: DefDef) extends Binder with BlockSymbol

/**
 * Synthetic symbol representing potentially multiple call targets
 *
 * Refined by typer.
 */
case class CallTarget(name: Name, symbols: List[Set[BlockSymbol]]) extends Synthetic with BlockSymbol

/**
 * Introduced by Transformer
 */
case class Wildcard(module: Module) extends ValueSymbol { val name = Name.local("_") }
case class Tmp(module: Module) extends ValueSymbol { val name = Name.local("tmp" + Symbol.fresh.next()) }



// reflecting the two namespaces
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
sealed trait TypeVar extends ValueTypeSymbol

/**
 * Type parameters that show up in user programs
 */
case class TypeParam(name: Name) extends TypeVar

/**
 * Introduced when instantiating type schemes
 *
 * Should neither occur in source programs, nor in inferred types
 */
case class UnificationVar(role: UnificationVar.Role) extends TypeVar {
  val name = Name.local("?")

  override def toString = role match {
    case UnificationVar.TypeVariableInstantiation(underlying, _) => "?" + underlying.toString + id
    case _ => "?" + id
  }
}
object UnificationVar {
  sealed trait Role
  case class TypeVariableInstantiation(underlying: TypeVar, call: source.Tree) extends Role
}

case class TypeAlias(name: Name, tparams: List[TypeVar], tpe: ValueType) extends ValueTypeSymbol

/**
 * Types that _can_ be used in type constructor position. e.g. >>>List<<<[T]
 *
 * - [[DataType]]
 * - [[Record]]
 * - [[BuiltinType]]
 */
sealed trait TypeConstructor extends TypeSymbol

case class DataType(name: Name, tparams: List[TypeVar], var variants: List[Record] = Nil) extends TypeConstructor

  /**
 * Structures are also function symbols to represent the constructor
 */
case class Record(name: Name, tparams: List[TypeVar], var tpe: ValueType, var fields: List[Field] = Nil) extends TypeConstructor with Fun with Synthetic {
  // Parameter and return type of the constructor:
  lazy val vparams = fields.map { f => f.param }
  val bparams = List.empty[BlockParam]

  def annotatedResult = Some(tpe)

  def annotatedEffects = Some(Effects.Pure)
}

/**
 * The record symbols is _both_ a type (record type) _and_ a term symbol (constructor).
 *
 * param: The underlying constructor parameter
 */
case class Field(name: Name, param: ValueParam, record: Record) extends Fun with Synthetic {
  val tparams = record.tparams
  val tpe = param.tpe.get
  val vparams = List(ValueParam(record.name, Some(ValueTypeApp(record, record.tparams map ValueTypeRef.apply))))
  val bparams = List.empty[BlockParam]

  def annotatedResult = Some(tpe)

  def annotatedEffects = Some(Effects.Pure)
}


case class Interface(name: Name, tparams: List[TypeVar], var ops: List[Operation] = Nil) extends BlockTypeSymbol
case class Operation(name: Name, tparams: List[TypeVar], vparams: List[ValueParam], resultType: ValueType, otherEffects: Effects, effect: Interface) extends Fun {
  val bparams = List.empty[BlockParam]
  def annotatedResult = Some(resultType)
  def annotatedEffects = Some(Effects(otherEffects.toList))

  def appliedEffect = InterfaceType(effect, effect.tparams map ValueTypeRef.apply)

  def isBidirectional: Boolean = otherEffects.nonEmpty
}

/**
 * Effect aliases are *not* block types; they cannot be used for example in `def foo { f : Alias }`.
 */
case class EffectAlias(name: Name, tparams: List[TypeVar], effs: Effects) extends BlockTypeSymbol


/**
 * Something that can be substituted by a capture set
 */
sealed trait CaptVar

/**
 * "Tracked" capture parameters. Like [[TypeVar]] used to abstract
 * over capture. Also see [[BlockParam.capture]].
 *
 * Can be either
 * - [[LexicalRegion]] to model self regions of functions
 */
trait Capture extends TypeSymbol, CaptVar
case class LexicalRegion(name: Name, tree: source.Tree) extends Capture
case class CaptureParameter(name: Name) extends Capture


case class CaptUnificationVar(role: CaptUnificationVar.Role) extends Captures, CaptVar, TypeSymbol {
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
  override def toString = s"{${captures.mkString(", ")}}"

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
 * Builtins
 */
sealed trait Builtin extends Symbol {
  override def builtin = true
}
def isBuiltin(e: Symbol): Boolean = e.builtin

case class BuiltinFunction(
  name: Name,
  tparams: List[TypeVar],
  vparams: List[ValueParam],
  bparams: List[BlockParam],
  result: ValueType,
  effects: Effects,
  purity: ExternFlag.Purity = ExternFlag.Pure,
  body: String = ""
) extends Fun with BlockSymbol with Builtin {
  def annotatedResult = Some(result)
  def annotatedEffects = Some(effects)
}

case class BuiltinType(name: Name, tparams: List[TypeVar]) extends TypeConstructor, Builtin

