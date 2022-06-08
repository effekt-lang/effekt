package effekt

import effekt.source.{ Def, FunDef, ModuleDecl, ValDef, VarDef, ExternFlag }
import effekt.context.Context
import kiama.util.Source

/**
 * The symbol table contains things that can be pointed to:
 * - function definitions
 * - type definitions
 * - effect definitions
 * - parameters
 * - value / variable binders
 * - ...
 */
package object symbols {

  // reflecting the two namespaces
  sealed trait TypeSymbol extends Symbol
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

  sealed trait TrackedParam extends Param {
    // every block parameter gives rise to a capture parameter
    lazy val capture: CaptureParam = CaptureParam(name)
  }
  case class BlockParam(name: Name, tpe: BlockType) extends TrackedParam with BlockSymbol
  //  case class CapabilityParam(name: Name, tpe: CapabilityType) extends TrackedParam with Capability {
  //    def effect = tpe.eff
  //    override def toString = s"@${tpe.eff.name}"
  //  }

  case class ResumeParam(module: Module) extends TrackedParam with BlockSymbol { val name = Name.local("resume") }

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
  sealed trait Binder extends ValueSymbol {
    def tpe: Option[ValueType]
    def decl: Def
  }
  case class ValBinder(name: Name, tpe: Option[ValueType], decl: ValDef) extends Binder
  case class VarBinder(name: Name, tpe: Option[ValueType], decl: VarDef) extends Binder

  /**
   * Synthetic symbol representing potentially multiple call targets
   *
   * Refined by typer.
   */
  case class CallTarget(name: Name, symbols: List[Set[TermSymbol]]) extends Synthetic with BlockSymbol

  /**
   * Introduced by Transformer
   */
  case class Wildcard(module: Module) extends ValueSymbol { val name = Name.local("_") }
  case class Tmp(module: Module) extends ValueSymbol { val name = Name.local("tmp" + Symbol.fresh.next()) }

  case class BuiltinCapability(effect: Effect) extends BlockSymbol {
    override val name = effect.name
  }

  /**
   * Types
   */
  sealed trait Type

  /**
   * like Params but without name binders
   */
  type Sections = List[Type]

  sealed trait ValueType extends Type

  /**
   * Types of first-class functions
   */
  case class BoxedType(tpe: BlockType, capture: Captures) extends ValueType {
    // TODO move rendering to different component

    //    override def toString: String = {
    //
    //      val FunctionType(_, params, ret, effs) = tpe
    //      // copy and paste from BlockType.toString
    //      val ps = params.map {
    //        case List(b: FunctionType) => s"{${b.toString}}"
    //        case ps: List[ValueType @unchecked] => s"(${ps.map { _.toString }.mkString(", ")})"
    //        case _ => sys error "Parameter lists are either singleton block params or a list of value params."
    //      }.mkString("")
    //
    //      val effects = effs.toList
    //      val regs = region match {
    //        case RegionSet(r) => r.regions.toList
    //        // to not confuse users, we render uninstantiated region variables as ?
    //        case e            => List("?")
    //      }
    //      val both: List[String] = (effects ++ regs).map { _.toString }
    //
    //      val tpeString = if (both.isEmpty) ret.toString else s"$ret / { ${both.mkString(", ")} }"
    //
    //      s"$ps ⟹ $tpeString"
    //    }
  }

  class TypeVar(val name: Name) extends ValueType with TypeSymbol
  object TypeVar {
    def apply(name: Name): TypeVar = new TypeVar(name)
  }

  /**
   * Introduced when instantiating type schemes
   *
   * Should neither occur in source programs, nor in inferred types
   */
  case class UnificationVar(role: UnificationVar.Role) extends TypeVar(Name.local("?")) {
    override def toString = role match {
      case UnificationVar.TypeVariableInstantiation(underlying, _) => "?" + underlying.toString + id
      case _ => "?" + id
    }
  }
  object UnificationVar {
    sealed trait Role
    case class TypeVariableInstantiation(underlying: TypeVar, call: source.Tree) extends Role
  }

  case class ValueTypeApp(tpe: ValueType, args: List[ValueType]) extends ValueType {
    override def toString = s"${tpe}[${args.map { _.toString }.mkString(", ")}]"
  }

  case class TypeAlias(name: Name, tparams: List[TypeVar], tpe: ValueType) extends ValueType with TypeSymbol

  /**
   * Types that _can_ be used in type constructor position. e.g. >>>List<<<[T]
   */
  sealed trait TypeConstructor extends TypeSymbol with ValueType

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
  case class Field(name: Name, param: ValueParam, rec: Record) extends Fun with Synthetic {
    val tparams = rec.tparams
    val tpe = param.tpe.get
    val vparams = List(ValueParam(rec.name, Some(if (rec.tparams.isEmpty) rec else ValueTypeApp(rec, rec.tparams))))
    val bparams = List.empty[BlockParam]
    def annotatedResult = Some(tpe)
    def annotatedEffects = Some(Effects.Pure)
  }

  /**
   * Block Types
   */

  sealed trait BlockType extends Type

  case class FunctionType(tparams: List[TypeVar], cparams: List[CaptureParam], vparams: List[ValueType], bparams: List[BlockType], result: ValueType, effects: Effects) extends BlockType {
    // TODO move rendering
    //    override def toString: String = {
    //      val ps = params.map {
    //        case List(b: FunctionType) => s"{${b.toString}}"
    //        case ps: List[ValueType @unchecked] => s"(${ps.map { _.toString }.mkString(", ")})"
    //        case _ => sys error "Parameter lists are either singleton block params or a list of value params."
    //      }.mkString("")
    //
    //      tparams match {
    //        case Nil => s"$ps ⟹ $result / $effects"
    //        case tps => s"[${tps.map { _.toString }.mkString(", ")}] $ps ⟹ $result / $effects"
    //      }
    //    }
  }

  /** Effects */

  sealed trait Effect {
    def name: Name
    def builtin: Boolean
  }

  // Interfaces can be used as effects, hence Interface <: Effect
  sealed trait InterfaceType extends BlockType with Effect

  object InterfaceType {
    def unapply(i: InterfaceType): Option[(Interface, List[ValueType])] = i match {
      case i: Interface          => Some((i, Nil))
      case BlockTypeApp(i, args) => Some((i, args))
    }
  }

  case class BlockTypeApp(typeConstructor: Interface, args: List[ValueType]) extends InterfaceType {
    override def toString = s"${typeConstructor}[${args.map { _.toString }.mkString(", ")}]"
    def builtin = typeConstructor.builtin
    def name = typeConstructor.name
  }

  case class Interface(name: Name, tparams: List[TypeVar], var ops: List[Operation] = Nil) extends InterfaceType with TypeSymbol
  case class Operation(name: Name, tparams: List[TypeVar], vparams: List[ValueParam], resultType: ValueType, otherEffects: Effects, effect: Interface) extends Fun {
    val bparams = List.empty[BlockParam]
    def annotatedResult = Some(resultType)
    def annotatedEffects = Some(Effects(appliedEffect :: otherEffects.toList))

    def appliedEffect = if (effect.tparams.isEmpty) effect else BlockTypeApp(effect, effect.tparams)

    def isBidirectional: Boolean = otherEffects.nonEmpty
  }

  /**
   * Effect aliases are *not* block types; they cannot be used for example in `def foo { f : Alias }`.
   */
  case class EffectAlias(name: Name, tparams: List[TypeVar], effs: Effects) extends Effect with TypeSymbol

  /**
   * symbols.Effects is like source.Effects, but with resolved effects
   *
   * Effect sets and effectful computations are themselves *not* symbols, they are just aggregates
   *
   * `effects` is dealiased by the smart constructors
   */
  class Effects private[symbols] (effects: List[Effect]) {

    lazy val toList: List[Effect] = effects.distinct

    def isEmpty: Boolean = effects.isEmpty
    def nonEmpty: Boolean = effects.nonEmpty

    //    override def equals(other: Any): Boolean = other match {
    //      case other: Effects => this.contains(other.toList) && other.contains(this.toList)
    //      case _              => false
    //    }

    def filterNot(p: Effect => Boolean): Effects =
      Effects(effects.filterNot(p))

    def forall(p: Effect => Boolean): Boolean = effects.forall(p)
    def exists(p: Effect => Boolean): Boolean = effects.exists(p)

    override def toString: String = toList match {
      case Nil        => "{}"
      case eff :: Nil => eff.toString
      case effs       => s"{ ${effs.mkString(", ")} }"
    }
  }
  object Effects {

    def apply(effs: Effect*): Effects =
      new Effects(effs.toList)

    def apply(effs: Iterable[Effect]): Effects =
      new Effects(effs.toList)

    def empty: Effects = new Effects(Nil)
    val Pure = empty
  }

  extension(effs: List[Effect]) {
    def controlEffects: List[InterfaceType] = effs.collect {
      case i: InterfaceType if !i.builtin => i
    }
  }

  /**
   * Capture Sets
   */

  sealed trait Captures

  case class CaptureSet(captures: Set[CaptureParam]) extends Captures {
    override def toString = s"{${captures.mkString(", ")}}"

    // This is a very simple form of subtraction, make sure that all constraints have been solved before using it!
    def --(other: CaptureSet): CaptureSet = CaptureSet(captures -- other.captures)
    def ++(other: CaptureSet): CaptureSet = CaptureSet(captures ++ other.captures)
    def +(c: CaptureParam): CaptureSet = CaptureSet(captures + c)
    def flatMap(f: CaptureParam => CaptureSet): CaptureSet = CaptureSet(captures.flatMap(x => f(x).captures))
  }
  object CaptureSet {
    def apply(captures: CaptureParam*): CaptureSet = CaptureSet(captures.toSet)
    def apply(captures: List[CaptureParam]): CaptureSet = CaptureSet(captures.toSet)
    def empty = CaptureSet()
  }

  /**
   * Something that can be substituted by a capture set
   */
  sealed trait CaptVar

  /**
   * "Tracked" capture parameters. Like [[TypeVar]] used to abstract
   * over capture. Also see [[BlockParam.capture]].
   */
  case class CaptureParam(name: Name) extends TypeSymbol, CaptVar

  case class CaptUnificationVar(role: CaptUnificationVar.Role) extends Captures, CaptVar, TypeSymbol {
    val name = Name.local("?C")
    override def toString = role match {
      case CaptUnificationVar.VariableInstantiation(underlying, _) => "?" + underlying.toString + id
      case CaptUnificationVar.Subtraction(handled, underlying) => s"?(${underlying} - {${handled.mkString(", ")}})"
      case CaptUnificationVar.FunctionRegion(fun) => s"?${fun.id.name}" + id
      case CaptUnificationVar.AnonymousFunctionRegion(fun) => s"?anonFun" + id
      case CaptUnificationVar.HandlerRegion(handler) => s"?Ck" + id
      case _ => "?" + id
    }
  }
  object CaptUnificationVar {
    sealed trait Role
    case class VariableInstantiation(underlying: CaptureParam, call: source.Tree) extends Role
    case class HandlerRegion(handler: source.TryHandle) extends Role
    case class FunctionRegion(fun: source.FunDef) extends Role
    case class AnonymousFunctionRegion(fun: source.FunctionArg) extends Role
    case class InferredBox(box: source.Box) extends Role
    case class InferredUnbox(unbox: source.Unbox) extends Role
    // underlying should be a UnificationVar
    case class Subtraction(handled: List[CaptureParam], underlying: CaptUnificationVar) extends Role
    case class Substitution() extends Role
  }

  /**
   * Builtins
   */
  sealed trait Builtin extends Symbol {
    override def builtin = true
  }

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

  case class BuiltinType(name: Name, tparams: List[TypeVar]) extends ValueType with TypeSymbol with Builtin
  case class BuiltinEffect(name: Name, tparams: List[TypeVar] = Nil) extends Effect with TypeSymbol with Builtin

  def isBuiltin(e: Symbol): Boolean = e.builtin
}
