package effekt

import effekt.source.{ Def, ExternFlag, FunDef, ModuleDecl, ValDef, VarDef }
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
  case class VarBinder(name: Name, tpe: Option[ValueType], region: TrackedParam, decl: VarDef) extends Binder with BlockSymbol

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

  case class BuiltinCapability(name: Name, effect: InterfaceType) extends BlockSymbol {
    lazy val capture: Capture = CaptureParameter(name)
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

  case class TypeAlias(name: Name, tparams: List[TypeVar], tpe: ValueType) extends TypeSymbol

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
   * [[BlockType]]
   *   |
   *   |- [[FunctionType]]
   *   |
   *   |- [[InterfaceType]]
   *      |
   *      |- [[BlockTypeApp]]
   *      |- [[Interface]]
   *      |- [[BuiltinEffect]]
   *
   * Effects are a
   *   list of [[InterfaceType]]
   *
   * Outside of the hierarchy are
   *   [[EffectAlias]]
   * which are resolved by [[Namer]] to a list of [[InterfaceType]]s
   */

  sealed trait BlockType extends Type

  // TODO new function type draft:
  //   example
  //     FunctionType(Nil, List(Cf), Nil, Nil, List((Exc -> Cf)), BoxedType(Exc, Cf), List(Console))
  //   instantiated:
  //     FunctionType(Nil, Nil, Nil, Nil, List((Exc -> ?C1)), BoxedType(Exc, ?C1), List(Console))
  //  case class FunctionType(
  //    tparams: List[TypeVar],
  //    cparams: List[Capture],
  //    vparams: List[ValueType],
  //    // (S -> C) corresponds to { f :^C S }, that is a block parameter with capture C
  //    bparams: List[(BlockType, Captures)],
  //    capabilities: List[(InterfaceType, Captures)],
  //    result: ValueType,
  //    builtins: List[InterfaceType]
  //  ) extends BlockType {
  //    def controlEffects = capabilities.map { _._1 }
  //    def builtinEffects = builtins
  //    def effects: Effects = Effects(controlEffects ++ builtinEffects)
  //  }


  case class FunctionType(
    tparams: List[TypeVar],
    cparams: List[Capture],
    vparams: List[ValueType],
    bparams: List[BlockType],
    result: ValueType,
    effects: Effects
  ) extends BlockType


  /** Effects */

  sealed trait InterfaceType extends BlockType {
    def builtin: Boolean
    def name: Name
  }

  case class BlockTypeApp(typeConstructor: Interface | BuiltinEffect, args: List[ValueType]) extends InterfaceType {
    override def toString = s"${typeConstructor}[${args.map { _.toString }.mkString(", ")}]"
    def builtin = typeConstructor.builtin
    def name = typeConstructor.name
  }

  case class Interface(name: Name, tparams: List[TypeVar], var ops: List[Operation] = Nil) extends InterfaceType, TypeSymbol
  case class Operation(name: Name, tparams: List[TypeVar], vparams: List[ValueParam], resultType: ValueType, otherEffects: Effects, effect: Interface) extends Fun {
    val bparams = List.empty[BlockParam]
    def annotatedResult = Some(resultType)
    def annotatedEffects = Some(Effects(otherEffects.toList))

    def appliedEffect = if (effect.tparams.isEmpty) effect else BlockTypeApp(effect, effect.tparams)

    def isBidirectional: Boolean = otherEffects.nonEmpty
  }

  def interfaceOf(tpe: InterfaceType)(using C: ErrorReporter): Interface = tpe match {
    case BlockTypeApp(i: Interface, args) => i
    case i: Interface => i
    case BlockTypeApp(b: BuiltinEffect, args) => C.abort(s"Required a concrete interface but got a builtin effect: ${b}")
    case b: BuiltinEffect => C.abort(s"Required a concrete interface but got a builtin effect: ${b}")
  }

    /**
     * Effect aliases are *not* block types; they cannot be used for example in `def foo { f : Alias }`.
     */
    case class EffectAlias(name: Name, tparams: List[TypeVar], effs: Effects) extends TypeSymbol

  /**
   * Represents effect sets on function types.
   *
   * All effects are dealiased by namer. Effects are inferred via [[typer.ConcreteEffects]] so
   * by construction all entries in the set of effects here should be concrete (no unification variables).
   *
   * Effect sets are themselves *not* symbols, they are just aggregates.
   *
   * We do not enforce entries to be distinct. This way we can substitute types and keep duplicate entries.
   * For instances { State[S], State[T] }[S -> Int, T -> Int] then becomes { State[Int], State[Int] }.
   * This is important since we need to pass two capabilities in this case.
   *
   * Method [[controlEffects]] computes the canonical ordering of capabilities for this set of effects.
   * Disjointness needs to be ensured manually when constructing effect sets (for instance via [[typer.ConcreteEffects]]).
   */
  case class Effects(effects: List[InterfaceType]) {

    lazy val toList: List[InterfaceType] = effects.distinct

    def isEmpty: Boolean = effects.isEmpty
    def nonEmpty: Boolean = effects.nonEmpty

    def filterNot(p: InterfaceType => Boolean): Effects =
      Effects(effects.filterNot(p))

    def forall(p: InterfaceType => Boolean): Boolean = effects.forall(p)
    def exists(p: InterfaceType => Boolean): Boolean = effects.exists(p)

    lazy val controlEffects: List[InterfaceType] = effects.controlEffects
    lazy val builtinEffects: List[InterfaceType] = effects.builtinEffects

    def distinct: Effects = Effects(effects.distinct)

    override def toString: String = toList match {
      case Nil        => "{}"
      case eff :: Nil => eff.toString
      case effs       => s"{ ${effs.mkString(", ")} }"
    }
  }
  object Effects {

    def apply(effs: InterfaceType*): Effects =
      new Effects(effs.toList)

    def apply(effs: Iterable[InterfaceType]): Effects =
      new Effects(effs.toList)

    def empty: Effects = new Effects(Nil)
    val Pure = empty
  }

  extension(effs: List[InterfaceType]) {
    // establishes the canonical ordering
    def controlEffects: List[InterfaceType] = effs.collect {
      case i: InterfaceType if !i.builtin => i
    }.sortBy(f => f.hashCode())
    def builtinEffects: List[InterfaceType] = effs.collect {
      case i: InterfaceType if i.builtin => i
    }
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
    case class AnonymousFunctionRegion(fun: source.FunctionArg) extends Role
    case class InferredBox(box: source.Box) extends Role
    case class InferredUnbox(unbox: source.Unbox) extends Role
    // underlying should be a UnificationVar
    case class Subtraction(handled: List[Capture], underlying: CaptUnificationVar) extends Role
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

  case class BuiltinType(name: Name, tparams: List[TypeVar]) extends ValueType, TypeSymbol, Builtin

  // Builtin effects take the role of built in block types, while BuiltinType plays the role of a builtin value type
  case class BuiltinEffect(name: Name, tparams: List[TypeVar] = Nil) extends InterfaceType, TypeSymbol, Builtin

  def isBuiltin(e: Symbol): Boolean = e.builtin
}
