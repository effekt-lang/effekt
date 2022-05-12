package effekt

import effekt.source.{ Def, FunDef, ModuleDecl, ValDef, VarDef, ExternFlag }
import effekt.context.Context
import effekt.regions.{ Region, RegionSet, RegionVar }
import kiama.util.Source
import effekt.substitutions._

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
      case e: Effect => e
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

  sealed trait TrackedParam extends Param
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

  sealed trait ValueType extends Type {
    def dealias: ValueType = this
  }

  /**
   * Types of first-class functions
   */
  case class BoxedType(tpe: FunctionType, region: Region) extends ValueType {
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
   * Should neither occur in source programs, nor in infered types
   */
  case class RigidVar(underlying: TypeVar) extends TypeVar(underlying.name) {
    // override def toString = "?" + underlying.name + id
  }

  case class ValueTypeApp(tpe: ValueType, args: List[ValueType]) extends ValueType {
    override def toString = s"${tpe}[${args.map { _.toString }.mkString(", ")}]"

    override def dealias: ValueType = tpe match {
      case TypeAlias(name, tparams, tpe) =>
        (tparams zip args).toMap.substitute(tpe).dealias
      case other => ValueTypeApp(other.dealias, args.map { _.dealias })
    }
  }

  sealed trait BlockType extends Type
  case class CapabilityType(effect: Effect) extends BlockType

  case class FunctionType(tparams: List[TypeVar], vparams: List[ValueType], bparams: List[BlockType], result: ValueType, effects: Effects) extends BlockType {
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

  case class TypeAlias(name: Name, tparams: List[TypeVar], tpe: ValueType) extends ValueType with TypeSymbol {
    override def dealias: ValueType =
      if (tparams.isEmpty) { tpe } else { sys error "Cannot delias unapplied type constructor" }
  }

  /**
   * Types that _can_ be used in type constructor position. e.g. >>>List<<<[T]
   */
  sealed trait TypeConstructor extends TypeSymbol with ValueType
  object TypeConstructor {
    def unapply(t: ValueType): Option[TypeConstructor] = t match {
      case t: TypeVar              => None
      case t: TypeAlias            => unapply(t.dealias)
      case t: TypeConstructor      => Some(t)
      case ValueTypeApp(tpe, args) => unapply(tpe)
      case t: BuiltinType          => None
      case t: BoxedType            => None
    }
  }

  case class DataType(name: Name, tparams: List[TypeVar], var variants: List[Record] = Nil) extends TypeConstructor

  /**
   * Structures are also function symbols to represent the constructor
   */
  case class Record(name: Name, tparams: List[TypeVar], var tpe: ValueType, var fields: List[Field] = Nil) extends TypeConstructor with Fun with Synthetic {
    // Parameter and return type of the constructor:
    lazy val vparams = fields.map { f => f.param }
    val bparams = List.empty[BlockParam]
    def annotatedResult = Some(tpe)
    def annotatedEffects = Some(Pure)
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
    def annotatedEffects = Some(Pure)
  }

  /** Effects */

  // TODO effects are only temporarily symbols to be resolved by namer
  sealed trait Effect {
    def name: Name
    def builtin: Boolean
    // invariant: no EffectAlias in this list
    def dealias: List[Effect] = List(this)
  }

  case class EffectApp(effect: Effect, args: List[ValueType]) extends Effect {
    override def toString = s"${effect}[${args.map { _.toString }.mkString(", ")}]"
    override def builtin = effect.builtin
    override val name = effect.name

    // override def dealias: List[Effect] = ??? // like dealiasing of TypeApp we potentially need to substitute

  }

  case class EffectAlias(name: Name, tparams: List[TypeVar], effs: Effects) extends Effect with TypeSymbol {
    override def dealias: List[Effect] = effs.dealias
  }

  case class ControlEffect(name: Name, tparams: List[TypeVar], var ops: List[EffectOp] = Nil) extends Effect with TypeSymbol
  case class EffectOp(name: Name, tparams: List[TypeVar], vparams: List[ValueParam], resultType: ValueType, otherEffects: Effects, effect: ControlEffect) extends Fun {
    val bparams = List.empty[BlockParam]
    def annotatedResult = Some(resultType)
    def annotatedEffects = Some(otherEffects + appliedEffect)

    def appliedEffect = if (effect.tparams.isEmpty) effect else EffectApp(effect, effect.tparams)

    def isBidirectional: Boolean = otherEffects.nonEmpty
  }

  /**
   * symbols.Effects is like source.Effects, but with resolved effects
   *
   * Effect sets and effectful computations are themselves *not* symbols, they are just aggregates
   *
   * `effects` is dealiased by the smart constructors
   */
  class Effects private[symbols] (effects: List[Effect]) {

    lazy val toList: List[Effect] = effects.distinct

    // This is only used by typer
    def +(eff: Effect): Effects = this ++ Effects(eff)

    def ++(other: Effects): Effects = Effects((other.toList ++ this.toList).distinct)
    def --(other: Effects): Effects = Effects(this.toList.filterNot(other.contains))

    def isEmpty: Boolean = effects.isEmpty
    def nonEmpty: Boolean = effects.nonEmpty

    override def equals(other: Any): Boolean = other match {
      case other: Effects => this.contains(other.toList) && other.contains(this.toList)
      case _              => false
    }

    def contains(e: Effect): Boolean = contains(e.dealias)
    def contains(other: List[Effect]): Boolean = other.toList.forall {
      e => this.toList.flatMap(_.dealias).contains(e)
    }

    def filterNot(p: Effect => Boolean): Effects =
      Effects(effects.filterNot(p))

    def controlEffects: Effects =
      filterNot(_.builtin)

    def dealias: List[Effect] = effects.flatMap { _.dealias }

    override def toString: String = toList match {
      case Nil        => "{}"
      case eff :: Nil => eff.toString
      case effs       => s"{ ${effs.mkString(", ")} }"
    }
  }
  object Effects {

    def apply(effs: Effect*): Effects =
      new Effects(effs.flatMap(_.dealias).toList)

    def apply(effs: Iterable[Effect]): Effects =
      new Effects(effs.flatMap(_.dealias).toList)
  }

  lazy val Pure = new Effects(Nil)

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
