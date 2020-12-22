package effekt

import effekt.source.{ Def, FunDef, ModuleDecl, ValDef, VarDef }
import effekt.context.{ Context }
import effekt.util.messages.{ ErrorReporter }
import org.bitbucket.inkytonik.kiama.util.Source
import effekt.subtitutions._

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
    val name = Name.module(decl.path)

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

    // toplevle declared effects
    def effects: Effects = Effects(types.values.collect {
      case e: Effect => e
    })

    /**
     * It is actually possible, that exports is invoked on a single module multiple times:
     * The dependencies of a module might change, which triggers frontend on the same module
     * again. It is the same, since the source and AST did not change.
     */
    def export(
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
  case class BlockParam(name: Name, tpe: BlockType) extends Param with BlockSymbol
  case class ResumeParam(module: Module) extends Param with BlockSymbol { val name = Name("resume", module) }

  /**
   * Right now, parameters are a union type of a list of value params and one block param.
   */
  // TODO Introduce ParamSection also on symbol level and then use Params for types
  type Params = List[List[Param]]

  def paramsToTypes(ps: Params): Sections =
    ps map {
      _ map {
        case BlockParam(_, tpe) => tpe
        case v: ValueParam      => v.tpe.get
        case r: ResumeParam     => sys error "Internal Error: No type annotated on resumption parameter"
      }
    }

  trait Fun extends BlockSymbol {
    def tparams: List[TypeVar]
    def params: Params
    def ret: Option[Effectful]

    // invariant: only works if ret is defined!
    def toType: BlockType = BlockType(tparams, paramsToTypes(params), ret.get)
    def toType(ret: Effectful): BlockType = BlockType(tparams, paramsToTypes(params), ret)

    def effects(implicit C: Context): Effects =
      ret.orElse { C.blockTypeOption(this).map { _.ret } }.getOrElse {
        C.abort(s"Result type of recursive function ${name} needs to be annotated")
      }.effects
  }

  object Fun {
    def unapply(f: Fun): Option[(Name, List[TypeVar], Params, Option[Effectful])] = Some((f.name, f.tparams, f.params, f.ret))
  }

  case class UserFunction(
    name: Name,
    tparams: List[TypeVar],
    params: Params,
    ret: Option[Effectful],
    decl: FunDef
  ) extends Fun

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
  case class CallTarget(name: Name, symbols: List[Set[BlockSymbol]]) extends Synthetic with BlockSymbol

  /**
   * A symbol that represents a termlevel capability
   */
  trait Capability extends BlockSymbol {
    def effect: UserEffect
    val name = effect.name
  }
  case class UserCapability(effect: UserEffect) extends Capability

  case class StateCapability(effect: UserEffect, get: EffectOp, put: EffectOp) extends Capability
  object StateCapability {
    def apply(binder: VarBinder)(implicit C: Context): StateCapability = {
      val tpe = C.valueTypeOf(binder)
      val eff = UserEffect(binder.name, Nil)
      val get = EffectOp(binder.name.rename(name => "get"), Nil, List(Nil), Some(tpe / Pure), eff)
      val put = EffectOp(binder.name.rename(name => "put"), Nil, List(List(ValueParam(binder.name, Some(tpe)))), Some(builtins.TUnit / Pure), eff)
      eff.ops = List(get, put)
      StateCapability(eff, get, put)
    }
  }

  /**
   * Types
   */
  sealed trait Type

  /**
   * like Params but without name binders
   */
  type Sections = List[List[Type]]

  sealed trait ValueType extends Type {
    def /(effs: Effects): Effectful = Effectful(this, effs)
    def dealias: ValueType = this
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
    override def toString = "?" + underlying.name + id
  }

  case class TypeApp(tpe: ValueType, args: List[ValueType]) extends ValueType {
    override def toString = s"${tpe}[${args.map { _.toString }.mkString(", ")}]"

    override def dealias: ValueType = tpe match {
      case TypeAlias(name, tparams, tpe) =>
        Unifier((tparams zip args).toMap).substitute(tpe).dealias
      case other => TypeApp(other.dealias, args.map { _.dealias })
    }
  }

  case class BlockType(tparams: List[TypeVar], params: Sections, ret: Effectful) extends Type {
    override def toString: String = {
      val ps = params.map {
        case List(b: BlockType)             => s"{${b.toString}}"
        case ps: List[ValueType @unchecked] => s"(${ps.map { _.toString }.mkString(", ")})"
      }.mkString("")

      tparams match {
        case Nil => s"$ps ⟹ $ret"
        case tps => s"[${tps.map { _.toString }.mkString(", ")}] $ps ⟹ $ret"
      }
    }
  }

  case class TypeAlias(name: Name, tparams: List[TypeVar], tpe: ValueType) extends ValueType with TypeSymbol {
    override def dealias: ValueType =
      if (tparams.isEmpty) { tpe } else { sys error "Cannot delias unapplied type constructor" }
  }

  trait TypeConstructor extends TypeSymbol with ValueType
  object TypeConstructor {
    def unapply(t: ValueType): Option[TypeConstructor] = t match {
      case t: TypeVar         => None
      case t: TypeAlias       => unapply(t.dealias)
      case t: TypeConstructor => Some(t)
      case TypeApp(tpe, args) => unapply(tpe)
      case t: BuiltinType     => None
    }
  }

  case class DataType(name: Name, tparams: List[TypeVar], var variants: List[Record] = Nil) extends TypeConstructor

  /**
   * Structures are also function symbols to represent the constructor
   */
  case class Record(name: Name, tparams: List[TypeVar], var tpe: ValueType, var fields: List[Field] = Nil) extends TypeConstructor with Fun with Synthetic {
    // Parameter and return type of the constructor:
    lazy val params = List(fields.map { f => f.param })
    def ret = Some(Effectful(tpe, Pure))
  }

  /**
   * The record symbols is _both_ a type (record type) _and_ a term symbol (constructor).
   *
   * param: The underlying constructor parameter
   */
  case class Field(name: Name, param: ValueParam, rec: Record) extends Fun with Synthetic {
    val tparams = rec.tparams
    val tpe = param.tpe.get
    val params = List(List(ValueParam(rec.name, Some(if (rec.tparams.isEmpty) rec else TypeApp(rec, rec.tparams)))))
    val ret = Some(Effectful(tpe, Pure))
  }

  /** Effects */

  sealed trait Effect {
    def name: Name
    def builtin: Boolean
    // invariant: no EffectAlias in this list
    def dealias: List[Effect] = List(this)
  }

  case class EffectAlias(name: Name, effs: Effects) extends Effect with TypeSymbol {
    override def dealias: List[Effect] = effs.dealias
  }

  case class UserEffect(name: Name, tparams: List[TypeVar], var ops: List[EffectOp] = Nil) extends Effect with TypeSymbol
  case class EffectOp(name: Name, tparams: List[TypeVar], params: List[List[ValueParam]], ret: Option[Effectful], effect: UserEffect) extends Fun {
    def otherEffects: Effects = ret.get.effects - effect
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

    def +(eff: Effect): Effects = this ++ Effects(eff)
    def -(eff: Effect): Effects = this -- Effects(eff)

    def ++(other: Effects): Effects = Effects((other.toList ++ this.toList).distinct)
    def --(other: Effects): Effects = Effects(this.toList.filterNot(other.contains))

    def isEmpty: Boolean = effects.isEmpty
    def nonEmpty: Boolean = effects.nonEmpty

    def distinct: Effects = new Effects(toList)

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

    def userDefined: Effects =
      filterNot(_.builtin)

    def userEffects: List[UserEffect] =
      effects collect {
        case u: UserEffect => u
      }

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

  case class Effectful(tpe: ValueType, effects: Effects) {
    override def toString = if (effects.isEmpty) tpe.toString else s"$tpe / $effects"
  }

  object / {
    def unapply(e: Effectful): Option[(ValueType, Effects)] = Some(e.tpe, e.effects)
  }

  /**
   * Builtins
   */
  sealed trait Builtin extends Symbol {
    override def builtin = true
  }

  case class BuiltinFunction(name: Name, tparams: List[TypeVar], params: Params, ret: Option[Effectful], pure: Boolean = true, body: String = "") extends Fun with BlockSymbol with Builtin
  case class BuiltinType(name: Name, tparams: List[TypeVar]) extends ValueType with TypeSymbol with Builtin
  case class BuiltinEffect(name: Name, tparams: List[TypeVar] = Nil) extends Effect with TypeSymbol with Builtin

  def isBuiltin(e: Symbol): Boolean = e.builtin
}
