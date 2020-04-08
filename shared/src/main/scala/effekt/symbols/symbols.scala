package effekt

import effekt.source.{ Def, FunDef, ModuleDecl, ValDef, VarDef }
import effekt.context.TypesDB
import effekt.util.messages.{ ErrorReporter, FatalPhaseError }
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
  sealed trait ValueSymbol extends TermSymbol
  sealed trait BlockSymbol extends TermSymbol

  // TODO move this to Name

  /**
   * path should be an include Effekt-path (foo/bar), not a system dependent file path
   */
  def moduleName(path: String): String = "$" + path.replace('/', '_')
  def moduleFile(path: String): String = path.replace('/', '_') + ".js"

  /**
   * The result of running the frontend on a module.
   * Symbols and types are stored globally in CompilerContext.
   */
  case class Module(
    decl: ModuleDecl,
    source: Source
  ) extends Symbol {
    val name = Name(moduleName(decl.path), this)

    def path = decl.path

    private var _terms: Map[String, Set[TermSymbol]] = _
    def terms = _terms

    private var _types: Map[String, TypeSymbol] = _
    def types = _types

    def export(terms: Map[String, Set[TermSymbol]], types: Map[String, TypeSymbol]): this.type = {
      if (_terms != null)
        throw new FatalPhaseError("Internal compiler error: Already set exports on module")
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

    def effects(implicit db: TypesDB with ErrorReporter): Effects =
      ret.orElse { db.blockTypeOption(this).map { _.ret } }.getOrElse {
        db.abort(s"Result type of recursive function ${name} needs to be annotated")
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
  case class CallTarget(name: Name, symbols: List[Set[BlockSymbol]]) extends BlockSymbol

  /**
   * Types
   */

  sealed trait Type {
  }

  // like Params but without name binders
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
        (tparams zip args).toMap.substitute(tpe).dealias
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

  case class DataType(name: Name, tparams: List[TypeVar], var ctors: List[Constructor] = Nil) extends ValueType with TypeSymbol

  case class Constructor(name: Name, params: List[List[ValueParam]], datatype: DataType) extends Fun {
    def tparams = datatype.tparams
    def ret = if (tparams.size > 0) Some(Effectful(TypeApp(datatype, tparams), Pure)) else Some(Effectful(datatype, Pure))
  }

  /**
   * The record symbols is _both_ a type (record type) _and_ a term symbol (constructor).
   */
  case class Record(name: Name, tparams: List[TypeVar], var fields: List[List[Field]] = Nil) extends ValueType with Fun with TypeSymbol {
    lazy val params = fields.map { _ map { field => field.field } }
    def ret = Some(Effectful(tpe, Pure))
    def tpe = if (tparams.size > 0) TypeApp(this, tparams) else this
  }

  case class Field(name: Name, tpe: ValueType, rec: Record) extends Fun {
    val field = ValueParam(name, Some(tpe))
    val tparams = rec.tparams
    val params = List(List(ValueParam(rec.name, Some(rec.tpe))))
    val ret = Some(Effectful(tpe, Pure))
  }

  sealed trait Effect extends TypeSymbol {
    // invariant: no EffectAlias in this list
    def dealias: List[Effect] = List(this)
  }

  case class EffectAlias(name: Name, effs: Effects) extends Effect {
    override def dealias: List[Effect] = effs.dealias
  }

  case class UserEffect(name: Name, tparams: List[TypeVar], var ops: List[EffectOp] = Nil) extends Effect
  case class EffectOp(name: Name, tparams: List[TypeVar], params: List[List[ValueParam]], ret: Option[Effectful], effect: UserEffect) extends Fun

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

    def contains(e: Effect): Boolean = contains(e.dealias)
    def contains(other: List[Effect]): Boolean = other.toList.forall {
      e => this.toList.contains(e)
    }

    def filterNot(p: Effect => Boolean): Effects =
      Effects(effects.filterNot(p))

    def userDefined: Effects =
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
