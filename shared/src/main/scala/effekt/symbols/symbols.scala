package effekt

import effekt.source.{ Def, FunDef, ModuleDecl, ValDef, VarDef }
import effekt.context.Context
import effekt.regions.{ Region, RegionSet, RegionVar }
import org.bitbucket.inkytonik.kiama.util.Source
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

    // toplevel declared effects
    def effects: Effects = Effects(types.values.collect {
      case e: Effect => e
    })

    // the transformed ast after frontend
    private var _ast = decl
    def ast = _ast

    /**
     * Should be called once after frontend
     */
    def setAst(ast: ModuleDecl): this.type = {
      _ast = ast
      this
    }

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
    def ret: Option[ValueType]

    // invariant: only works if ret is defined!
    def toType: FunctionType = FunctionType(tparams, paramsToTypes(params), ret.get)
    def toType(ret: ValueType): FunctionType = FunctionType(tparams, paramsToTypes(params), ret)

    // TODO maybe readd as "regions", not effects
    //    def effects(implicit C: Context): Effects =
    //      ret.orElse { C.blockTypeOption(this).map { _.ret } }.getOrElse {
    //        C.abort(s"Result type of recursive function ${name} needs to be annotated")
    //      }.effects
  }

  object Fun {
    def unapply(f: Fun): Option[(Name, List[TypeVar], Params, Option[ValueType])] = Some((f.name, f.tparams, f.params, f.ret))
  }

  case class UserFunction(
    name: Name,
    tparams: List[TypeVar],
    params: Params,
    ret: Option[ValueType],
    decl: FunDef
  ) extends Fun

  //  case class Lambda(params: Params) extends Fun {
  //    val name = Name("Anonymous function")
  //
  //    // Lambdas currently do not have an annotated return type
  //    def ret = None
  //
  //    // Lambdas currently do not take type parameters
  //    def tparams = Nil
  //  }

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
   * Introduced by Transformer
   */
  case class Wildcard(module: Module) extends ValueSymbol { val name = Name("_", module) }
  case class Tmp(module: Module) extends ValueSymbol { val name = Name("tmp" + Symbol.fresh.next(), module) }

  /**
   * A symbol that represents a termlevel capability
   */
  //  trait Capability extends BlockSymbol {
  //    def effect: Effect
  //  }

  /**
   * Types
   */
  sealed trait Type

  /**
   * like Params but without name binders
   */
  type Sections = List[List[Type]]

  sealed trait ValueType extends Type {
    def dealias: ValueType = this
  }

  case class BoxedType(blk: BlockType, reg: Region) extends Type

  /**
   * Types of first-class functions
   */
  //  case class FunType(tpe: FunctionType, region: Region) extends ValueType {
  //    override def toString: String = {
  //
  //      val FunctionType(_, params, Effectful(ret, effs)) = tpe
  //      // copy and paste from BlockType.toString
  //      val ps = params.map {
  //        case List(b: FunctionType)          => s"{${b.toString}}"
  //        case ps: List[ValueType @unchecked] => s"(${ps.map { _.toString }.mkString(", ")})"
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
  //  }

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

  case class TypeApp(tpe: ValueType, args: List[ValueType]) extends ValueType {
    override def toString = s"${tpe}[${args.map { _.toString }.mkString(", ")}]"

    override def dealias: ValueType = tpe match {
      case TypeAlias(name, tparams, tpe) =>
        (tparams zip args).toMap.substitute(tpe).dealias
      case other => TypeApp(other.dealias, args.map { _.dealias })
    }
  }

  sealed trait BlockType extends Type {
    def at(reg: Region) = BoxedType(this, reg)
  }

  case class FunctionType(tparams: List[TypeVar], params: Sections, ret: ValueType) extends BlockType {
    override def toString: String = {
      val ps = params.map {
        case List(b: FunctionType)          => s"{${b.toString}}"
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

  /**
   * Types that _can_ be used in type constructor position. e.g. >>>List<<<[T]
   */
  sealed trait TypeConstructor extends TypeSymbol with ValueType
  object TypeConstructor {
    def unapply(t: ValueType): Option[TypeConstructor] = t match {
      case t: TypeVar         => None
      case t: TypeAlias       => unapply(t.dealias)
      case t: TypeConstructor => Some(t)
      case TypeApp(tpe, args) => unapply(tpe)
      case t: BuiltinType     => None
      case t: FunType         => None
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

  sealed trait Effect extends BlockType {
    def name: Name
    def builtin: Boolean
  }

  case class EffectApp(effect: Effect, args: List[ValueType]) extends Effect {
    override def toString = s"${effect}[${args.map { _.toString }.mkString(", ")}]"
    override def builtin = effect.builtin
    override val name = effect.name

    // override def dealias: List[Effect] = ??? // like dealiasing of TypeApp we potentially need to substitute

  }

  case class UserEffect(name: Name, tparams: List[TypeVar], var ops: List[EffectOp] = Nil) extends Effect with TypeSymbol
  case class EffectOp(name: Name, tparams: List[TypeVar], params: List[List[ValueParam]], annotatedReturn: ValueType, effect: UserEffect) extends Fun {
    def ret: Option[ValueType] = Some(annotatedReturn.tpe)
    def appliedEffect = if (effect.tparams.isEmpty) effect else EffectApp(effect, effect.tparams)

    // The effects as seen by the capability passing transformation
    def otherEffects: Effects = annotatedReturn.effects
    def isBidirectional: Boolean = otherEffects.nonEmpty
  }

  /**
   * Builtins
   */
  sealed trait Builtin extends Symbol {
    override def builtin = true
  }

  case class BuiltinFunction(name: Name, tparams: List[TypeVar], params: Params, ret: Option[Effectful], pure: Boolean = true, body: String = "") extends Fun with BlockSymbol with Builtin
  case class BuiltinType(name: Name, tparams: List[TypeVar]) extends ValueType with TypeSymbol with Builtin
  case class BuiltinEffect(name: Name) extends Effect with TypeSymbol with Builtin

  def isBuiltin(e: Symbol): Boolean = e.builtin
}
