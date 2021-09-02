package effekt

import effekt.source.{ Def, FunDef, ModuleDecl, ValDef, VarDef }
import effekt.context.Context
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
  case class ValueParam(name: LocalName, tpe: Option[ValueType]) extends Param with ValueSymbol

  sealed trait InterfaceParam extends Param
  // TODO maybe revive the CapabilityParam below
  // ultimately we might want to allow arbitrary expressions with implicit unboxing and so forth.
  case class BlockParam(name: LocalName, tpe: InterfaceType) extends InterfaceParam with BlockSymbol
  //  case class CapabilityParam(name: Name, tpe: CapabilityType) extends TrackedParam with Capability {
  //    def effect = tpe.eff
  //    override def toString = s"@${tpe.eff.name}"
  //  }
  //  case class ResumeParam(module: Module) extends TrackedParam with BlockSymbol { val name = Name("resume", module) }

  /**
   * Right now, parameters are a union type of a list of value params and one block param.
   */
  // TODO Introduce ParamSection also on symbol level and then use Params for types
  type Params = List[List[Param]]

  def paramsToTypes(ps: Params): Sections =
    ps map {
      _ map {
        case BlockParam(_, tpe) => tpe
        // case CapabilityParam(_, tpe) => tpe
        case v: ValueParam      => v.tpe.get
        // case r: ResumeParam     => sys error "Internal Error: No type annotated on resumption parameter"
      }
    }

  trait Fun extends BlockSymbol {
    def tparams: List[TypeVar]
    def params: Params
    def ret: Option[ValueType]

    // invariant: only works if ret is defined!
    def toType: BlockType = BlockType(tparams, paramsToTypes(params), ret.get)
    def toType(ret: ValueType): BlockType = BlockType(tparams, paramsToTypes(params), ret)
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

  /**
   * Anonymous symbols used to represent scopes / regions in the region checker
   */
  sealed trait Anon extends TermSymbol {
    val name = NoName
    def decl: source.Tree
  }

  case class BlockArg(decl: source.Tree) extends BlockSymbol with Anon

  case class Lambda(params: Params, decl: source.Tree) extends Fun with Anon {
    // Lambdas currently do not have an annotated return type
    def ret = None

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
  case class ValBinder(name: LocalName, tpe: Option[ValueType], decl: ValDef) extends Binder
  case class VarBinder(name: LocalName, tpe: Option[ValueType], decl: VarDef) extends Binder

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

  sealed trait ValueType extends Type

  /**
   * Types of first-class functions
   */
  case class FunType(tpe: BlockType /*, region: Region */ ) extends ValueType

  class TypeVar(val name: LocalName) extends ValueType with TypeSymbol
  object TypeVar {
    def apply(name: LocalName): TypeVar = new TypeVar(name)
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
  }

  sealed trait InterfaceType extends Type
  case class CapabilityType(effect: Effect) extends InterfaceType {
    override def toString = effect.toString
  }

  case class BlockType(tparams: List[TypeVar], params: Sections, ret: ValueType) extends InterfaceType {
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

  /**
   * Types that _can_ be used in type constructor position. e.g. >>>List<<<[T]
   */
  sealed trait TypeConstructor extends TypeSymbol with ValueType
  object TypeConstructor {
    def unapply(t: ValueType): Option[TypeConstructor] = t match {
      case t: TypeVar         => None
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
    def ret = Some(tpe)
  }

  /**
   * The record symbols is _both_ a type (record type) _and_ a term symbol (constructor).
   *
   * param: The underlying constructor parameter
   */
  case class Field(name: LocalName, param: ValueParam, rec: Record) extends Fun with Synthetic {
    val tparams = rec.tparams
    val tpe = param.tpe.get
    val typeName = Name.local(rec.name.name)
    val params = List(List(ValueParam(typeName, Some(if (rec.tparams.isEmpty) rec else TypeApp(rec, rec.tparams)))))
    val ret = Some(tpe)
  }

  /** Effects */

  // TODO effects are only temporarily symbols to be resolved by namer
  sealed trait Effect {
    def name: Name
    def builtin: Boolean
  }
  //  case class EffectApp(effect: UserEffect, args: List[ValueType]) extends Effect {
  //    override def toString = s"${effect}[${args.map { _.toString }.mkString(", ")}]"
  //    override def builtin = effect.builtin
  //    override val name = effect.name
  //
  //  }
  //
  case class UserEffect(name: Name, tparams: List[TypeVar], var ops: List[EffectOp] = Nil) extends Effect with TypeSymbol
  case class EffectOp(name: Name, tparams: List[TypeVar], params: List[List[ValueParam]], annotatedReturn: ValueType, effect: UserEffect) extends Fun {
    def ret: Option[ValueType] = Some(annotatedReturn)
    //    def appliedEffect = if (effect.tparams.isEmpty) effect else EffectApp(effect, effect.tparams)
  }

  /**
   * Builtins
   */
  sealed trait Builtin extends Symbol {
    override def builtin = true
  }

  case class BuiltinFunction(name: Name, tparams: List[TypeVar], params: Params, ret: Option[ValueType], pure: Boolean = true, body: String = "") extends Fun with BlockSymbol with Builtin
  case class BuiltinType(name: Name, tparams: List[TypeVar]) extends ValueType with TypeSymbol with Builtin
  // case class BuiltinEffect(name: Name, tparams: List[TypeVar] = Nil) extends Effect with TypeSymbol with Builtin

  def isBuiltin(e: Symbol): Boolean = e.builtin
}
