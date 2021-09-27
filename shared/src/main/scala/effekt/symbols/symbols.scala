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
  case class ValueParam(name: LocalName, tpe: ValueType) extends Param with ValueSymbol

  //  sealed trait InterfaceParam extends Param
  // TODO maybe revive the CapabilityParam below
  // ultimately we might want to allow arbitrary expressions with implicit unboxing and so forth.
  case class BlockParam(name: LocalName, tpe: BlockType) extends Param with BlockSymbol
  //  case class CapabilityParam(name: Name, tpe: CapabilityType) extends TrackedParam with Capability {
  //    def effect = tpe.eff
  //    override def toString = s"@${tpe.eff.name}"
  //  }
  case class ResumeParam(module: Module) extends Param with BlockSymbol { val name = LocalName("resume") }

  def paramToType(p: ValueParam) = p.tpe
  def paramToType(p: BlockParam) = p.tpe

  trait Fun extends BlockSymbol {
    def tparams: List[TypeVar]
    def vparams: List[ValueParam]
    def bparams: List[BlockParam]
    def ret: Option[ValueType]

    // invariant: only works if ret is defined!
    def toType: FunctionType = annotatedType.get
    def toType(ret: ValueType): FunctionType = FunctionType(tparams, bparams map CaptureOf, vparams map paramToType, bparams map paramToType, ret)
    def annotatedType: Option[FunctionType] = ret map { toType }
  }

  case class UserFunction(
    name: Name,
    tparams: List[TypeVar],
    vparams: List[ValueParam],
    bparams: List[BlockParam],
    ret: Option[ValueType],
    decl: FunDef
  ) extends Fun

  /**
   * Anonymous symbols used to represent scopes / regions in the region checker
   */
  case class Anon(decl: source.Tree) extends BlockSymbol {
    val name = NoName
  }

  /**
   * Binders represent local value and variable binders
   *
   * They also store a reference to the original defition in the source code
   */

  case class ValBinder(name: LocalName, tpe: Option[ValueType], decl: ValDef) extends ValueSymbol
  case class VarBinder(name: LocalName, tpe: Option[ValueType], decl: VarDef) extends BlockSymbol

  /**
   * Introduced by Transformer
   */
  case class Wildcard(module: Module) extends ValueSymbol { val name = Name.local("_") }
  case class Tmp(module: Module) extends ValueSymbol { val name = Name.local("tmp" + Symbol.fresh.next()) }

  /**
   * A symbol that represents a termlevel capability
   */

  /**
   * Types
   */
  sealed trait Type

  /**
   * Value Types
   */
  sealed trait ValueType extends Type

  /**
   * Types of first-class functions
   */
  case class BoxedType(tpe: BlockType, capt: CaptureSet) extends ValueType {
    override def toString = s"($tpe) at $capt"
  }

  class TypeVar(val name: LocalName) extends ValueType with TypeSymbol
  object TypeVar {
    def apply(name: LocalName): TypeVar = new TypeVar(name)
  }

  /**
   * Introduced when instantiating type schemes
   *
   * Should neither occur in source programs, nor in infered types
   */
  case class UnificationVar(underlying: TypeVar, scope: UnificationScope) extends TypeVar(underlying.name) {
    override def toString = s"?${scope.id}.${underlying.name}$id"
  }

  case class ValueTypeApp(tpe: ValueType, args: List[ValueType]) extends ValueType {
    override def toString = s"${tpe}[${args.map { _.toString }.mkString(", ")}]"
  }

  /**
   * Types that _can_ be used in type constructor position. e.g. >>>List<<<[T]
   */
  sealed trait TypeConstructor extends TypeSymbol with ValueType
  object TypeConstructor {
    def unapply(t: ValueType): Option[TypeConstructor] = t match {
      case t: TypeVar              => None
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
    val bparams = Nil
    def ret = Some(tpe)
  }

  /**
   * The record symbols is _both_ a type (record type) _and_ a term symbol (constructor).
   *
   * param: The underlying constructor parameter
   */
  case class Field(name: LocalName, param: ValueParam, rec: Record) extends Fun with Synthetic {
    val tparams = rec.tparams
    val tpe = param.tpe
    val typeName = Name.local(rec.name.name)
    val vparams = List(ValueParam(typeName, if (rec.tparams.isEmpty) rec else ValueTypeApp(rec, rec.tparams)))
    val bparams = Nil
    val ret = Some(tpe)
  }

  /**
   * Block Types
   */
  sealed trait BlockType extends Type

  case class FunctionType(tparams: List[TypeVar], cparams: List[Capture], vparams: List[ValueType], bparams: List[BlockType], ret: ValueType) extends BlockType {
    override def toString: String = {
      val vps = s"(${vparams.map { _.toString }.mkString(", ")})"
      val bps = bparams.map { b => s"{${b.toString}}" }
      val ps = (vps ++ bps).mkString

      val ts = tparams match {
        case Nil => s""
        case tps => s"[${tps.map { _.toString }.mkString(", ")}]"
      }

      val cs = cparams match {
        case Nil => s""
        case cps => s"[${cps.map { _.toString }.mkString(", ")}]"
      }

      s"$ts $cs $ps => $ret"
    }
  }

  sealed trait InterfaceType extends BlockType {
    def name: Name
    def builtin: Boolean
    def interface: Interface
  }
  object InterfaceType {
    def unapply(i: InterfaceType): Option[(Interface, List[ValueType])] = i match {
      case i: Interface          => Some((i, Nil))
      case BlockTypeApp(i, args) => Some((i, args))
      case _                     => None
    }
  }
  case class BlockTypeApp(constructor: Interface, args: List[ValueType]) extends InterfaceType {
    override def toString = s"${constructor}[${args.map { _.toString }.mkString(", ")}]"
    override def builtin = constructor.builtin
    override val name = constructor.name
    override def interface = constructor
  }

  case class Interface(name: Name, tparams: List[TypeVar], var ops: List[Operation] = Nil) extends InterfaceType with TypeSymbol {
    def interface = this
  }
  case class Operation(name: Name, tparams: List[TypeVar], vparams: List[ValueParam], annotatedReturn: ValueType, effect: Interface) extends Fun {
    def ret: Option[ValueType] = Some(annotatedReturn)
    def bparams = Nil
    //    def appliedEffect = if (effect.tparams.isEmpty) effect else EffectApp(effect, effect.tparams)
  }

  /**
   * Builtins
   */
  sealed trait Builtin extends Symbol {
    override def builtin = true
  }

  case class BuiltinFunction(name: Name, tparams: List[TypeVar], vparams: List[ValueParam], ret: Option[ValueType], pure: Boolean = true, body: String = "") extends Fun with BlockSymbol with Builtin {
    def bparams = Nil
  }
  case class BuiltinType(name: Name, tparams: List[TypeVar]) extends ValueType with TypeSymbol with Builtin
  // case class BuiltinEffect(name: Name, tparams: List[TypeVar] = Nil) extends Effect with TypeSymbol with Builtin

  def isBuiltin(e: Symbol): Boolean = e.builtin

  /**
   * Capture Sets
   */

  case class CaptureSet(captures: Set[Capture]) {
    override def toString = s"{${captures.mkString(", ")}}"

    // This is a very simple form of subtraction, make sure that all constraints have been solved before using it!
    def --(other: CaptureSet): CaptureSet = CaptureSet(captures -- other.captures)
    def ++(other: CaptureSet): CaptureSet = CaptureSet(captures ++ other.captures)
    def +(c: Capture): CaptureSet = CaptureSet(captures + c)
  }
  object CaptureSet {
    def apply(captures: Capture*): CaptureSet = CaptureSet(captures.toSet)
    def apply(captures: List[Capture]): CaptureSet = CaptureSet(captures.toSet)
  }
  val Pure = CaptureSet()

  sealed trait Capture extends TypeSymbol {
    val name: Name
  }

  // TODO we could fuse CaptureOf and CaptureParam into one constructor
  case class CaptureOf(sym: TermSymbol) extends Capture {
    val name = sym.name
    // we compare captures of term symbols by comparing the term symbols
    override def equals(other: Any): Boolean = other match {
      case CaptureOf(otherSym) => sym == otherSym
      case _                   => false
    }
    override def hashCode: Int = sym.hashCode + 13
  }
  case class CaptureParam(name: Name) extends Capture
  case class CaptureUnificationVar(underlying: Capture, scope: UnificationScope) extends Capture {
    val name = underlying.name
    override def toString = "?" + underlying.name + id
  }
}
