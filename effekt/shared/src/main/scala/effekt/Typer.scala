package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{Annotation, Annotations, Context, ContextOps}
import effekt.context.assertions.*
import effekt.source.{ AnyPattern, Def, Effectful, IgnorePattern, MatchPattern, MatchGuard, ModuleDecl, Stmt, TagPattern, Term, Tree, resolve, symbol }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.symbols.kinds.*
import effekt.util.messages.*
import effekt.util.foreachAborting

import scala.language.implicitConversions

/**
 * Typechecking
 * ============
 *
 * Preconditions:
 * --------------
 * Typer assumes that all dependencies already have been type checked.
 * In particular, it assumes that all definitions / symbols (functions, parameters etc.)
 * have been annotated with a type: this models a (global) typing context.
 *
 * Postconditions:
 * ---------------
 * All trees will be annotated with intermediate types (and effects). This is useful for
 * IDE support.
 * Also, after type checking, all definitions of the file will be annotated with their type.
 *
 * Invariants:
 * -----------
 * All effects inferred by Typer are concrete and dealiased. This is established by
 * type [[ConcreteEffects]] and constructor [[Typer.asConcrete]].
 */
case class Result[+T](tpe: T, effects: ConcreteEffects)


object Typer extends Phase[NameResolved, Typechecked] {

  val phaseName = "typer"

  def run(input: NameResolved)(using Context): Option[Typechecked] = ???

}

/**
 * Instances of this class represent an immutable backup of the typer state
 */
private[typer] case class TyperState(annotations: Annotations, unification: UnificationState, capabilityScope: CapabilityScope)

trait TyperOps extends ContextOps { self: Context =>

  private [typer] var annotations: Annotations = Annotations.empty

  /**
   * The unification engine, keeping track of constraints and the current unification scope
   *
   * Contains mutable variables. The methods [[unification.backup()]] and [[unification.restore()]]
   * allow to save a copy of the current state.
   */
  private[typer] val unification = new Unification(using this)

  export unification.{ requireSubtype, requireSubregion, join, instantiate, instantiateFresh, freshTypeVar, freshCaptVar, without, requireSubregionWithout }


  private[typer] def withUnificationScope[T](additional: List[CaptUnificationVar])(block: => T): T = ???

  private[typer] def withUnificationScope[T](block: => T): T = ???

  private [typer] var capabilityScope: CapabilityScope = GlobalCapabilityScope

  private [typer] def bindingCapabilities[R](binder: source.Tree, caps: List[symbols.BlockParam])(f: => R): R = ???

  private [typer] def bindCapabilities[R](binder: source.Tree, caps: List[symbols.BlockParam]): Unit = ()

  private [typer] def bindingAllCapabilities[R](binder: source.Tree)(f: => R): (R, Map[InterfaceType, symbols.BlockParam]) = ???

  private [typer] def capabilityFor(tpe: InterfaceType): symbols.BlockParam = ???

  private [typer] def freshCapabilityFor(tpe: InterfaceType): symbols.BlockParam = ???
  private [typer] def freshCapabilityFor(tpe: InterfaceType, capture: CaptureSet): symbols.BlockParam = ???

  private [typer] def provideCapabilities(call: source.CallLike, effs: List[InterfaceType]): List[BlockParam] = ???

  private [typer] def capabilityReceiver(call: source.Do, eff: InterfaceType): BlockParam = ???

  private[typer] def lookup(s: ValueSymbol): symbols.ValueType = ???

  private[typer] def lookup(s: BlockSymbol): (BlockType, Captures) = ???

  private[typer] def lookupFunctionType(s: BlockSymbol): FunctionType = ???

  private[typer] def lookupBlockType(s: BlockSymbol): BlockType = ???

  private[typer] def lookupCapture(s: BlockSymbol): Captures = ???

  private[typer] def bind(s: ValueSymbol, tpe: ValueType): Unit = ()

  private[typer] def bind(s: BlockSymbol, tpe: BlockType, capt: Captures): Unit = ()

  private[typer] def bind(s: BlockSymbol, tpe: BlockType): Unit = ()

  private[typer] def bind(s: BlockSymbol, capt: Captures): Unit = ()

  private[typer] def bind(bs: Map[Symbol, ValueType]): Unit = ()

  private[typer] def bind(p: ValueParam): Unit = ()

  private[typer] def bind(p: TrackedParam): Unit = ()

  private[typer] def annotateInferredType(t: Tree, e: ValueType) = ()

  private[typer] def annotateInferredType(t: Tree, e: BlockType) = ()

  private[typer] def annotateInferredEffects(t: Tree, e: Effects) = ()

  private[typer] def annotateTypeArgs(call: source.CallLike, targs: List[symbols.ValueType]): Unit = ()

  private[typer] def annotatedTypeArgs(call: source.CallLike): List[symbols.ValueType] = ???

  private[typer] def initTyperstate(): Unit = ()

  private[typer] def backupTyperstate(): TyperState = ???

  private[typer] def restoreTyperstate(st: TyperState): Unit = ()

  private[typer] def commitTypeAnnotations(): Unit = ()
}
