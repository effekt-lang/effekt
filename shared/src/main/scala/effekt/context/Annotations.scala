package effekt
package context

import effekt.util.messages.ErrorReporter
import org.bitbucket.inkytonik.kiama.util.Memoiser

case class Annotation[K, V](name: String, description: String) {
  type Value = V
  override def toString = name
}

/**
 * "local" annotatations that can be backtracked
 *
 * Local annotations can be backed-up and restored to allow backtracking
 * (mostly for typer and overload resolution).
 *
 * Local annotations can be "comitted" to become global ones in the DB,
 * that are assumed to not change anymore.
 */
class Annotations private (private var annotations: Annotations.DB) {
  import Annotations._

  def copy: Annotations = new Annotations(annotations)

  private def annotationsAt(key: Any): Map[Annotation[_, _], Any] =
    annotations.getOrElse(new Key(key), Map.empty)

  private def updateAnnotations(key: Any, annos: Map[Annotation[_, _], Any]): Unit =
    annotations = annotations.updated(new Key(key), annos)

  def annotate[K, V](ann: Annotation[K, V], key: K, value: V): Unit = {
    val anns = annotationsAt(key)
    updateAnnotations(key, anns + (ann -> value))
  }

  def annotationOption[K, V](ann: Annotation[K, V], key: K): Option[V] =
    annotationsAt(key).get(ann).asInstanceOf[Option[V]]

  def annotation[K, V](ann: Annotation[K, V], key: K)(implicit C: ErrorReporter): V =
    annotationOption(ann, key).getOrElse { C.abort(s"Cannot find ${ann.name} for '${key}'") }

  def commit()(implicit global: AnnotationsDB): Unit =
    annotations.foreach {
      case (k, annos) =>
        global.annotate(k.key, annos)
    }

  override def toString = s"Annotations(${annotations})"
}
object Annotations {

  def empty: Annotations = new Annotations(Map.empty)

  private type DB = Map[Annotations.Key[Any], Map[Annotation[_, _], Any]]

  private class Key[T](val key: T) {
    override val hashCode = System.identityHashCode(key)
    override def equals(o: Any) = o match {
      case k: Key[_] => hashCode == k.hashCode
      case _         => false
    }
  }

  /**
   * The type and effect as inferred by typer at a given position in the tree
   *
   * Important for finding the types of temporary variables introduced by transformation
   * Can also be used by LSP server to display type information for type-checked trees
   */
  val TypeAndEffect = Annotation[source.Tree, symbols.Effectful]("TypeAndEffect", "the inferred type and effect")

  /**
   * Type arguments of a _function call_ as inferred by typer
   */
  val TypeArguments = Annotation[source.Call, List[symbols.Type]]("TyperArguments", "the inferred or annotated type arguments")

  /**
   * Value type of symbols like value binders or value parameters
   */
  val ValueType = Annotation[symbols.ValueSymbol, symbols.ValueType]("ValueType", "the type of a value symbol")

  /**
   * Block type of symbols like function definitions, block parameters, or continuations
   */
  val BlockType = Annotation[symbols.BlockSymbol, symbols.BlockType]("BlockType", "the type of a block symbol")
}

trait AnnotationsDB { self: ErrorReporter =>

  private type Annotations = Map[Annotation[_, _], Any]
  private val annotations: Memoiser[Any, Annotations] = Memoiser.makeIdMemoiser()
  private def annotationsAt(key: Any): Annotations = annotations.getOrDefault(key, Map.empty)

  /**
   * Bulk annotating the key
   *
   * Used by Annotations.commit to commit all temporary annotations to the DB
   */
  def annotate[K, V](key: K, value: Map[Annotation[_, _], Any]): Unit = {
    val anns = annotationsAt(key)
    annotations.put(key, anns ++ value)
  }

  def annotate[K, V](ann: Annotation[K, V], key: K, value: V): Unit = {
    val anns = annotationsAt(key)
    annotations.put(key, anns + (ann -> value))
  }

  def annotationOption[K, V](ann: Annotation[K, V], key: K): Option[V] =
    annotationsAt(key).get(ann).asInstanceOf[Option[V]]

  def annotation[K, V](ann: Annotation[K, V], key: K): V =
    annotationOption(ann, key).getOrElse { abort(s"Cannot find ${ann.description} for '${key}'") }

  // Customized Accessors
  // --------------------
  import symbols.{ Symbol, Type, ValueType, BlockType, ValueSymbol, BlockSymbol, Effectful }

  def typeOf(t: source.Tree): Option[Effectful] =
    annotationOption(Annotations.TypeAndEffect, t)

  def assignType(s: Symbol, tpe: BlockType): Unit = s match {
    case b: BlockSymbol => annotate(Annotations.BlockType, b, tpe)
    case _              => abort(s"Trying to store a block type for non block '${s}'")
  }

  def assignType(s: Symbol, tpe: ValueType): Unit = s match {
    case b: ValueSymbol => annotate(Annotations.ValueType, b, tpe)
    case _              => abort(s"Trying to store a value type for non value '${s}'")
  }

  def typeOf(s: Symbol): Type = s match {
    case s: ValueSymbol => valueTypeOf(s)
    case s: BlockSymbol => blockTypeOf(s)
    case _              => abort(s"Cannot find a type for symbol '${s}'")
  }

  def blockTypeOf(s: Symbol): BlockType =
    blockTypeOption(s) getOrElse { abort(s"Cannot find type for block '${s}'") }

  def blockTypeOption(s: Symbol): Option[BlockType] =
    s match {
      case b: BlockSymbol => annotationOption(Annotations.BlockType, b)
      case _              => abort(s"Trying to find a block type for non block '${s}'")
    }

  def valueTypeOf(s: Symbol): ValueType =
    valueTypeOption(s) getOrElse { abort(s"Cannot find value binder for ${s}") }

  def valueTypeOption(s: Symbol): Option[ValueType] = s match {
    case s: ValueSymbol => annotationOption(Annotations.ValueType, s)
    case _              => abort(s"Trying to find a value type for non-value '${s}'")
  }
}
