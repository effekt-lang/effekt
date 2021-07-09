package effekt
package context

import effekt.util.messages.ErrorReporter
import org.bitbucket.inkytonik.kiama.util.Memoiser
import effekt.symbols.SourceModule

case class Annotation[K, V](name: String, description: String) {
  type Value = V
  override def toString = name
}

/**
 * "local" annotations that can be backtracked
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
    annotationOption(ann, key).getOrElse { C.abort(s"Cannot find ${ann.name} '${key}'") }

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
  val TypeAndEffect = Annotation[source.Tree, symbols.Effectful](
    "TypeAndEffect",
    "the inferred type and effect of"
  )

  /**
   * Type arguments of a _function call_ as inferred by typer
   */
  val TypeArguments = Annotation[source.Call, List[symbols.ValueType]](
    "TypeArguments",
    "the inferred or annotated type arguments of"
  )

  /**
   * Value type of symbols like value binders or value parameters
   */
  val ValueType = Annotation[symbols.ValueSymbol, symbols.ValueType](
    "ValueType",
    "the type of value symbol"
  )

  /**
   * Block type of symbols like function definitions, block parameters, or continuations
   */
  val BlockType = Annotation[symbols.BlockSymbol, symbols.InterfaceType](
    "BlockType",
    "the type of block symbol"
  )

  /**
   * The module a given symbol is defined in
   */
  val SourceModule = Annotation[symbols.Symbol, symbols.SourceModule](
    "SourceModule",
    "the source module of symbol"
  )

  val Implements = Annotation[symbols.UserFunction, symbols.Method](
    "Implements",
    "the implementation of a interface method"
  )

  /**
   * The definition tree of a symbol in source
   *
   * Annotated by namer and used by the LSP server for jump-to-definition
   *
   * TODO maybe store the whole definition tree instead of the name, which requries refactoring of assignSymbol
   */
  val DefinitionTree = Annotation[symbols.Symbol, source.IdDef](
    "DefinitionTree",
    "the tree identifying the definition site of symbol"
  )

  /**
   * Approximate list of all references pointing to a symbol
   *
   * Filled by namer and used for reverse lookup in LSP server
   */
  val References = Annotation[symbols.Symbol, List[source.Reference]](
    "References",
    "the references referring to symbol"
  )

  /**
   * The symbol for an identifier as resolved by namer
   *
   * Id can be the definition-site (IdDef) or use-site (IdRef) of the
   * specific symbol
   */
  val Symbol = Annotation[source.Id, symbols.Symbol](
    "Symbol",
    "the symbol for identifier"
  )

  /**
   * The resolved type for a type tree that appears in the source program
   *
   * Resolved and annotated by namer and used by typer.
   */
  val Type = Annotation[source.Type, symbols.Type](
    "Type",
    "the resolved type for"
  )

  /**
   * The blocktype of a calltarget as annotated by typer
   */
  val TargetType = Annotation[source.CallTarget, symbols.BlockType](
    "TargetType",
    "the blocktype for calltarget"
  )

  /**
   * The block type of a block argument as annotated by typer
   */
  val BlockArgumentType = Annotation[source.BlockArg, symbols.BlockType](
    "BlockArgumentType",
    "the inferred type for block argument"
  )

  /*
   * The region a given symbol can be used in
   */
  val Regions = Annotation[symbols.Symbol, regions.Region](
    "Regions",
    "the regions associated with symbol"
  )

  /**
   * The unifier as computed by typer when type checking the module
   */
  val Unifier = Annotation[symbols.SourceModule, substitutions.Unifier](
    "Unifier",
    "the unifier for module"
  )

  /**
   * Similar to TypeAndEffect: the region of a program inferred by RegionChecker
   */
  val InferredRegion = Annotation[source.Tree, regions.RegionSet](
    "InferredRegion",
    "the inferred region for source tree"
  )

}

/**
 * A global annotations database
 *
 * This database is mixed into the compiler `Context` and is
 * globally visible across all phases. If you want to hide changes in
 * subsequent phases, consider using an instance of `Annotions`, instead.
 *
 * Calling `Annotations.commit` transfers all annotations into this global DB.
 *
 * The DB is also "global" in the sense, that modifications cannot be backtracked.
 * It should thus only be used to store a "ground" truth that will not be changed again.
 */
trait AnnotationsDB { self: Context =>

  private type Annotations = Map[Annotation[_, _], Any]
  private val annotations: Memoiser[Any, Annotations] = Memoiser.makeIdMemoiser()
  private def annotationsAt(key: Any): Annotations = annotations.getOrDefault(key, Map.empty)

  /**
   * Copies annotations, keeping existing annotations at `to`
   */
  def copyAnnotations(from: Any, to: Any): Unit = {
    val existing = annotationsAt(to)
    val source = annotationsAt(from)
    annotate(to, source ++ existing)
  }

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
    annotationOption(ann, key).getOrElse { panic(s"Cannot find ${ann.description} for '${key}'") }

  def hasAnnotation[K, V](ann: Annotation[K, V], key: K): Boolean =
    annotationsAt(key).isDefinedAt(ann)

  // Customized Accessors
  // ====================
  import symbols.{ Symbol, Type, ValueType, BlockType, InterfaceType, ValueSymbol, BlockSymbol, Effectful, UserFunction, Method }

  // Types
  // -----

  def typeArguments(c: source.Call): List[symbols.ValueType] =
    annotation(Annotations.TypeArguments, c)

  def inferredTypeOption(t: source.Tree): Option[Effectful] =
    annotationOption(Annotations.TypeAndEffect, t)

  def inferredTypeOf(t: source.Tree): Effectful =
    inferredTypeOption(t).getOrElse {
      panic(s"Internal Error: Missing type of source expression: '${t}'")
    }

  def inferredRegion(t: source.Tree): regions.RegionSet =
    annotation(Annotations.InferredRegion, t)

  def inferredRegionOption(t: source.Tree): Option[regions.RegionSet] =
    annotationOption(Annotations.InferredRegion, t)

  // TODO maybe move to TyperOps
  def assignType(s: Symbol, tpe: InterfaceType): Unit = s match {
    case b: BlockSymbol => annotate(Annotations.BlockType, b, tpe)
    case _              => panic(s"Trying to store a block type for non block '${s}'")
  }

  def assignType(s: Symbol, tpe: ValueType): Unit = s match {
    case b: ValueSymbol => annotate(Annotations.ValueType, b, tpe)
    case _              => panic(s"Trying to store a value type for non value '${s}'")
  }

  def annotateResolvedType(tree: source.Type)(tpe: tree.resolved): Unit =
    annotate(Annotations.Type, tree, tpe)

  def resolvedType(tree: source.Type): tree.resolved =
    annotation(Annotations.Type, tree).asInstanceOf[tree.resolved]

  def typeOf(s: Symbol): Type = s match {
    case s: ValueSymbol => valueTypeOf(s)
    case s: BlockSymbol => interfaceTypeOf(s)
    case _              => panic(s"Cannot find a type for symbol '${s}'")
  }

  def blockTypeOf(s: Symbol): BlockType =
    blockTypeOption(s) getOrElse { panic(s"Cannot find type for block '${s}'") }

  def blockTypeOption(s: Symbol): Option[BlockType] =
    s match {
      case b: BlockSymbol => annotationOption(Annotations.BlockType, b) flatMap {
        case b: BlockType => Some(b)
        case _            => None
      }
      case v: ValueSymbol => valueTypeOption(v).flatMap { v =>
        v.dealias match {
          case symbols.FunType(tpe, _) => Some(tpe)
          case _ => None
        }
      }
    }

  def interfaceTypeOf(s: Symbol): InterfaceType =
    interfaceTypeOption(s) getOrElse { panic(s"Cannot find interface type for block '${s}'") }

  def interfaceTypeOption(s: Symbol): Option[InterfaceType] =
    s match {
      case b: BlockSymbol => annotationOption(Annotations.BlockType, b) flatMap {
        case b: InterfaceType => Some(b)
        case _                => None
      }
      case _ => panic(s"Trying to find a interface type for non block '${s}'")
    }

  def valueTypeOf(s: Symbol): ValueType =
    valueTypeOption(s) getOrElse { panic(s"Cannot find value binder for ${s}") }

  def valueTypeOption(s: Symbol): Option[ValueType] = s match {
    case s: ValueSymbol => annotationOption(Annotations.ValueType, s)
    case _              => panic(s"Trying to find a value type for non-value '${s}'")
  }

  // Calltargets
  // -----------

  // annotated by capability passing
  def annotateCalltarget(t: source.CallTarget, tpe: BlockType): Unit =
    annotate(Annotations.TargetType, t, tpe)

  def blockTypeOf(t: source.CallTarget): BlockType =
    annotation(Annotations.TargetType, t)

  def blockTypeOption(t: source.CallTarget): Option[BlockType] =
    annotationOption(Annotations.TargetType, t)

  def blockTypeOf(t: source.BlockArg): BlockType =
    annotation(Annotations.BlockArgumentType, t)

  // Symbols
  // -------

  /**
   * Stores symbol `sym` as the corresponding symbol for `id`
   *
   * Almost all calls to this method are performed by Namer, which
   * resolves identifier and then assigns the symbols.
   *
   * Typer also calls this method to resolve overloads and store
   * the result of overload resolution.
   */
  def assignSymbol(id: source.Id, sym: Symbol): Unit = id match {
    case id: source.IdDef =>
      annotate(Annotations.DefinitionTree, sym, id)
      annotate(Annotations.Symbol, id, sym)
      annotate(Annotations.SourceModule, sym, sourceModule)
    case _ =>
      annotate(Annotations.Symbol, id, sym)
    //annotate(Annotations.SourceModule, sym, sourceModule)
  }

  def symbolOf(id: source.Id): Symbol = symbolOption(id) getOrElse {
    panic(s"Internal Compiler Error: Cannot find symbol for ${id}")
  }
  def symbolOption(id: source.Id): Option[Symbol] =
    annotationOption(Annotations.Symbol, id)

  def sourceModuleOf(sym: Symbol): SourceModule =
    annotation(Annotations.SourceModule, sym)

  /**
   * Searching the defitions for a Reference
   *
   * This one can fail.
   */
  def symbolOf(tree: source.Reference): tree.symbol = {
    val sym = symbolOf(tree.id).asInstanceOf[tree.symbol]

    val refs = annotationOption(Annotations.References, sym).getOrElse(Nil)
    annotate(Annotations.References, sym, tree :: refs)
    sym
  }

  /**
   * Searching the symbol for a definition
   *
   * These lookups should not fail (except there is a bug in the compiler)
   */
  def symbolOf(tree: source.Definition): tree.symbol =
    symbolOf(tree.id).asInstanceOf[tree.symbol]

  /**
   * Searching the definition for a symbol
   */
  def definitionTreeOption(s: Symbol): Option[source.IdDef] =
    annotationOption(Annotations.DefinitionTree, s)

  /**
   * List all symbols that have a source module
   *
   * Used by the LSP server to generate outline
   */
  def sourceSymbols: Vector[Symbol] =
    annotations.keys.collect {
      case s: Symbol if hasAnnotation(Annotations.SourceModule, s) => s
    }

  /**
   * List all references for a symbol
   *
   * Used by the LSP server for reverse lookup
   */
  def distinctReferencesTo(sym: Symbol): List[source.Reference] =
    annotationOption(Annotations.References, sym)
      .getOrElse(Nil)
      .distinctBy(r => System.identityHashCode(r))

  def annotateRegions(sym: Symbol, r: regions.Region): Unit =
    annotate(Annotations.Regions, sym, r)

  def regionOf(sym: Symbol): regions.Region =
    annotation(Annotations.Regions, sym)

  /** marks function as implementation of method. */
  def implements(f: UserFunction, m: Method): Unit =
    annotate(Annotations.Implements, f, m)

  /** search for method which is implemented by the given function. */
  def implements(f: UserFunction): Option[Method] =
    annotationOption(Annotations.Implements, f)
}
