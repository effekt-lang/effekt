package effekt
package context

import effekt.symbols.{BlockSymbol, BlockType, ResumeParam, Symbol, ValueSymbol}
import effekt.util.messages.ErrorReporter

import java.util

sealed trait Annotation[K, V]

case class SymbolAnnotation[K <: symbols.Symbol, V](name: String, description: String) extends Annotation[K, V] {
  type Value = V

  override def toString = name
}

case class SourceAnnotation[K <: kiama.util.Source, V](name: String, description: String) extends Annotation[K, V] {
  type Value = V

  override def toString = name
}

case class TreeAnnotation[K <: source.Tree, V](name: String, description: String) extends Annotation[K, V] {
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
class Annotations private(
  /**
   * Local Annotations are organized differently to allow simple access.
   */
  private var annotations: Map[Annotation[_, _], Map[Annotations.Key[Any], Any]]
) {
  import Annotations._

  def copy: Annotations = new Annotations(annotations)

  def annotationsAt[K, V](ann: Annotation[K, V]): Map[Key[K], V] =
    annotations.getOrElse(ann, Map.empty).asInstanceOf

  def update[K, V](ann: Annotation[K, V], key: K, value: V): Unit = {
    val anns = annotationsAt(ann)
    val updatedAnns = anns.updated(Key(key), value)
    annotations = annotations.updated(ann, updatedAnns.asInstanceOf)
  }

  def get[K, V](ann: Annotation[K, V], key: K): Option[V] =
    annotationsAt(ann).get(Key(key))

  def getOrElse[K, V](ann: Annotation[K, V], key: K, default: => V): V =
    annotationsAt(ann).getOrElse(Key(key), default)

  def getOrElseUpdate[K, V](ann: Annotation[K, V], key: K, default: => V): V =
    annotationsAt(ann).getOrElse(Key(key), {
      val value = default
      update(ann, key, value)
      value
    })

  def removed[K, V](ann: Annotation[K, V], key: K): Unit =
    annotations = annotations.updated(ann, annotationsAt(ann).removed(Key(key)).asInstanceOf)

  def apply[K, V](ann: Annotation[K, V]): List[(K, V)] =
    annotationsAt(ann).map { case (k, v) => (k.key, v) }.toList

  def apply[K, V](ann: Annotation[K, V], key: K)(using C: ErrorReporter): V =
    get(ann, key).getOrElse { C.abort(s"Cannot find ${ann.toString} '${key}'") }

  def updateAndCommit[K, V](ann: Annotation[K, V])(f: (K, V) => V)(using treesDB: TreeAnnotations, symbolsDB: SymbolAnnotations): Unit =
    val anns = annotationsAt(ann)
    anns.foreach { case (kk, v) =>
      kk.key match {
        case sym: symbols.Symbol =>
          symbolsDB.annotate(ann.asInstanceOf[SymbolAnnotation[_, V]], sym, f(sym, v))
        case key: source.Tree =>
          treesDB.annotate(ann.asInstanceOf[TreeAnnotation[_, V]], key, f(key, v))
      }
    }

  override def toString = s"Annotations(${annotations})"
}
object Annotations {

  def empty: Annotations = new Annotations(Map.empty)

  class Key[T](val key: T) {
    override val hashCode = System.identityHashCode(key)

    override def equals(o: Any) = o match {
      case k: Key[_] => hashCode == k.hashCode
      case _ => false
    }
  }

  /**
   * The as inferred by typer at a given position in the tree
   *
   * Can also be used by LSP server to display type information for type-checked trees
   */
  val InferredEffect = TreeAnnotation[source.Tree, symbols.Effects](
    "InferredEffect",
    "the inferred effect of"
  )

  /**
   * The type as inferred by typer at a given position in the tree
   *
   * Important for finding the types of temporary variables introduced by transformation
   * Can also be used by LSP server to display type information for type-checked trees
   */
  val InferredValueType = TreeAnnotation[source.Tree, symbols.ValueType](
    "InferredValueType",
    "the inferred type of"
  )

  /**
   * The type as inferred by typer at a given position in the tree
   */
  val InferredBlockType = TreeAnnotation[source.Tree, symbols.BlockType](
    "InferredBlockType",
    "the inferred block type of"
  )

  /**
   * Type arguments of a _function call_ as inferred by typer
   */
  val TypeArguments = TreeAnnotation[source.CallLike, List[symbols.ValueType]](
    "TypeArguments",
    "the inferred or annotated type arguments of"
  )

  /**
   * Existential type parameters inferred by the typer when type-checking pattern matches.
   */
  val TypeParameters = TreeAnnotation[source.TagPattern | source.OpClause, List[symbols.TypeVar]](
    "TypeParameters",
    "the existentials of the constructor pattern or operation clause"
  )

  /**
   * Value type of symbols like value binders or value parameters
   */
  val ValueType = SymbolAnnotation[symbols.ValueSymbol, symbols.ValueType](
    "ValueType",
    "the type of value symbol"
  )

  /**
   * Block type of symbols like function definitions, block parameters, or continuations
   */
  val BlockType = SymbolAnnotation[symbols.BlockSymbol, symbols.BlockType](
    "BlockType",
    "the type of block symbol"
  )

  /**
   * Capability set used by a function definition, block parameter, ...
   */
  val Captures = SymbolAnnotation[symbols.BlockSymbol, symbols.Captures](
    "Captures",
    "the set of used capabilities of a block symbol"
  )

  /**
   * Used by LSP to list all captures
   */
  val CaptureForFile = SourceAnnotation[kiama.util.Source, List[(source.Tree, symbols.CaptureSet)]](
    "CaptureSet",
    "all inferred captures for file"
  )

  /**
   * Used by LSP to list all holes
   */
  val HolesForFile = SourceAnnotation[kiama.util.Source, List[(symbols.Hole, symbols.scopes.Scope)]](
    "HolesForFile",
    "All holes with information about the names in scope"
  )
  
  /**
   * The module a given symbol is defined in
   *
   * @deprecated
   */
  val SourceModule = SymbolAnnotation[symbols.Symbol, symbols.Module](
    "SourceModule",
    "the source module of symbol"
  )

  /**
   * Used by LSP for jump-to-definition of imports
   */
  val IncludedSymbols = TreeAnnotation[source.Include, symbols.Module](
    "IncludedSymbols",
    "the symbol for an import / include"
  )

  /**
   * All symbols defined in a source file
   */
  val DefinedSymbols = SourceAnnotation[kiama.util.Source, Set[symbols.Symbol]](
    "DefinedSymbols",
    "all symbols for source file"
  )

  /**
   * The definition tree of a symbol in source
   *
   * Annotated by namer and used by the LSP server for jump-to-definition
   *
   * TODO maybe store the whole definition tree instead of the name, which requries refactoring of assignSymbol
   */
  val DefinitionTree = SymbolAnnotation[symbols.Symbol, source.IdDef](
    "DefinitionTree",
    "the tree identifying the definition site of symbol"
  )

  /**
   * Approximate list of all references pointing to a symbol
   *
   * Filled by namer and used for reverse lookup in LSP server
   */
  val References = SymbolAnnotation[symbols.Symbol, List[source.Reference]](
    "References",
    "the references referring to symbol"
  )

  /**
   * The symbol for an identifier as resolved by namer
   *
   * Id can be the definition-site (IdDef) or use-site (IdRef) of the
   * specific symbol
   */
  val Symbol = TreeAnnotation[source.Id, symbols.Symbol](
    "Symbol",
    "the symbol for identifier"
  )

  /**
   * The resolved type for a type tree that appears in the source program
   *
   * Resolved and annotated by namer and used by typer.
   */
  val Type = TreeAnnotation[source.Type, symbols.Type](
    "Type",
    "the resolved type for"
  )

  val Capture = TreeAnnotation[source.CaptureSet, symbols.CaptureSet](
    "Capture",
    "the resolved capture set for"
  )

  /**
   * Similar to TypeAndEffect: the capture set of a program
   */
  val InferredCapture = TreeAnnotation[source.Tree, symbols.CaptureSet](
    "InferredCapture",
    "the inferred capture for source tree"
  )

  /**
   * Capabilities bound by either a [[source.TryHandle]], [[source.FunDef]],
   *   [[source.VarDef]], or [[source.BlockLiteral]].
   *
   * Inferred by typer, used by elaboration.
   */
  val BoundCapabilities = TreeAnnotation[source.Tree, List[symbols.BlockParam]](
    "BoundCapabilities",
    "capabilities bound by this tree"
  )

  /**
   * Capabilities inferred as additional arguments to a call.
   *
   * Inferred by typer, used by elaboration.
   */
  val CapabilityArguments = TreeAnnotation[source.CallLike, List[symbols.BlockParam]](
    "CapabilityArguments",
    "capabilities inferred as additional arguments for this call"
  )

  val CapabilityReceiver = TreeAnnotation[source.Do, symbols.BlockParam](
    "CapabilityReceiver",
    "the receiver as inferred for this effect operation call"
  )

  /**
   * The lexical region as introduced by namer
   *
   * Used by typer for region checking mutable variables.
   */
  val SelfRegion = TreeAnnotation[source.Tree, symbols.TrackedParam](
    "SelfRegion",
    "the region corresponding to a lexical scope"
  )

  /**
   * The [[source.Def]] which is a parent of this [[source.Unbox]] node
   * provided that the [[source.Unbox]] was synthesized by the compiler.
   *
   * Introduced by the pretyper.
   * Used by typer in order to display a more precise error message.
   */
  val UnboxParentDef = TreeAnnotation[source.Unbox, source.Def](
    "UnboxParentDef",
    "the parent definition of an Unbox if it was synthesized"
  )
}

/**
 * Global annotations on syntax trees
 *
 * This database is mixed into the compiler `Context` and is
 * globally visible across all phases. If you want to hide changes in
 * subsequent phases, consider using an instance of `Annotations`, instead.
 *
 * Calling `Annotations.commit` transfers all annotations into the global databases.
 *
 * The DB is also "global" in the sense, that modifications cannot be backtracked.
 * It should thus only be used to store a "ground" truth that will not be changed again.
 */
trait TreeAnnotations { self: Context =>
  private type AnnotationsMap = Map[TreeAnnotation[_, _], Any]

  private type Annotations = Map[TreeAnnotation[_, _], Any]
  type DB = util.IdentityHashMap[source.Tree, Map[TreeAnnotation[_, _], Any]]
  var db: DB = new util.IdentityHashMap()

  private def annotationsAt[K](key: K): AnnotationsMap =
    db.getOrDefault(key, Map.empty)

  /**
   * Copies annotations, keeping existing annotations at `to`
   */
  def copyAnnotations(from: source.Tree, to: source.Tree): Unit = {
    val existing = annotationsAt(to)
    val source   = annotationsAt(from)
    annotate(to, source ++ existing)
  }

  /**
   * Bulk annotating the key
   *
   * Used by Annotations.commit to commit all temporary annotations to the DB
   */
  def annotate(key: source.Tree, value: AnnotationsMap): Unit = {
    val anns = db.getOrDefault(key, Map.empty)
    db.put(key, anns ++ value)
  }

  def annotate[K <: source.Tree, V](ann: TreeAnnotation[K, V], key: source.Tree, value: V): Unit = {
    val anns = db.getOrDefault(key, Map.empty)
    db.put(key, anns + (ann -> value))
  }

  def annotationOption[V](ann: TreeAnnotation[_, V], key: source.Tree): Option[V] =
    annotationsAt(key).get(ann).asInstanceOf[Option[V]]

  def annotation[V](ann: TreeAnnotation[_, V], key: source.Tree): V =
    annotationOption(ann, key).getOrElse {
      panic(s"Cannot find ${ann.description} for '${key}'")
    }

  def hasAnnotation[V](ann: TreeAnnotation[_, V], key: source.Tree): Boolean =
    annotationsAt(key).isDefinedAt(ann)

  // Customized Accessors
  // ====================
  import symbols.{ Symbol, Type, ValueType, FunctionType, BlockType, ValueSymbol, BlockSymbol, Module, Effects }

  // Types
  // -----

  def typeArguments(c: source.CallLike): List[symbols.ValueType] =
    annotation(Annotations.TypeArguments, c)

  def inferredTypeOption(t: source.Tree): Option[ValueType] =
    annotationOption(Annotations.InferredValueType, t)

  def inferredTypeOf(t: source.Tree): ValueType =
    inferredTypeOption(t).getOrElse {
      panic(s"Internal Error: Missing type of source expression: '${t}'")
    }

  def inferredBlockTypeOption(t: source.Tree): Option[BlockType] =
    annotationOption(Annotations.InferredBlockType, t)

  def inferredBlockTypeOf(t: source.Tree): BlockType =
    inferredBlockTypeOption(t).getOrElse {
      panic(s"Internal Error: Missing type of source block: '${ t }'")
    }

  def inferredEffectOption(t: source.Tree): Option[Effects] =
    annotationOption(Annotations.InferredEffect, t)

  def inferredEffectOf(t: source.Tree): Effects =
    inferredEffectOption(t).getOrElse {
      panic(s"Internal Error: Missing effect of source expression: '${t}'")
    }

  def inferredTypeAndEffectOption(t: source.Tree): Option[(ValueType, Effects)] =
    for {
      tpe <- inferredTypeOption(t)
      eff <- inferredEffectOption(t)
    } yield (tpe, eff)

  def inferredTypeAndEffectOf(t: source.Tree): (ValueType, Effects) =
    inferredTypeAndEffectOption(t).getOrElse {
      panic(s"Internal Error: Missing type of source expression: '${t}'")
    }

  def inferredCapture(t: source.Tree): symbols.CaptureSet =
    annotation(Annotations.InferredCapture, t)

  def inferredCaptureOption(t: source.Tree): Option[symbols.CaptureSet] =
    annotationOption(Annotations.InferredCapture, t)

  def annotateResolvedType(tree: source.Type)(tpe: symbols.Type): Unit =
    annotate(Annotations.Type, tree, tpe)

  def resolvedType(tree: source.Type): symbols.Type =
    annotation(Annotations.Type, tree)

  def annotateResolvedCapture(tree: source.CaptureSet)(capt: symbols.CaptureSet): Unit =
    annotate(Annotations.Capture, tree, capt)

  def resolvedCapture(tree: source.CaptureSet): symbols.CaptureSet =
    annotation(Annotations.Capture, tree)

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
      sym match {
        case _: ResumeParam =>
        case s: symbols.TrackedParam =>
          // for tracked params, also note the id als definition site for the capture.
          annotate(Annotations.DefinitionTree, s.capture, id)
        case _ =>
      }
      annotate(Annotations.Symbol, id, sym)
      addDefinedSymbolToSource(sym)
    case _ =>
      annotate(Annotations.Symbol, id, sym)
      // addDefinedSymbolToSource(sym)
  }

  def symbolOf(id: source.Id): Symbol = this.symbolOption(id) getOrElse {
    panic(s"Internal Compiler Error: Cannot find symbol for ${id}")
  }
  def symbolOption(id: source.Id): Option[Symbol] =
    annotationOption(Annotations.Symbol, id)

  /**
   * Searching the definitions for a Reference
   *
   * This one can fail.
   */
  def symbolOf(tree: source.Reference): Symbol = {
    val sym = symbolOf(tree.id)

    val refs = annotationOption(Annotations.References, sym).getOrElse(Nil)
    annotate(Annotations.References, sym, tree :: refs)
    sym
  }

  /**
   * Searching the symbol for a definition
   *
   * These lookups should not fail (except there is a bug in the compiler)
   */
  def symbolOf(tree: source.Definition): Symbol =
    symbolOf(tree.id)
}

/**
 * Global annotations on entire Source objects
 *
 * It is very important that the comparison between keys is based on value rather than object identity:
 * Even when a separate Source object is crated for the same file contents, it should track the same annotations.
 * This situation frequently occurs in the language server where sources are transmitted from the language client (editor).
 */
trait SourceAnnotations { self: Context =>
  import scala.collection.mutable

  private val sourceAnnotationsDB: mutable.Map[kiama.util.Source, Map[SourceAnnotation[_, _], Any]] =
    mutable.Map.empty

  private def annotationsAt(source: kiama.util.Source): Map[SourceAnnotation[_, _], Any] =
    sourceAnnotationsDB.getOrElse(source, Map.empty)

  def annotate[A](ann: SourceAnnotation[_, A], source: kiama.util.Source, value: A): Unit = {
    val anns = annotationsAt(source)
    sourceAnnotationsDB.update(source, anns + (ann -> value))
  }

  def annotationOption[A](ann: SourceAnnotation[_, A], source: kiama.util.Source): Option[A] =
    annotationsAt(source).get(ann).asInstanceOf[Option[A]]

  /**
   * List all symbols that have a source module
   *
   * Used by the LSP server to generate outline
   */
  def sourceSymbolsFor(src: kiama.util.Source): Set[symbols.Symbol] =
    annotationOption(Annotations.DefinedSymbols, src).getOrElse(Set.empty)

  /**
   * Adds [[s]] to the set of defined symbols for the current module, by writing
   * it into the [[Annotations.DefinedSymbols]] annotation.
   */
  def addDefinedSymbolToSource(s: symbols.Symbol): Unit =
    if (module != null) {
      val src = module.source
      val syms = annotationOption(Annotations.DefinedSymbols, src).getOrElse(Set.empty)
      annotate(Annotations.DefinedSymbols, src, syms + s)
    }
}

/**
 * Global annotations on symbols
 */
trait SymbolAnnotations { self: Context =>

  private val symbolAnnotationsDB: util.IdentityHashMap[symbols.Symbol, Map[SymbolAnnotation[_, _], Any]] =
    new util.IdentityHashMap()

  // Retrieve the annotations for a given symbol.
  private def annotationsAt(sym: symbols.Symbol): Map[SymbolAnnotation[_, _], Any] =
    symbolAnnotationsDB.getOrDefault(sym, Map.empty)

  // Annotate a symbol with an annotation and its value.
  def annotate[A](ann: SymbolAnnotation[_, A], sym: symbols.Symbol, value: A): Unit = {
    val key = sym
    val anns = annotationsAt(sym)
    symbolAnnotationsDB.put(key, anns + (ann -> value))
  }

  // Retrieve an optional annotation for a symbol.
  def annotationOption[A](ann: SymbolAnnotation[_, A], sym: symbols.Symbol): Option[A] =
    annotationsAt(sym).get(ann).asInstanceOf[Option[A]]

  def typeOf(s: Symbol): symbols.Type = s match {
    case s: ValueSymbol => valueTypeOf(s)
    case s: BlockSymbol => blockTypeOf(s)
    case _ => panic(s"Cannot find a type for symbol '${s}'")
  }

  // Retrieve the value type of a value symbol.
  def valueTypeOption(s: symbols.Symbol): Option[symbols.ValueType] = s match {
    case vs: symbols.ValueSymbol => annotationOption(Annotations.ValueType, vs)
    case _ => panic(s"Trying to find a value type for non-value '${s}'")
  }

  def valueTypeOf(s: symbols.Symbol): symbols.ValueType =
    valueTypeOption(s).getOrElse(panic(s"Cannot find value type for ${s}"))

  def blockTypeOption(s: Symbol): Option[BlockType] =
    s match {
      case b: BlockSymbol => annotationOption(Annotations.BlockType, b) flatMap {
        case b: BlockType => Some(b)
      }
      case _ => panic(s"Trying to find a interface type for non block '${s}'")
    }

  def blockTypeOf(s: symbols.Symbol): symbols.BlockType =
    blockTypeOption(s).getOrElse(panic(s"Cannot find block type for ${s}"))

  // Retrieve the function type of a block symbol.
  def functionTypeOption(s: symbols.Symbol): Option[symbols.FunctionType] = s match {
    case bs: symbols.BlockSymbol =>
      annotationOption(Annotations.BlockType, bs) match {
        case Some(ft: symbols.FunctionType) => Some(ft)
        case _ => None
      }
      // The callsite should be adjusted, this is NOT the job of annotations...
    case v: ValueSymbol => ???
      //        valueTypeOption(v).flatMap { v =>
      //          v.dealias match {
      //            case symbols.BoxedType(tpe: FunctionType, _) => Some(tpe)
      //            case _ => None
      //          }
      //        }
    case _ => None
  }

  def functionTypeOf(s: symbols.Symbol): symbols.FunctionType =
    functionTypeOption(s).getOrElse(panic(s"Cannot find function type for ${s}"))

  /**
   * Searching the definition for a symbol
   */
  def definitionTreeOption(s: symbols.Symbol): Option[source.IdDef] =
    annotationOption(Annotations.DefinitionTree, s)

  /**
   * List all references for a symbol
   *
   * Used by the LSP server for reverse lookup
   */
  def distinctReferencesTo(sym: symbols.Symbol): List[source.Reference] =
    annotationOption(Annotations.References, sym)
      .getOrElse(Nil)
      .asInstanceOf[List[source.Reference]]
      .distinctBy(r => System.identityHashCode(r))

  def captureOf(sym: symbols.BlockSymbol): symbols.Captures =
    annotationOption(Annotations.Captures, sym)
      .getOrElse(panic(s"Cannot find captures for ${sym}"))

  def captureOfOption(sym: symbols.BlockSymbol): Option[symbols.Captures] =
    annotationOption(Annotations.Captures, sym)
}
