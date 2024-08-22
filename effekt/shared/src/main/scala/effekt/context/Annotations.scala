package effekt
package context

import effekt.symbols.ResumeParam
import effekt.util.messages.ErrorReporter
import kiama.util.Memoiser

case class Annotation[K, V](name: String, description: String, bindToObjectIdentity: Boolean = true) {
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
    val updatedAnns = anns.updated(Annotations.makeKey(ann, key), value)
    annotations = annotations.updated(ann, updatedAnns.asInstanceOf)
  }

  def get[K, V](ann: Annotation[K, V], key: K): Option[V] =
    annotationsAt(ann).get(Annotations.makeKey(ann, key))

  def getOrElse[K, V](ann: Annotation[K, V], key: K, default: => V): V =
    annotationsAt(ann).getOrElse(Annotations.makeKey(ann, key), default)

  def getOrElseUpdate[K, V](ann: Annotation[K, V], key: K, default: => V): V =
    annotationsAt(ann).getOrElse(Annotations.makeKey(ann, key), {
      val value = default
      update(ann, key, value)
      value
    })

  def removed[K, V](ann: Annotation[K, V], key: K): Unit =
    annotations = annotations.updated(ann, annotationsAt(ann).removed(Annotations.makeKey(ann, key)).asInstanceOf)

  def apply[K, V](ann: Annotation[K, V]): List[(K, V)] =
    annotationsAt(ann).map { case (k, v) => (k.key, v) }.toList

  def apply[K, V](ann: Annotation[K, V], key: K)(implicit C: ErrorReporter): V =
    get(ann, key).getOrElse { C.abort(s"Cannot find ${ann.name} '${key}'") }

  def updateAndCommit[K, V](ann: Annotation[K, V])(f: (K, V) => V)(implicit global: AnnotationsDB): Unit =
    val anns = annotationsAt(ann)
    anns.foreach { case (kk, v) => global.annotate(ann, kk.key, f(kk.key, v)) }

  override def toString = s"Annotations(${annotations})"
}
object Annotations {

  def empty: Annotations = new Annotations(Map.empty)

  sealed trait Key[T] { def key: T }

  private class HashKey[T](val key: T) extends Key[T] {
    override val hashCode = System.identityHashCode(key)
    override def equals(o: Any) = o match {
      case k: HashKey[_] => hashCode == k.hashCode
      case _         => false
    }
  }

  private class IdKey[T](val key: T) extends Key[T] {
    override val hashCode = key.hashCode()
    override def equals(o: Any) = o match {
      case k: Key[_] => key == k.key
      case _         => false
    }
  }

  object Key {
    def unapply[T](k: Key[T]): Option[T] = Some(k.key)
  }

  private def makeKey[K, V](ann: Annotation[K, V], k: K): Key[K] =
    if (ann.bindToObjectIdentity) new HashKey(k)
    else new IdKey(k)

  /**
   * The as inferred by typer at a given position in the tree
   *
   * Can also be used by LSP server to display type information for type-checked trees
   */
  val InferredEffect = Annotation[source.Tree, symbols.Effects](
    "InferredEffect",
    "the inferred effect of"
  )

  /**
   * The type as inferred by typer at a given position in the tree
   *
   * Important for finding the types of temporary variables introduced by transformation
   * Can also be used by LSP server to display type information for type-checked trees
   */
  val InferredValueType = Annotation[source.Tree, symbols.ValueType](
    "InferredValueType",
    "the inferred type of"
  )

  /**
   * The type as inferred by typer at a given position in the tree
   */
  val InferredBlockType = Annotation[source.Tree, symbols.BlockType](
    "InferredBlockType",
    "the inferred block type of"
  )

  /**
   * Type arguments of a _function call_ as inferred by typer
   */
  val TypeArguments = Annotation[source.CallLike, List[symbols.ValueType]](
    "TypeArguments",
    "the inferred or annotated type arguments of"
  )

  /**
   * Existential type parameters inferred by the typer when type-checking pattern matches.
   */
  val TypeParameters = Annotation[source.TagPattern | source.OpClause, List[symbols.TypeVar]](
    "TypeParameters",
    "the existentials of the constructor pattern or operation clause"
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
  val BlockType = Annotation[symbols.BlockSymbol, symbols.BlockType](
    "BlockType",
    "the type of block symbol"
  )

  /**
   * Capability set used by a function definition, block parameter, ...
   */
  val Captures = Annotation[symbols.BlockSymbol, symbols.Captures](
    "Captures",
    "the set of used capabilities of a block symbol"
  )

  /**
   * Used by LSP to list all captures
   */
  val CaptureForFile = Annotation[kiama.util.Source, List[(source.Tree, symbols.CaptureSet)]](
    "CaptureSet",
    "all inferred captures for file"
  )

  /**
   * The module a given symbol is defined in
   *
   * @deprecated
   */
  val SourceModule = Annotation[symbols.Symbol, symbols.Module](
    "SourceModule",
    "the source module of symbol"
  )

  /**
   * All symbols defined in a source file
   */
  val DefinedSymbols = Annotation[kiama.util.Source, Set[symbols.Symbol]](
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

  val Capture = Annotation[source.CaptureSet, symbols.CaptureSet](
    "Capture",
    "the resolved capture set for"
  )

  /**
   * Similar to TypeAndEffect: the capture set of a program
   */
  val InferredCapture = Annotation[source.Tree, symbols.CaptureSet](
    "InferredCapture",
    "the inferred capture for source tree"
  )

  /**
   * Capabilities bound by either a [[source.TryHandle]], [[source.FunDef]],
   *   [[source.VarDef]], or [[source.BlockLiteral]].
   *
   * Inferred by typer, used by elaboration.
   */
  val BoundCapabilities = Annotation[source.Tree, List[symbols.BlockParam]](
    "BoundCapabilities",
    "capabilities bound by this tree"
  )

  /**
   * Capabilities inferred as additional arguments to a call.
   *
   * Inferred by typer, used by elaboration.
   */
  val CapabilityArguments = Annotation[source.CallLike, List[symbols.BlockParam]](
    "CapabilityArguments",
    "capabilities inferred as additional arguments for this call"
  )

  val CapabilityReceiver = Annotation[source.Do, symbols.BlockParam](
    "CapabilityReceiver",
    "the receiver as inferred for this effect operation call"
  )

  /**
   * The lexical region as introduced by namer
   *
   * Used by typer for region checking mutable variables.
   */
  val SelfRegion = Annotation[source.Tree, symbols.TrackedParam](
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
  val UnboxParentDef = Annotation[source.Unbox, source.Def](
    "UnboxParentDef",
    "the parent definition of an Unbox if it was synthesized"
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
  type DB = Memoiser[Any, Map[Annotation[_, _], Any]]
  var db: DB = Memoiser.makeIdMemoiser()

  private def annotationsAt(key: Any): Map[Annotation[_, _], Any] = db.getOrDefault(key, Map.empty)

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
    db.put(key, anns ++ value)
  }

  def annotate[K, V](ann: Annotation[K, V], key: K, value: V): Unit = {
    val anns = annotationsAt(key)
    db.put(key, anns + (ann -> value))
  }

  def annotationOption[K, V](ann: Annotation[K, V], key: K): Option[V] =
    annotationsAt(key).get(ann).asInstanceOf[Option[V]]

  def annotation[K, V](ann: Annotation[K, V], key: K): V =
    annotationOption(ann, key).getOrElse { panic(s"Cannot find ${ann.description} for '${key}'") }

  def hasAnnotation[K, V](ann: Annotation[K, V], key: K): Boolean =
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

  def typeOf(s: Symbol): Type = s match {
    case s: ValueSymbol => valueTypeOf(s)
    case s: BlockSymbol => blockTypeOf(s)
    case _              => panic(s"Cannot find a type for symbol '${s}'")
  }

  def functionTypeOf(s: Symbol): FunctionType =
    functionTypeOption(s) getOrElse { panic(s"Cannot find type for block '${s}'") }

  def functionTypeOption(s: Symbol): Option[FunctionType] =
    s match {
      case b: BlockSymbol => annotationOption(Annotations.BlockType, b) flatMap {
        case b: FunctionType => Some(b)
        case _               => None
      }
      // The callsite should be adjusted, this is NOT the job of annotations...
      case v: ValueSymbol => ???
      //        valueTypeOption(v).flatMap { v =>
      //          v.dealias match {
      //            case symbols.BoxedType(tpe: FunctionType, _) => Some(tpe)
      //            case _ => None
      //          }
      //        }
    }

  def blockTypeOf(s: Symbol): BlockType =
    blockTypeOption(s) getOrElse { panic(s"Cannot find interface type for block '${s}'") }

  def blockTypeOption(s: Symbol): Option[BlockType] =
    s match {
      case b: BlockSymbol => annotationOption(Annotations.BlockType, b) flatMap {
        case b: BlockType => Some(b)
      }
      case _ => panic(s"Trying to find a interface type for non block '${s}'")
    }

  def valueTypeOf(s: Symbol): ValueType =
    valueTypeOption(s) getOrElse { panic(s"Cannot find value binder for ${s}") }

  def valueTypeOption(s: Symbol): Option[ValueType] = s match {
    case s: ValueSymbol => annotationOption(Annotations.ValueType, s)
    case _              => panic(s"Trying to find a value type for non-value '${s}'")
  }


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

  def symbolOf(id: source.Id): Symbol = symbolOption(id) getOrElse {
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

  /**
   * Searching the definition for a symbol
   */
  def definitionTreeOption(s: Symbol): Option[source.IdDef] =
    annotationOption(Annotations.DefinitionTree, s)

  /**
   * Adds [[s]] to the set of defined symbols for the current module, by writing
   * it into the [[Annotations.DefinedSymbols]] annotation.
   */
  def addDefinedSymbolToSource(s: Symbol): Unit =
    if (module != null) {
      val syms = annotationOption(Annotations.DefinedSymbols, module.source).getOrElse(Set.empty)
      annotate(Annotations.DefinedSymbols, module.source, syms + s)
    }

  /**
   * List all symbols that have a source module
   *
   * Used by the LSP server to generate outline
   */
  def sourceSymbolsFor(src: kiama.util.Source): Set[Symbol] =
    annotationOption(Annotations.DefinedSymbols, src).getOrElse(Set.empty)

  /**
   * List all references for a symbol
   *
   * Used by the LSP server for reverse lookup
   */
  def distinctReferencesTo(sym: Symbol): List[source.Reference] =
    annotationOption(Annotations.References, sym)
      .getOrElse(Nil)
      .distinctBy(r => System.identityHashCode(r))

  def captureOf(sym: BlockSymbol): symbols.Captures =
    annotation(Annotations.Captures, sym)

  def captureOfOption(sym: BlockSymbol): Option[symbols.Captures] =
    annotationOption(Annotations.Captures, sym)
}
