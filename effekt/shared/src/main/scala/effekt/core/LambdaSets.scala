package effekt
package core

import effekt.context.Context
import effekt.PhaseResult.CoreTransformed
import effekt.core.Type.functionType
import effekt.util.DB
import effekt.util.messages.ErrorMessageReifier

import java.util.IdentityHashMap
import scala.collection.mutable

/** Relational flow analysis for block implementations after monomorphization.
  *
  * A solution associates every callable with the complete vectors of block
  * implementations observed at its entries. Free block variables are treated
  * as hidden parameters and precede the explicit block parameters in a vector.
  * Keeping vectors intact preserves correlations between their components.
  */
object LambdaSets extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "lambda-set-specialization"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case input @ CoreTransformed(_, _, _, core) =>
      if Context.config.optimize() then {
        Some(input.copy(core = transform(core)))
      }
      else Some(input)
  }

  enum Callable {
    case Function(id: Id)
    case Operation(implementation: Id, method: Id)
  }

  enum BlockCase {
    case Function(id: Id)
    case Implementation(id: Id)
    case Open
  }

  case class Projection(owner: Callable, position: Int)

  enum Entry {
    case Known(block: BlockCase)
    case Project(projection: Projection)
    case Open
  }

  private case class CallableInfo(
    captureIds: Vector[Id],
    captures: Vector[Entry],
    explicitParameters: Int
  ) {
    def arity: Int = captures.size + explicitParameters
  }

  private case class FunctionDefinition(
    literal: BlockLit,
    valueCaptures: Vector[ValueParam],
    topLevel: Boolean
  ) {
    def valueClosed: Boolean = valueCaptures.isEmpty
  }

  private case class ImplementationDefinition(
    implementation: Implementation,
    captureIds: Vector[Id],
    captures: Vector[Entry],
    valueCaptures: Vector[ValueParam],
    topLevel: Boolean
  )

  private enum Call {
    case Apply(callee: Entry, arguments: Vector[Entry])
    case Invoke(receiver: Entry, method: Id, arguments: Vector[Entry])
  }

  private enum BoxSource {
    case Block(box: Expr.Box, entry: Entry)
    case Value(id: Id)
    case Open
  }

  private case class BoxFlow(source: BoxSource, destination: Id)

  case class Constraints private[LambdaSets] (
    private[LambdaSets] val callables: Map[Callable, CallableInfo],
    private[LambdaSets] val operations: Map[(Id, Id), Callable.Operation],
    private[LambdaSets] val calls: List[Call],
    private[LambdaSets] val escapes: List[Entry],
    private[LambdaSets] val functions: Map[Id, FunctionDefinition],
    private[LambdaSets] val implementations: Map[Id, ImplementationDefinition],
    private[LambdaSets] val anonymous: IdentityHashMap[Block, BlockCase],
    private[LambdaSets] val boxFlows: List[BoxFlow],
    private[LambdaSets] val boxes: IdentityHashMap[Expr.Box, Entry],
    private[LambdaSets] val unboxes: Set[Id],
    private[LambdaSets] val directUnboxes: IdentityHashMap[Expr.Box, Unit],
    private[LambdaSets] val unsafeBoxValues: Set[Id],
    private[LambdaSets] val unsafeBoxes: IdentityHashMap[Expr.Box, Unit]
  )

  case class Solution(
    variants: Map[Callable, Set[Vector[BlockCase]]],
    genericCases: Set[BlockCase]
  )

  /** Assert the fragment on which lambda-set specialization operates.
    * Polymorphic externs remain legal opaque boundaries.
    */
  def assertMonomorphic(module: ModuleDecl)(using Context): Unit = {
    val externs = module.externs.collect { case Extern.Def(id, _, _, _, _, _, _, _, _) => id }.toSet

    def require(condition: Boolean, message: => String): Unit =
      if !condition then Context.abort(pretty"Lambda-set analysis expected monomorphic Core: ${message}")

    module.declarations.foreach {
      case Declaration.Data(id, tparams, constructors) =>
        require(tparams.isEmpty, s"data '${id}' still has type parameters")
        constructors.foreach { constructor =>
          require(constructor.tparams.isEmpty, s"constructor '${constructor.id}' still has type parameters")
        }
      case Declaration.Interface(id, tparams, properties) =>
        require(tparams.isEmpty, s"interface '${id}' still has type parameters")
        properties.foreach {
          case Property(id, BlockType.Function(tparams, _, _, _, _)) =>
            require(tparams.isEmpty, s"operation '${id}' still has type parameters")
          case _ => ()
        }
    }

    Tree.visit(module.definitions) {
      case BlockLit(tparams, _, _, _, _) =>
        require(tparams.isEmpty, "a block literal still has type parameters")

      case Operation(id, tparams, _, _, _, _) =>
        require(tparams.isEmpty, s"operation '${id}' still has type parameters")

      case Stmt.App(BlockVar(id, _, _), targs, _, _) =>
        require(targs.isEmpty || externs.contains(id), s"application of '${id}' still has type arguments")

      case Stmt.App(_, targs, _, _) =>
        require(targs.isEmpty, "an application still has type arguments")

      case Stmt.ImpureApp(_, callee, targs, _, _, _) =>
        require(targs.isEmpty || externs.contains(callee.id), s"application of '${callee.id}' still has type arguments")

      case Expr.PureApp(callee, targs, _) =>
        require(targs.isEmpty || externs.contains(callee.id), s"application of '${callee.id}' still has type arguments")

      case Stmt.Invoke(_, method, _, targs, _, _) =>
        require(targs.isEmpty, s"invocation of '${method}' still has type arguments")
    }
  }

  def collect(module: ModuleDecl)(using Context): Constraints = {
    assertMonomorphic(module)
    new Collector(module).result()
  }

  def solve(constraints: Constraints)(using Context): Solution = {
    val solution = mutable.Map.empty[Callable, Set[Vector[BlockCase]]]
      .withDefaultValue(Set.empty)

    def variants(owner: Callable): Set[Vector[BlockCase]] = solution(owner)

    type Substitution = Map[Callable, Vector[BlockCase]]

    def dependencies(entries: Iterable[Entry]): List[Callable] =
      entries.collect { case Entry.Project(Projection(owner, _)) => owner }.toList.distinct

    def substitute(entry: Entry, substitution: Substitution): BlockCase = entry match {
      case Entry.Known(block) => block
      case Entry.Open => BlockCase.Open
      case Entry.Project(Projection(owner, position)) =>
        substitution.getOrElse(owner,
          Context.abort(pretty"Missing lambda-set substitution for '${show(owner)}'"))
          .lift(position)
          .getOrElse(Context.abort(pretty"Invalid lambda-set projection '${show(owner)}.${position}'"))
    }

    /** Ground entries together. One complete vector is chosen for each owner,
      * so two projections of the same owner remain correlated.
      */
    def ground(
      entries: Vector[Entry],
      initial: Substitution = Map.empty
    ): List[(Vector[BlockCase], Substitution)] = {
      val substitutions = dependencies(entries).filterNot(initial.contains).foldLeft(List(initial)) {
        case (current, owner) =>
          for {
            substitution <- current
            variant <- variants(owner).toList
          } yield substitution + (owner -> variant)
      }
      substitutions.map { substitution =>
        entries.map(substitute(_, substitution)) -> substitution
      }
    }

    def add(owner: Callable, variant: Vector[BlockCase]): Boolean = {
      val info = constraints.callables.getOrElse(owner,
        Context.abort(pretty"No lambda-set information for '${show(owner)}'"))
      if variant.size != info.arity then
        Context.abort(pretty"Wrong lambda-vector arity for '${show(owner)}': expected ${info.arity}, found ${variant.size}")

      val previous = solution(owner)
      if previous.contains(variant) then false
      else {
        solution(owner) = previous + variant
        true
      }
    }

    def enter(
      owner: Callable,
      arguments: Vector[BlockCase],
      substitution: Substitution
    ): Boolean = {
      val info = constraints.callables(owner)
      ground(info.captures, substitution).foldLeft(false) {
        case (changed, (captures, _)) => add(owner, captures ++ arguments) || changed
      }
    }

    var changed = true
    while changed do {
      changed = false
      constraints.calls.foreach {
        case Call.Apply(callee, arguments) =>
          ground(callee +: arguments).foreach { case (resolved, substitution) =>
            resolved.head match {
              case BlockCase.Function(id) =>
                val owner = Callable.Function(id)
                if constraints.callables.contains(owner) then
                  changed = enter(owner, resolved.tail, substitution) || changed
              case BlockCase.Implementation(_) | BlockCase.Open => ()
            }
          }

        case Call.Invoke(receiver, method, arguments) =>
          ground(receiver +: arguments).foreach { case (resolved, substitution) =>
            resolved.head match {
              case BlockCase.Implementation(id) =>
                constraints.operations.get(id -> method).foreach { owner =>
                  changed = enter(owner, resolved.tail, substitution) || changed
                }
              case BlockCase.Function(_) | BlockCase.Open => ()
            }
          }
      }
    }

    val genericCases = mutable.Set.empty[BlockCase]

    def retainGeneric(block: BlockCase): Unit = block match {
      case BlockCase.Open => ()
      case known => genericCases += known
    }

    constraints.escapes.foreach { entry =>
      ground(Vector(entry)).foreach { case (resolved, _) => retainGeneric(resolved.head) }
    }

    // Arguments to an unknown callee or receiver cross a representation
    // boundary even when the argument itself is statically known.
    constraints.calls.foreach {
      case Call.Apply(callee, arguments) =>
        ground(callee +: arguments).foreach { case (resolved, _) =>
          if resolved.head == BlockCase.Open then resolved.tail.foreach(retainGeneric)
        }
      case Call.Invoke(receiver, _, arguments) =>
        ground(receiver +: arguments).foreach { case (resolved, _) =>
          if resolved.head == BlockCase.Open then resolved.tail.foreach(retainGeneric)
        }
    }

    Solution(solution.toMap, genericCases.toSet)
  }

  def analyze(module: ModuleDecl)(using Context): Solution = solve(collect(module))

  /** Specialize definitions at their demanded concrete lambda vectors. Fixed
    * block and capture parameters are removed immediately. Specialization is
    * demand driven: rewriting one worker can reveal more precise demands on
    * definitions that it calls.
    */
  def transform(module: ModuleDecl)(using Context): ModuleDecl = {
    val constraints = collect(module)
    val solution = solve(constraints)
    val graph = new GraphBuilder(constraints, solution).result()
    val result = new Specializer(module, constraints, graph).result()
    result.typecheck()
    result
  }

  def show(solution: Solution): String = {
    val bindings = solution.variants.toList
      .filter(_._2.nonEmpty)
      .sortBy { case (owner, _) => show(owner) }
      .map { case (owner, variants) =>
        val rendered = variants.toList.map(show).sorted.mkString("{ ", ", ", " }")
        s"${show(owner)} ↦ ${rendered}"
      }
    val flows = if bindings.isEmpty then "S = ∅" else bindings.mkString("S = ", ",\n    ", "")
    if solution.genericCases.isEmpty then flows
    else {
      val generic = solution.genericCases.toList.map(show).sorted.mkString("{ ", ", ", " }")
      s"${flows}\ngeneric = ${generic}"
    }
  }

  private def show(owner: Callable): String = owner match {
    case Callable.Function(id) => id.name.name
    case Callable.Operation(implementation, method) => s"${implementation.name.name}.${method.name.name}"
  }

  private def show(block: BlockCase): String = block match {
    case BlockCase.Function(id) => id.name.name
    case BlockCase.Implementation(id) => id.name.name
    case BlockCase.Open => "?"
  }

  private def show(vector: Vector[BlockCase]): String =
    vector.map(show).mkString("<", ", ", ">")

  /** Nominal node in the finite lambda-set graph. Cases refer to other nodes;
    * lambda sets are never expanded structurally.
    */
  case class LambdaSetId(id: Int)
  case class LambdaCase(block: BlockCase, captures: Vector[LambdaSetId])
  case class LambdaSet(cases: Set[LambdaCase], open: Boolean)

  private case class LambdaGraph(
    sets: Map[LambdaSetId, LambdaSet],
    instances: Map[(BlockCase, Vector[LambdaSetId]), LambdaSetId],
    projections: Map[Projection, LambdaSetId],
    boxes: Map[Id, LambdaSetId],
    boxedExpressions: IdentityHashMap[Expr.Box, LambdaSetId]
  ) {
    def apply(id: LambdaSetId): LambdaSet = sets(id)

    def instance(block: BlockCase, captures: Vector[LambdaSetId]): Option[LambdaSetId] =
      instances.get(block -> captures)
  }

  /** A coordinate is either fixed to a concrete lambda term or remains
    * represented by the original block parameter. Complete vectors are kept
    * intact, so specialization never invents a Cartesian product.
    */
  private type Shape = Vector[Option[LambdaSetId]]

  private case class Variant(owner: Callable, shape: Shape, id: Id)

  private enum Selection {
    case Known(set: LambdaSetId, representation: List[Expr])
    case Dynamic

    def lambdaSet: Option[LambdaSetId] = this match {
      case Known(set, _) => Some(set)
      case Dynamic => None
    }
  }

  private case class Specialization(blocks: Map[Id, Selection])

  /** Build and quotient a finite nominal graph. Projection and variant nodes
    * are allocated before their equations are filled, so recursive lambda-set
    * equations become ordinary graph cycles rather than recursive Scala
    * values. Partition refinement then identifies bisimilar nodes.
    */
  private class GraphBuilder(constraints: Constraints, solution: Solution)(using Context) {
    private type RawId = Int
    private case class RawCase(block: BlockCase, captures: Vector[RawId])
    private case class RawNode(
      cases: mutable.LinkedHashSet[RawCase] = mutable.LinkedHashSet.empty,
      includes: mutable.LinkedHashSet[RawId] = mutable.LinkedHashSet.empty,
      var open: Boolean = false
    )

    private val nodes = mutable.ArrayBuffer.empty[RawNode]
    private def fresh(open: Boolean = false): RawId = {
      val id = nodes.size
      nodes += RawNode(open = open)
      id
    }

    private val openNode = fresh(open = true)

    private val orderedVariants = solution.variants.toList
      .sortBy { case (owner, _) => show(owner) }
      .flatMap { case (owner, variants) =>
        variants.toList.sortBy(_.map(show).mkString("|")).map(owner -> _)
      }

    private val genericRaw: Map[Projection, RawId] =
      constraints.callables.toList
        .sortBy { case (owner, _) => show(owner) }
        .flatMap { case (owner, info) =>
          Vector.tabulate(info.arity)(position => Projection(owner, position) -> fresh())
        }.toMap

    private val variantRaw: Map[(Callable, Vector[BlockCase]), Vector[RawId]] =
      orderedVariants.map { case key @ (owner, vector) =>
        key -> Vector.fill(vector.size)(fresh())
      }.toMap

    private val boxRaw: Map[Id, RawId] = {
      val ids = constraints.boxFlows.iterator.flatMap { flow =>
        flow.destination :: (flow.source match {
          case BoxSource.Value(id) => id :: Nil
          case BoxSource.Block(_, _) | BoxSource.Open => Nil
        })
      }.toSet.toList.sortBy(id => (id.name.name, id.id))
      ids.map(_ -> fresh()).toMap
    }

    /** Box aliases form representation components: every value connected by
      * an alias flow must use the same representation. A component is lowered
      * precisely when it is observed by a local unbox and never crosses an
      * opaque value boundary.
      */
    private val lowerableBoxes: Set[Id] = {
      val neighbours = mutable.Map.empty[Id, mutable.Set[Id]]
        .withDefaultValue(mutable.Set.empty)

      def connect(left: Id, right: Id): Unit = {
        neighbours.getOrElseUpdate(left, mutable.Set.empty) += right
        neighbours.getOrElseUpdate(right, mutable.Set.empty) += left
      }

      constraints.boxFlows.foreach {
        case BoxFlow(BoxSource.Value(source), destination) => connect(source, destination)
        case _ => ()
      }

      def closure(seeds: Iterable[Id]): Set[Id] = {
        val reached = mutable.Set.from(seeds)
        val pending = mutable.Stack.from(seeds)
        while pending.nonEmpty do {
          neighbours.getOrElse(pending.pop(), mutable.Set.empty).foreach { next =>
            if reached.add(next) then pending.push(next)
          }
        }
        reached.toSet
      }

      val opaque = constraints.boxFlows.collect {
        case BoxFlow(BoxSource.Open, destination) => destination
        case BoxFlow(BoxSource.Block(box, _), destination)
            if constraints.unsafeBoxes.containsKey(box) => destination
      }
      val unsafe = closure(constraints.unsafeBoxValues ++ opaque)
      closure(constraints.unboxes).diff(unsafe)
    }

    private val boxedExpressionRaw = new IdentityHashMap[Expr.Box, RawId]

    private val termRaw = mutable.LinkedHashMap.empty[(BlockCase, Vector[RawId]), RawId]

    private def captures(block: BlockCase): Vector[Entry] = block match {
      case BlockCase.Function(id) => constraints.callables(Callable.Function(id)).captures
      case BlockCase.Implementation(id) => constraints.implementations(id).captures
      case BlockCase.Open => Vector.empty
    }

    private def resolve(entry: Entry, environment: Map[Callable, Vector[RawId]]): RawId = entry match {
      case Entry.Known(block) => term(block, environment)
      case Entry.Project(projection @ Projection(owner, position)) =>
        environment.get(owner).flatMap(_.lift(position))
          .orElse(genericRaw.get(projection))
          .getOrElse(openNode)
      case Entry.Open => openNode
    }

    private def term(block: BlockCase, environment: Map[Callable, Vector[RawId]]): RawId = block match {
      case BlockCase.Open => openNode
      case block =>
        val captured = captures(block).map(resolve(_, environment))
        termRaw.getOrElseUpdate(block -> captured, {
          val id = fresh()
          nodes(id).cases += RawCase(block, captured)
          id
        })
    }

    private def defineEquations(): Unit = {
      orderedVariants.foreach { case key @ (owner, vector) =>
        val exact = variantRaw(key)
        val closed = vector.zipWithIndex.map {
          case (BlockCase.Open, _) => openNode
          case (_, position) => genericRaw(Projection(owner, position))
        }
        val exactEnvironment = Map(owner -> exact)
        val closedEnvironment = Map(owner -> closed)

        vector.indices.foreach { position =>
          // Exact nodes retain call-entry correlations for ordinary
          // specialization. Projection nodes describe the regular closure
          // family at a formal position: captured projections therefore point
          // back to that family rather than to one finite unfolding. An opaque
          // entry is not part of the closed family, but remains opaque when it
          // occurs in the environment of a known alternative.
          nodes(exact(position)).includes += term(vector(position), exactEnvironment)
          vector(position) match {
            case BlockCase.Open => ()
            case block =>
              nodes(genericRaw(Projection(owner, position))).includes +=
                term(block, closedEnvironment)
          }
        }

        owner match {
          case Callable.Function(id) =>
            val captureArity = constraints.callables(owner).captures.size
            val captured = exact.take(captureArity)
            termRaw.getOrElseUpdate(BlockCase.Function(id) -> captured, {
              val node = fresh()
              nodes(node).cases += RawCase(BlockCase.Function(id), captured)
              node
            })
          case Callable.Operation(implementation, _) =>
            val info = constraints.implementations(implementation)
            val captured = info.captures.map(resolve(_, exactEnvironment))
            termRaw.getOrElseUpdate(BlockCase.Implementation(implementation) -> captured, {
              val node = fresh()
              nodes(node).cases += RawCase(BlockCase.Implementation(implementation), captured)
              node
            })
        }
      }

      constraints.directUnboxes.forEach { (box, _) =>
        boxedExpressionRaw.put(box, resolve(constraints.boxes.get(box), Map.empty))
      }

      constraints.boxFlows.foreach { flow =>
        val source = flow.source match {
          case BoxSource.Block(box, entry) =>
            // The expression is represented at the set of its destination,
            // not merely at the singleton set of the constructor it creates.
            // This gives all branches of a value-producing statement one
            // nominal result type.
            if lowerableBoxes(flow.destination) then
              boxedExpressionRaw.put(box, boxRaw(flow.destination))
            resolve(entry, Map.empty)
          case BoxSource.Value(id) => boxRaw.getOrElse(id, openNode)
          case BoxSource.Open => openNode
        }
        nodes(boxRaw(flow.destination)).includes += source
      }
    }

    private def closeIncludes(): Unit = {
      var changed = true
      while changed do {
        changed = false
        nodes.indices.foreach { id =>
          val node = nodes(id)
          node.includes.toVector.foreach { included =>
            val other = nodes(included)
            val oldCases = node.cases.size
            val oldIncludes = node.includes.size
            val oldOpen = node.open
            node.cases ++= other.cases
            node.includes ++= other.includes
            node.open ||= other.open
            changed ||= oldCases != node.cases.size ||
              oldIncludes != node.includes.size || oldOpen != node.open
          }
        }
      }
    }

    private def blockKey(block: BlockCase): String = block match {
      case BlockCase.Function(id) => s"f:${id.name.name}:${id.id}"
      case BlockCase.Implementation(id) => s"i:${id.name.name}:${id.id}"
      case BlockCase.Open => "?"
    }

    private def quotient(): (Vector[Int], Map[LambdaSetId, LambdaSet]) = {
      var colors = Vector.fill(nodes.size)(0)
      var stable = false
      while !stable do {
        val signatures = nodes.indices.map { id =>
          val node = nodes(id)
          val cases = node.cases.toVector.map { c =>
            blockKey(c.block) -> c.captures.map(colors)
          }.sortBy { case (block, captures) => block + captures.mkString("[", ",", "]") }
          (node.open, cases)
        }.toVector
        val palette = signatures.distinct.sortBy(_.toString).zipWithIndex.toMap
        val next = signatures.map(palette)
        stable = next == colors
        colors = next
      }

      val sets = nodes.indices.groupBy(colors).map { case (color, members) =>
        val cases = members.iterator.flatMap(nodes(_).cases).map { c =>
          LambdaCase(c.block, c.captures.map(raw => LambdaSetId(colors(raw))))
        }.toSet
        LambdaSetId(color) -> LambdaSet(cases, members.exists(nodes(_).open))
      }
      colors -> sets
    }

    def result(): LambdaGraph = {
      defineEquations()
      closeIncludes()
      val (colors, sets) = quotient()
      def canonical(raw: RawId): LambdaSetId = LambdaSetId(colors(raw))

      val instances = termRaw.map { case ((block, captures), raw) =>
        (block -> captures.map(canonical)) -> canonical(raw)
      }.toMap
      val projections = genericRaw.map { case (projection, raw) =>
        projection -> canonical(raw)
      }
      val boxes = boxRaw.collect {
        case (id, raw) if lowerableBoxes(id) => id -> canonical(raw)
      }
      val boxedExpressions = new IdentityHashMap[Expr.Box, LambdaSetId]
      boxedExpressionRaw.forEach { (box, raw) => boxedExpressions.put(box, canonical(raw)) }
      LambdaGraph(sets, instances, projections, boxes, boxedExpressions)
    }
  }

  private class Specializer(
    module: ModuleDecl,
    constraints: Constraints,
    graph: LambdaGraph
  )(using Context) {

    private val emptyContext = Specialization(Map.empty)

    private def sequence[A](values: Vector[Option[A]]): Option[Vector[A]] =
      Option.when(values.forall(_.isDefined))(values.flatten)

    /** Greatest fixed point: a set is erasable when it has exactly one case,
      * the case captures no runtime values, and all captured block sets are
      * themselves erasable. Cycles containing only such cases are harmless.
      */
    private val erasableSets: Set[LambdaSetId] = {
      var result = graph.sets.keySet.filter(id => {
        val set = graph(id)
        !set.open && set.cases.size == 1
      })
      var changed = true
      while changed do {
        val next = result.filter { id =>
          val LambdaCase(block, captures) = graph(id).cases.head
          val noValues = block match {
            case BlockCase.Function(function) => constraints.functions(function).valueClosed
            case BlockCase.Implementation(implementation) =>
              constraints.implementations(implementation).valueCaptures.isEmpty
            case BlockCase.Open => false
          }
          noValues && captures.forall(result)
        }
        changed = next != result
        result = next
      }
      result
    }

    private def erasable(id: LambdaSetId): Boolean = erasableSets(id)

    private def storable(tpe: ValueType): Boolean = tpe match {
      case ValueType.Var(_) => false // ordinary monomorphization runs first
      case ValueType.Data(_, arguments) => arguments.forall(storable)
      case ValueType.Boxed(_, _) => false
    }

    /** Closed block sets whose complete environments can be represented by
      * ordinary nominal values. Opaque boxed captures remain in the original
      * capture-aware representation.
      */
    private val lowerableSets: Set[LambdaSetId] = {
      var result = graph.sets.keySet.filter(id => {
        val set = graph(id)
        !set.open && set.cases.nonEmpty && set.cases.forall {
          case LambdaCase(BlockCase.Function(function), _) =>
            constraints.functions(function).valueCaptures.forall(p => storable(p.tpe))
          case LambdaCase(BlockCase.Implementation(implementation), _) =>
            constraints.implementations(implementation).valueCaptures.forall(p => storable(p.tpe))
          case LambdaCase(BlockCase.Open, _) => false
        }
      })
      var changed = true
      while changed do {
        val next = result.filter(id => graph(id).cases.forall(_.captures.forall(result)))
        changed = next != result
        result = next
      }
      result
    }

    private def lowerable(id: LambdaSetId): Boolean = lowerableSets(id)

    /** A boxed closure has one value representation. Even a singleton set is
      * therefore represented nominally rather than by a possibly multi-field
      * unboxed environment product. */
    private val boxedSets: Set[LambdaSetId] = {
      val result = mutable.Set.from(graph.boxes.values)
      graph.boxedExpressions.forEach((_, set) => result += set)
      result.toSet
    }

    /** A singleton closure is a product of its captured values and captured
      * block environments. This least fixed point selects exactly the finite
      * products obtained by expanding singleton captures; sums stop expansion,
      * while cycles retain a nominal representation.
      */
    private val unboxedSets: Set[LambdaSetId] = {
      val candidates = lowerableSets.filter { id =>
        !boxedSets(id) &&
        !erasable(id) && singleton(id).exists {
          case LambdaCase(BlockCase.Function(_), _) => true
          case _ => false
        }
      }
      var result = Set.empty[LambdaSetId]
      var changed = true
      while changed do {
        val next = result ++ candidates.filter { id =>
          singleton(id).exists(_.captures.forall { capture =>
            erasable(capture) || !candidates(capture) || result(capture)
          })
        }
        changed = next != result
        result = next
      }
      result
    }

    private def unboxed(id: LambdaSetId): Boolean = unboxedSets(id)

    private def selectable(id: LambdaSetId): Boolean = erasable(id) || lowerable(id)

    /** Nodes on a capture cycle denote regular, potentially unbounded closure
      * families. They must be represented nominally instead of specialized at
      * successively deeper finite unfoldings. */
    private val recursiveSets: Set[LambdaSetId] = graph.projections.values.toSet.filter { root =>
      val seen = mutable.Set.empty[LambdaSetId]
      val pending = mutable.Stack.from(graph(root).cases.flatMap(_.captures))
      var recursive = false
      while pending.nonEmpty && !recursive do {
        val current = pending.pop()
        if current == root then recursive = true
        else if seen.add(current) then
          pending.pushAll(graph(current).cases.flatMap(_.captures))
      }
      recursive
    }

    /** Select exact acyclic sets, but close recursive demands at the formal
      * projection that defines their shared nominal family. */
    private def select(owner: Callable, position: Int, actual: Option[LambdaSetId]): Option[LambdaSetId] =
      actual.filter(selectable).map { set =>
        graph.projections.get(Projection(owner, position)) match {
          case Some(family)
              if recursiveSets(family) &&
                selectable(family) &&
                graph(set).cases.subsetOf(graph(family).cases) => family
          case _ => set
        }
      }

    private def singleton(id: LambdaSetId): Option[LambdaCase] = {
      val set = graph(id)
      Option.when(!set.open && set.cases.size == 1)(set.cases.head)
    }

    private def set(id: Id)(using context: Specialization): Option[LambdaSetId] =
      context.blocks.get(id) match {
        case Some(selection) => selection.lambdaSet
        case None if constraints.functions.contains(id) =>
          val owner = Callable.Function(id)
          sequence(constraints.callables(owner).captureIds.map(set))
            .flatMap(graph.instance(BlockCase.Function(id), _))
        case None if constraints.implementations.contains(id) =>
          val info = constraints.implementations(id)
          sequence(info.captureIds.map(set))
            .flatMap(graph.instance(BlockCase.Implementation(id), _))
        case None => None
      }

    private def set(block: Block)(using context: Specialization): Option[LambdaSetId] = block match {
      case BlockVar(id, _, _) => set(id)
      case _: BlockLit | New(_) =>
        Option(constraints.anonymous.get(block)).flatMap {
          case block @ BlockCase.Function(id) =>
            sequence(constraints.callables(Callable.Function(id)).captureIds.map(set))
              .flatMap(graph.instance(block, _))
          case block @ BlockCase.Implementation(id) =>
            sequence(constraints.implementations(id).captureIds.map(set))
              .flatMap(graph.instance(block, _))
          case BlockCase.Open => None
        }
      case Unbox(box @ Expr.Box(_, _)) => Option(graph.boxedExpressions.get(box))
      case Unbox(Expr.ValueVar(id, _: ValueType.Boxed)) => graph.boxes.get(id)
      case Unbox(_) => None
    }

    private def variantName(owner: Callable, shape: Shape): String = {
      val base = owner match {
        case Callable.Function(id) => id.name.name
        case Callable.Operation(implementation, method) =>
          s"${implementation.name.name}_${method.name.name}"
      }
      val suffix = shape.map {
        case Some(set) => setName(set)
        case None => "open"
      }.mkString("_")
      if suffix.isEmpty then s"${base}_specialized" else s"${base}_${suffix}"
    }

    private def setName(id: LambdaSetId): String = {
      def loop(current: LambdaSetId, seen: Set[LambdaSetId], depth: Int): String =
        if seen(current) || depth == 3 then s"ls${current.id}"
        else singleton(current) match {
          case Some(LambdaCase(block, captures)) =>
            val base = block match {
              case BlockCase.Function(id) => id.name.name
              case BlockCase.Implementation(id) => id.name.name
              case BlockCase.Open => "open"
            }
            if captures.isEmpty then base
            else base + captures.map(loop(_, seen + current, depth + 1)).mkString("_", "_", "")
          case None => s"ls${current.id}"
        }
      loop(id, Set.empty, 0)
    }

    private case class Representation(set: LambdaSetId, block: BlockType, data: Id, dispatcher: Id) {
      val tpe: ValueType.Data = ValueType.Data(data, Nil)
    }

    private val representations = mutable.LinkedHashMap.empty[LambdaSetId, Representation]
    private val pendingRepresentations = mutable.Queue.empty[Representation]
    private val constructorNames = mutable.LinkedHashMap.empty[(Representation, LambdaCase), Id]

    private def representation(set: LambdaSetId, block: BlockType): Representation = {
      assert(lowerable(set))
      representations.get(set) match {
        case Some(result) if result.block == block => result
        case Some(_) => Context.abort(pretty"One lambda set cannot represent different block types")
        case None =>
          val name = setName(set)
          val result = Representation(set, block, Id(s"Closure_${name}"), Id(s"apply_${name}"))
          representations(set) = result
          pendingRepresentations.enqueue(result)
          result
      }
    }

    private def environmentTypes(set: LambdaSetId): List[ValueType] =
      singleton(set).toList.flatMap {
        case LambdaCase(BlockCase.Function(id), captures) =>
          val definition = constraints.functions(id)
          val info = constraints.callables(Callable.Function(id))
          definition.valueCaptures.map(_.tpe).toList ++
            (info.captureIds zip captures).flatMap { case (capture, capturedSet) =>
              val (tpe, _) = definition.literal.free.blocks(capture)
              runtimeTypes(capturedSet, tpe)
            }
        case _ => Nil
      }

    private def runtimeTypes(set: LambdaSetId, block: BlockType): List[ValueType] =
      if erasable(set) then Nil
      else block match {
        case function: BlockType.Function if unboxed(set) => environmentTypes(set)
        case _ => List(representation(set, block).tpe)
      }

    private def orderedCases(set: LambdaSetId): Vector[LambdaCase] =
      graph(set).cases.toVector.sortBy { c =>
        val label = c.block match {
          case BlockCase.Function(id) => s"f:${id.name.name}:${id.id}"
          case BlockCase.Implementation(id) => s"i:${id.name.name}:${id.id}"
          case BlockCase.Open => "?"
        }
        label + c.captures.map(_.id).mkString("[", ",", "]")
      }

    private def constructor(rep: Representation, lambda: LambdaCase): Id =
      constructorNames.getOrElseUpdate(rep -> lambda, {
        val label = lambda.block match {
          case BlockCase.Function(id) => id.name.name
          case BlockCase.Implementation(id) => id.name.name
          case BlockCase.Open => "open"
        }
        val alternative = orderedCases(rep.set).indexOf(lambda)
        Id(s"${label}_${rep.set.id}_${alternative}")
      })

    /** Specialization follows demands discovered while rewriting workers. This
      * is the operational counterpart of the paper's reverse dependency
      * specialization loop.
      */
    private val variants = mutable.LinkedHashMap.empty[(Callable, Shape), Variant]
    private val pending = mutable.Queue.empty[Variant]

    private def request(owner: Callable, shape: Shape): Id = owner match {
      case Callable.Function(id)
          if constraints.functions(id).topLevel && shape.forall(_.isEmpty) => id
      case _ =>
        variants.getOrElseUpdate(owner -> shape, {
          val variant = Variant(owner, shape, Id(variantName(owner, shape)))
          pending.enqueue(variant)
          variant
        }).id
    }

    private def shape(owner: Callable, arguments: List[Block])(using context: Specialization): Shape = {
      val info = constraints.callables(owner)
      val blocks = info.captureIds.map(set) ++ arguments.map(set)
      blocks.zipWithIndex.map { case (actual, position) =>
        select(owner, position, actual)
      }
    }

    private def shape(callee: LambdaSetId, arguments: List[Block])(using Specialization): Shape = {
      val LambdaCase(block, captures) = singleton(callee).getOrElse {
        Context.abort(pretty"Only a singleton lambda set can be called directly")
      }
      val owner = block match {
        case BlockCase.Function(id) => Callable.Function(id)
        case BlockCase.Implementation(_) | BlockCase.Open =>
          Context.abort(pretty"Only functions can be applied")
      }
      val info = constraints.callables(owner)
      if captures.size != info.captures.size then
        Context.abort(pretty"Wrong capture arity for '${show(owner)}'")
      val blocks = captures.map(Some.apply) ++ arguments.map(set)
      blocks.zipWithIndex.map { case (actual, position) =>
        select(owner, position, actual)
      }
    }

    private def explicitShape(owner: Callable, shape: Shape): Vector[Option[LambdaSetId]] = {
      val info = constraints.callables(owner)
      val result = shape.drop(info.captures.size)
      if result.size != info.explicitParameters then
        Context.abort(pretty"Wrong specialization arity for '${show(owner)}'")
      result
    }

    private def residual[A](owner: Callable, shape: Shape, values: List[A]): List[A] = {
      val choices = explicitShape(owner, shape)
      if choices.size != values.size then
        Context.abort(pretty"Wrong explicit specialization arity for '${show(owner)}'")
      (choices zip values).collect { case (None, value) => value }.toList
    }

    private case class SelectedBlock(
      id: Id,
      tpe: BlockType,
      set: LambdaSetId,
      parameters: List[ValueParam]
    )

    private def selectedBlocks(owner: Callable, shape: Shape, literal: BlockLit): List[SelectedBlock] = {
      val info = constraints.callables(owner)
      val captured = info.captureIds.map { id =>
        val (tpe, _) = literal.free.blocks(id)
        id -> tpe
      }
      val explicit = literal.bparams.map(parameter => parameter.id -> parameter.tpe)
      ((captured ++ explicit) zip shape).collect { case ((id, tpe), Some(set)) =>
        val parameters = runtimeTypes(set, tpe).zipWithIndex.map { case (valueType, index) =>
          ValueParam(Id(s"${id.name.name}_environment${index}"), valueType)
        }
        SelectedBlock(id, tpe, set, parameters)
      }.toList
    }

    private def specializationContext(
      owner: Callable,
      shape: Shape,
      literal: BlockLit,
      selected: List[SelectedBlock]
    ): Specialization = {
      val info = constraints.callables(owner)
      val ids = info.captureIds ++ literal.bparams.map(_.id)
      if ids.size != shape.size then
        Context.abort(pretty"Wrong specialization arity for '${show(owner)}': expected ${ids.size}, found ${shape.size}")
      val selectedById = selected.map(block => block.id -> block).toMap
      val selections = (ids zip shape).map {
        case (id, Some(set)) =>
          val block = selectedById.getOrElse(id,
            Context.abort(pretty"A captured non-static closure cannot be lifted without an environment"))
          val representation = block.parameters.map(p => Expr.ValueVar(p.id, p.tpe))
          id -> Selection.Known(set, representation)
        case (id, None) => id -> Selection.Dynamic
      }
      Specialization(selections.toMap)
    }

    private def operation(owner: Callable.Operation): Operation = {
      val implementation = constraints.implementations(owner.implementation).implementation
      implementation.operations.find(_.name == owner.method).getOrElse {
        Context.abort(pretty"Missing operation '${show(owner)}'")
      }
    }

    private def originalLiteral(owner: Callable): BlockLit = owner match {
      case Callable.Function(id) => constraints.functions(id).literal
      case operation: Callable.Operation =>
        val op = this.operation(operation)
        BlockLit(op.tparams, op.cparams, op.vparams, op.bparams, op.body)
    }

    private def valueCaptures(owner: Callable): Vector[ValueParam] = owner match {
      case Callable.Function(id) => constraints.functions(id).valueCaptures
      case Callable.Operation(implementation, _) =>
        constraints.implementations(implementation).valueCaptures
    }

    private def originalType(owner: Callable): BlockType.Function =
      originalLiteral(owner).tpe.asInstanceOf[BlockType.Function]

    private def originalCapture(owner: Callable): Captures = owner match {
      case Callable.Function(id) => constraints.functions(id).literal.capt
      case operation: Callable.Operation => this.operation(operation).capt
    }

    private val setCaptures: Map[LambdaSetId, Captures] = {
      val result = mutable.Map.from(erasableSets.map(_ -> Set.empty[Capture]))
      var changed = true
      while changed do {
        changed = false
        erasableSets.foreach { set =>
          val LambdaCase(block, captures) = graph(set).cases.head
          val current = block match {
            case BlockCase.Function(id) =>
              val info = constraints.callables(Callable.Function(id))
              val substitution = DB.from(info.captureIds zip captures.map(result))
              Type.substitute(constraints.functions(id).literal.capt, substitution)
            case BlockCase.Implementation(id) =>
              val info = constraints.implementations(id)
              val substitution = DB.from(info.captureIds zip captures.map(result))
              Type.substitute(info.implementation.capt, substitution)
            case BlockCase.Open => Set.empty
          }
          if current != result(set) then {
            result(set) = current
            changed = true
          }
        }
      }
      result.toMap
    }

    private def selectedCapture(set: LambdaSetId): Captures =
      if erasable(set) then setCaptures(set) else Set.empty

    /** The formal block types determine the representation at both ends of a
      * specialized call. The actual argument can use alpha-renamed capture
      * parameters, so its syntactic type is not a representation key. */
    private def specializedBlockTypes(owner: Callable, shape: Shape): List[BlockType] =
      originalType(owner) match {
        case BlockType.Function(_, cparams, _, bparams, _) =>
          val choices = explicitShape(owner, shape)
          val substitution = DB.from(
            (cparams zip choices).collect {
              case (capture, Some(set)) => capture -> selectedCapture(set)
            })
          bparams.map(Type.substitute(_, DB.empty, substitution))
      }

    private def specializedType(owner: Callable, shape: Shape): BlockType.Function =
      originalType(owner) match {
        case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
          val choices = explicitShape(owner, shape)
          if cparams.size != choices.size || bparams.size != choices.size then
            Context.abort(pretty"Capture and block parameter arities disagree for '${show(owner)}'")

          val captureSubstitution = DB.from(
            (cparams zip choices).collect {
              case (capture, Some(set)) => capture -> selectedCapture(set)
            })
          val noTypes = DB.empty[ValueType]
          val rewrittenBlocks = specializedBlockTypes(owner, shape)
          val literal = originalLiteral(owner)
          val info = constraints.callables(owner)
          val capturedBlocks = info.captureIds.map(id => literal.free.blocks(id)._1)
          val lowered = (shape zip (capturedBlocks ++ rewrittenBlocks)).flatMap {
            case (Some(set), block) => runtimeTypes(set, block)
            case (None, _) => Nil
          }
          val capturedValues = valueCaptures(owner).map(_.tpe)
          BlockType.Function(
            tparams,
            residual(owner, shape, cparams),
            (vparams ++ capturedValues ++ lowered)
              .map(Type.substitute(_, noTypes, captureSubstitution)),
            (choices zip rewrittenBlocks).collect { case (None, block) => block }.toList,
            Type.substitute(result, noTypes, captureSubstitution))
      }

    private def functionRef(owner: Callable, shape: Shape, target: Id): BlockVar =
      BlockVar(target, specializedType(owner, shape), specializedCapture(owner, shape))

    private def specializedCapture(owner: Callable, shape: Shape): Captures = {
      val info = constraints.callables(owner)
      val captureSubstitution = DB.from(
        (info.captureIds zip shape.take(info.captures.size)).collect {
          case (capture, Some(set)) => capture -> selectedCapture(set)
        })
      Type.substitute(originalCapture(owner), captureSubstitution)
    }

    private def representative(set: LambdaSetId): Block = {
      val LambdaCase(block, captures) = singleton(set).getOrElse {
        Context.abort(pretty"Cannot statically represent a non-singleton lambda set")
      }
      block match {
      case BlockCase.Function(id) =>
        val owner = Callable.Function(id)
        val info = constraints.callables(owner)
        val shape = captures.map(Some.apply) ++ Vector.fill(info.explicitParameters)(None)
        functionRef(owner, shape, request(owner, shape))

      case BlockCase.Implementation(id) if constraints.implementations(id).topLevel =>
        val definition = constraints.implementations(id).implementation
        BlockVar(id, definition.tpe, definition.capt)

      case BlockCase.Implementation(id) =>
        New(constraints.implementations(id).implementation)

      case BlockCase.Open => Context.abort(pretty"Cannot represent an open block")
      }
    }

    private def selection(id: Id)(using context: Specialization): Option[Selection] =
      context.blocks.get(id)

    private def lexicalEnvironment(lambda: LambdaCase)(using context: Specialization): List[Expr] =
      lambda.block match {
        case BlockCase.Function(id) =>
          val definition = constraints.functions(id)
          val values = definition.valueCaptures.map { parameter =>
            bodyRewriter.rewrite(Expr.ValueVar(parameter.id, parameter.tpe))
          }
          val info = constraints.callables(Callable.Function(id))
          val blocks = (info.captureIds zip lambda.captures).flatMap { case (capture, set) =>
            val (tpe, capt) = definition.literal.free.blocks(capture)
            runtimeArguments(BlockVar(capture, tpe, capt), set, tpe)
          }
          (values ++ blocks).toList

        case BlockCase.Implementation(id) =>
          val definition = constraints.implementations(id)
          val values = definition.valueCaptures.map { parameter =>
            bodyRewriter.rewrite(Expr.ValueVar(parameter.id, parameter.tpe))
          }
          val blocks = (definition.captureIds zip lambda.captures).flatMap { case (capture, set) =>
            val (tpe, capt) = definition.implementation.free.blocks(capture)
            runtimeArguments(BlockVar(capture, tpe, capt), set, tpe)
          }
          (values ++ blocks).toList

        case BlockCase.Open => Context.abort(pretty"An open block has no finite environment")
      }

    private def environment(
      block: Block,
      set: LambdaSetId
    )(using context: Specialization): List[Expr] = block match {
      case BlockVar(id, _, _) => selection(id) match {
        case Some(Selection.Known(actual, values)) if actual == set && unboxed(actual) => values
        case Some(Selection.Known(actual, Nil)) if actual == set => Nil
        case Some(Selection.Known(actual, _)) if actual == set =>
          Context.abort(pretty"A nominal closure environment cannot be projected statically")
        case _ => lexicalEnvironment(singleton(set).getOrElse(
          Context.abort(pretty"A block environment must denote one lambda case")))
      }
      case _ => lexicalEnvironment(singleton(set).getOrElse(
        Context.abort(pretty"A block environment must denote one lambda case")))
    }

    private def runtimeArguments(
      block: Block,
      expected: LambdaSetId,
      tpe: BlockType
    )(using context: Specialization): List[Expr] =
      if erasable(expected) then Nil
      else tpe match {
        case function: BlockType.Function if unboxed(expected) => environment(block, expected)
        case _ => List(closureExpression(block, expected, tpe))
      }

    private def closureExpression(
      block: Block,
      expected: LambdaSetId,
      blockType: BlockType
    )(using context: Specialization): Expr = block match {
      case BlockVar(id, _, _) => selection(id) match {
        case Some(Selection.Known(set, value :: Nil)) if set == expected && !unboxed(set) => value
        case _ => constructClosure(block, expected, blockType)
      }
      case Unbox(expr) => bodyRewriter.rewrite(expr)
      case _: BlockLit | New(_) => constructClosure(block, expected, blockType)
    }

    private def constructClosure(
      block: Block,
      expected: LambdaSetId,
      blockType: BlockType
    )(using context: Specialization): Expr = {
      val actual = set(block).getOrElse(
        Context.abort(pretty"Cannot construct a closure for an open block"))
      val lambda = singleton(actual).getOrElse(
        Context.abort(pretty"A concrete closure construction must denote one lambda case"))
      if !graph(expected).cases(lambda) then
        Context.abort(pretty"Lambda case is not contained in its target lambda set")

      val rep = representation(expected, blockType)
      Expr.Make(rep.tpe, constructor(rep, lambda), Nil, environment(block, actual))
    }

    private def loweredArguments(
      owner: Callable,
      shape: Shape,
      arguments: List[Block]
    )(using Specialization): List[Expr] = {
      val choices = explicitShape(owner, shape)
      (choices zip arguments zip specializedBlockTypes(owner, shape)).flatMap {
        case ((Some(set), argument), tpe) => runtimeArguments(argument, set, tpe)
        case _ => Nil
      }.toList
    }

    private def dispatcherRef(rep: Representation): BlockVar = rep.block match {
      case BlockType.Function(_, cparams, vparams, bparams, result) =>
        BlockVar(
          rep.dispatcher,
          BlockType.Function(Nil, cparams, rep.tpe :: vparams, bparams, result),
          Set.empty)
      case _: BlockType.Interface =>
        Context.abort(pretty"An implementation is dispatched through one of its operations")
    }

    private case class OperationDispatcher(
      representation: Representation,
      method: Id,
      function: BlockType.Function,
      id: Id
    )

    private val operationDispatchers = mutable.LinkedHashMap.empty[(Representation, Id), OperationDispatcher]
    private val pendingOperationDispatchers = mutable.Queue.empty[OperationDispatcher]

    private def operationDispatcher(
      rep: Representation,
      method: Id,
      function: BlockType.Function
    ): OperationDispatcher =
      operationDispatchers.getOrElseUpdate(rep -> method, {
        val result = OperationDispatcher(
          rep,
          method,
          function,
          Id(s"${method.name.name}_${setName(rep.set)}"))
        pendingOperationDispatchers.enqueue(result)
        result
      })

    private def operationDispatcherRef(dispatcher: OperationDispatcher): BlockVar =
      dispatcher.function match {
        case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
          BlockVar(
            dispatcher.id,
            BlockType.Function(tparams, cparams,
              dispatcher.representation.tpe :: vparams, bparams, result),
            Set.empty)
      }

    private def adapter(
      set: LambdaSetId,
      environment: List[Expr],
      function: BlockType.Function
    ): BlockLit = {
      val BlockType.Function(tparams, cparams, vtypes, btypes, result) = function
      val vparams = vtypes.zipWithIndex.map { case (tpe, index) =>
        ValueParam(Id(s"arg${index}"), tpe)
      }
      val bparams = btypes.zip(cparams).zipWithIndex.map { case ((tpe, capture), index) =>
        BlockParam(Id(s"block${index}"), tpe, Set(capture))
      }
      val values = vparams.map(p => Expr.ValueVar(p.id, p.tpe))
      val blocks = bparams.map(p => BlockVar(p.id, p.tpe, p.capt))
      val body = singleton(set) match {
        case Some(LambdaCase(BlockCase.Function(id), captures)) if erasable(set) || unboxed(set) =>
          val owner = Callable.Function(id)
          val shape = captures.map(Some.apply) ++ Vector.fill(bparams.size)(None)
          val target = request(owner, shape)
          Stmt.App(functionRef(owner, shape, target), Nil, values ++ environment, blocks)

        case _ =>
          val closure = environment match {
            case value :: Nil => value
            case _ => Context.abort(pretty"A nominal closure requires exactly one representation value")
          }
          Stmt.App(dispatcherRef(representation(set, function)), Nil, closure :: values, blocks)
      }
      BlockLit(tparams, cparams, vparams, bparams, body)
    }

    private case class CaseLayout(
      constructor: Id,
      fields: List[Field],
      parameters: List[ValueParam],
      valueSubstitution: DB[Expr],
      capturedBlocks: Map[Id, Selection]
    )

    private val caseLayouts = mutable.LinkedHashMap.empty[(Representation, LambdaCase), CaseLayout]

    private def closureLayout(block: BlockCase): (Vector[ValueParam], Vector[(Id, BlockType)]) = block match {
      case BlockCase.Function(id) =>
        val definition = constraints.functions(id)
        val captures = constraints.callables(Callable.Function(id)).captureIds.map { capture =>
          capture -> definition.literal.free.blocks(capture)._1
        }
        definition.valueCaptures -> captures

      case BlockCase.Implementation(id) =>
        val definition = constraints.implementations(id)
        val captures = definition.captureIds.map { capture =>
          capture -> definition.implementation.free.blocks(capture)._1
        }
        definition.valueCaptures -> captures

      case BlockCase.Open => Context.abort(pretty"An open block has no finite layout")
    }

    private def caseLayout(rep: Representation, lambda: LambdaCase): CaseLayout =
      caseLayouts.getOrElseUpdate(rep -> lambda, {
        val (values, blocks) = closureLayout(lambda.block)
        val valueFields = values.map { original =>
          val field = Id(s"${original.id.name.name}_captured")
          (Field(field, original.tpe), ValueParam(field, original.tpe),
            original.id -> Expr.ValueVar(field, original.tpe))
        }
        val blockLayouts = (blocks zip lambda.captures).map { case ((capture, tpe), set) =>
          val fieldTypes = runtimeTypes(set, tpe)
          val fields = fieldTypes.zipWithIndex.map { case (fieldType, index) =>
            val suffix = if fieldTypes.size == 1 then "" else s"_${index}"
            val field = Id(s"${capture.name.name}_captured${suffix}")
            (Field(field, fieldType), ValueParam(field, fieldType))
          }
          val selection = Selection.Known(set, fields.map { case (_, parameter) =>
            Expr.ValueVar(parameter.id, parameter.tpe)
          }.toList)
          (fields, capture -> selection)
        }
        val blockFields = blockLayouts.flatMap(_._1)
        CaseLayout(
          constructor(rep, lambda),
          (valueFields.map(_._1) ++ blockFields.map(_._1)).toList,
          (valueFields.map(_._2) ++ blockFields.map(_._2)).toList,
          DB.from(valueFields.map(_._3)),
          blockLayouts.map(_._2).toMap)
      })

    private object bodyRewriter extends Tree.RewriteWithContext[Specialization] {

      override def rewrite(block: BlockLit)(using context: Specialization): BlockLit =
        super.rewrite(block)

      override def rewrite(block: BlockVar)(using context: Specialization): BlockVar =
        super.rewrite(block)

      override def rewrite(block: Block)(using context: Specialization): Block = block match {
        case unbox @ Unbox(expr) => (set(unbox).filter(lowerable), unbox.tpe) match {
          case (Some(set), function: BlockType.Function) =>
            val environment = Option.when(!erasable(set))(rewrite(expr)).toList
            adapter(set, environment, function)
          case _ => super.rewrite(block)
        }
        case other => super.rewrite(other)
      }

      override def rewrite(expr: Expr)(using context: Specialization): Expr = expr match {
        case value @ Expr.ValueVar(id, ValueType.Boxed(block, _)) =>
          graph.boxes.get(id).filter(lowerable) match {
            case Some(set) => Expr.ValueVar(id, representation(set, block).tpe)
            case None => super.rewrite(value)
          }

        case box @ Expr.Box(block, _) =>
          Option(graph.boxedExpressions.get(box)).filter(lowerable) match {
            case Some(set) => closureExpression(block, set, block.tpe)
            case None => super.rewrite(box)
          }

        case other => super.rewrite(other)
      }

      override def rewrite(stmt: Stmt)(using context: Specialization): Stmt = stmt match {
        case Stmt.App(callee, targs, vargs, bargs) =>
          set(callee).filter(set => erasable(set) || unboxed(set)) match {
            case Some(calleeSet) => singleton(calleeSet) match {
              case Some(LambdaCase(BlockCase.Function(id), _))
                  if constraints.callables.contains(Callable.Function(id)) =>
                val owner = Callable.Function(id)
                val specialization = shape(calleeSet, bargs)
                val target = request(owner, specialization)
                Stmt.App(
                  functionRef(owner, specialization, target),
                  targs.map(rewrite),
                  vargs.map(rewrite) ++
                    runtimeArguments(callee, calleeSet, callee.functionType) ++
                    loweredArguments(owner, specialization, bargs),
                  residual(owner, specialization, bargs).map(rewrite))
              case _ => super.rewrite(stmt)
            }
            case None => set(callee).filter(id => lowerable(id) && !erasable(id)) match {
              case Some(set) =>
                val function = callee.functionType
                val rep = representation(set, function)
                Stmt.App(
                  dispatcherRef(rep),
                  Nil,
                  closureExpression(callee, set, function) :: vargs.map(rewrite),
                  bargs.map(rewrite))
              case None => super.rewrite(stmt)
            }
          }

        case Stmt.Invoke(receiver, method, methodType, targs, vargs, bargs) =>
          set(receiver).filter(erasable).flatMap(singleton).map(_.block) match {
            case Some(BlockCase.Implementation(implementation)) =>
              constraints.operations.get(implementation -> method).flatMap { owner =>
                val specialization = shape(owner, bargs)
                Some((owner, specialization, request(owner, specialization)))
              } match {
                case Some((owner, specialization, target)) =>
                  Stmt.App(
                    functionRef(owner, specialization, target),
                    targs.map(rewrite),
                    vargs.map(rewrite) ++ loweredArguments(owner, specialization, bargs),
                    residual(owner, specialization, bargs).map(rewrite))
                case None => super.rewrite(stmt)
              }
            case _ => set(receiver).filter(set => lowerable(set) && !erasable(set)) match {
              case Some(set) => methodType match {
                case function: BlockType.Function =>
                  val rep = representation(set, receiver.tpe)
                  val dispatcher = operationDispatcher(rep, method, function)
                  Stmt.App(
                    operationDispatcherRef(dispatcher),
                    targs.map(rewrite),
                    closureExpression(receiver, set, receiver.tpe) :: vargs.map(rewrite),
                    bargs.map(rewrite))
                case _: BlockType.Interface =>
                  Context.abort(pretty"An operation must have a function type")
              }
              case None => super.rewrite(stmt)
            }
          }

        case other => super.rewrite(other)
      }
    }

    private def specialize(literal: BlockLit, owner: Callable, shape: Shape): BlockLit = {
      val choices = explicitShape(owner, shape)
      val selected = selectedBlocks(owner, shape, literal)
      val capturedValues = valueCaptures(owner).map { original =>
        original -> ValueParam(Id(s"${original.id.name.name}_captured"), original.tpe)
      }
      val rewritten = bodyRewriter.rewrite(literal)(using
        specializationContext(owner, shape, literal, selected))
      val captureSubstitution = DB.from(
        (literal.cparams zip choices).collect {
          case (capture, Some(set)) => capture -> selectedCapture(set)
        })
      val usedBlocks = rewritten.body.free.blocks.keySet
      val blockSubstitution = DB.from(
        selected.collect {
          case block if erasable(block.set) && usedBlocks(block.id) =>
            block.id -> representative(block.set)
          case block if usedBlocks(block.id) =>
            val environment = block.parameters.map(p => Expr.ValueVar(p.id, p.tpe))
            block.id -> adapter(
              block.set,
              environment,
              block.tpe.asInstanceOf[BlockType.Function])
        })
      val valueSubstitution = DB.from(capturedValues.map { case (original, parameter) =>
        original.id -> Expr.ValueVar(parameter.id, parameter.tpe)
      })
      val substitution = substitutions.Substitution(
        DB.empty,
        captureSubstitution,
        valueSubstitution,
        blockSubstitution)

      BlockLit(
        rewritten.tparams,
        residual(owner, shape, rewritten.cparams),
        (rewritten.vparams ++ capturedValues.map(_._2) ++ selected.flatMap(_.parameters))
          .map(substitutions.substitute(_)(using substitution)),
        residual(owner, shape, rewritten.bparams)
          .map(substitutions.substitute(_)(using substitution)),
        substitutions.substitute(rewritten.body)(using substitution))
    }

    private def original(definition: Toplevel): Toplevel = definition match {
      case Toplevel.Def(id, literal: BlockLit) if constraints.callables.contains(Callable.Function(id)) =>
        val owner = Callable.Function(id)
        val info = constraints.callables(owner)
        val generic = Specialization(
          (info.captureIds ++ literal.bparams.map(_.id)).map(_ -> Selection.Dynamic).toMap)
        Toplevel.Def(id, literal.copy(body = bodyRewriter.rewrite(literal.body)(using generic)))
      case Toplevel.Def(id, block) => Toplevel.Def(id, bodyRewriter.rewrite(block)(using emptyContext))
      case Toplevel.Val(id, binding) => Toplevel.Val(id, bodyRewriter.rewrite(binding)(using emptyContext))
    }

    private def worker(variant: Variant): Option[Toplevel.Def] = variant.owner match {
      case Callable.Function(id) if variant.id != id =>
        val literal = originalLiteral(variant.owner)
        val body = specialize(literal, variant.owner, variant.shape)
        Some(Toplevel.Def(variant.id, Renamer.rename(body)._1))

      case owner: Callable.Operation =>
        val literal = originalLiteral(owner)
        val body = specialize(literal, owner, variant.shape)
        Some(Toplevel.Def(variant.id, Renamer.rename(body)._1))

      case Callable.Function(_) => None
    }

    private case class DispatcherLayout(
      closure: ValueParam,
      values: List[ValueParam],
      blocks: List[BlockParam]
    )

    private val dispatcherLayouts = mutable.LinkedHashMap.empty[(Representation, BlockType.Function), DispatcherLayout]

    private def dispatcherLayout(rep: Representation, function: BlockType.Function): DispatcherLayout =
      dispatcherLayouts.getOrElseUpdate(rep -> function, function match {
        case BlockType.Function(tparams, cparams, vtypes, btypes, _) =>
          if tparams.nonEmpty then
            Context.abort(pretty"Lambda-set lowering expected a monomorphic function")
          val values = vtypes.zipWithIndex.map { case (tpe, index) =>
            ValueParam(Id(s"arg${index}"), tpe)
          }
          val blocks = (btypes zip cparams).zipWithIndex.map { case ((tpe, capture), index) =>
            BlockParam(Id(s"block${index}"), tpe, Set(capture))
          }
          DispatcherLayout(ValueParam(Id("closure"), rep.tpe), values, blocks)
      })

    private def dataDeclaration(rep: Representation): Declaration.Data = {
      val constructors = orderedCases(rep.set).map { lambda =>
        val layout = caseLayout(rep, lambda)
        Constructor(layout.constructor, Nil, layout.fields)
      }.toList
      Declaration.Data(rep.data, Nil, constructors)
    }

    private def dispatcherClause(
      rep: Representation,
      lambda: LambdaCase,
      owner: Callable,
      dispatcher: DispatcherLayout
    ): (Id, BlockLit) = {
      val layout = caseLayout(rep, lambda)
      val literal = originalLiteral(owner)
      val dynamicParameters = literal.bparams.map(p => p.id -> Selection.Dynamic)
      val context = Specialization(layout.capturedBlocks ++ dynamicParameters)
      val rewritten = bodyRewriter.rewrite(literal.body)(using context)

      val valueSubstitution = layout.valueSubstitution ++ DB.from(
        literal.vparams.map(_.id) zip dispatcher.values.map(p => Expr.ValueVar(p.id, p.tpe)))
      val captureSubstitution = DB.from(
        literal.cparams zip dispatcher.blocks.map(_.capt))
      val parameterBlocks = DB.from(
        literal.bparams.map(_.id) zip dispatcher.blocks.map(p => BlockVar(p.id, p.tpe, p.capt)))
      val usedBlocks = rewritten.free.blocks.keySet
      val capturedBlocks = DB.from(layout.capturedBlocks.flatMap {
        case (capture, selection) if usedBlocks(capture) => literal.free.blocks.get(capture).map { case (tpe, _) =>
          val block = selection match {
            case Selection.Known(set, _) if erasable(set) => representative(set)
            case Selection.Known(set, environment) =>
              tpe match {
                case function: BlockType.Function => adapter(set, environment, function)
                case _: BlockType.Interface =>
                  Context.abort(pretty"A represented implementation must be used through an invocation")
              }
            case Selection.Dynamic =>
              Context.abort(pretty"A closure case cannot have a dynamic captured block")
          }
          capture -> block
        }
        case _ => None
      })
      val substitution = substitutions.Substitution(
        DB.empty,
        captureSubstitution,
        valueSubstitution,
        parameterBlocks ++ capturedBlocks)
      val body = substitutions.substitute(rewritten)(using substitution)
      layout.constructor -> BlockLit(Nil, Nil, layout.parameters, Nil, body)
    }

    private def dispatcherDefinition(rep: Representation): Toplevel.Def = {
      rep.block match {
        case function @ BlockType.Function(_, cparams, _, _, result) =>
          val layout = dispatcherLayout(rep, function)
          val clauses = orderedCases(rep.set).map { lambda =>
            lambda.block match {
              case BlockCase.Function(id) =>
                dispatcherClause(rep, lambda, Callable.Function(id), layout)
              case BlockCase.Implementation(_) | BlockCase.Open =>
                Context.abort(pretty"A function lambda set contains a non-function case")
            }
          }.toList
          val body = Stmt.Match(
            Expr.ValueVar(layout.closure.id, layout.closure.tpe),
            result,
            clauses,
            None)
          Toplevel.Def(
            rep.dispatcher,
            BlockLit(Nil, cparams, layout.closure :: layout.values, layout.blocks, body))

        case _: BlockType.Interface =>
          Context.abort(pretty"An implementation has one dispatcher per operation")
      }
    }

    private def operationDispatcherDefinition(dispatcher: OperationDispatcher): Toplevel.Def = {
      val rep = dispatcher.representation
      val layout = dispatcherLayout(rep, dispatcher.function)
      val result = dispatcher.function.result
      val clauses = orderedCases(rep.set).map { lambda =>
        lambda.block match {
          case BlockCase.Implementation(id) =>
            val owner = constraints.operations.getOrElse(id -> dispatcher.method,
              Context.abort(pretty"Missing operation '${dispatcher.method.name.name}'"))
            dispatcherClause(rep, lambda, owner, layout)
          case BlockCase.Function(_) | BlockCase.Open =>
            Context.abort(pretty"An implementation lambda set contains a non-implementation case")
        }
      }.toList
      val body = Stmt.Match(
        Expr.ValueVar(layout.closure.id, layout.closure.tpe),
        result,
        clauses,
        None)
      dispatcher.function match {
        case BlockType.Function(tparams, cparams, _, _, _) =>
          Toplevel.Def(
            dispatcher.id,
            BlockLit(tparams, cparams, layout.closure :: layout.values, layout.blocks, body))
      }
    }

    def result(): ModuleDecl = {
      val originals = module.definitions.map(original)
      val workers = mutable.ArrayBuffer.empty[Toplevel.Def]
      val declarations = mutable.ArrayBuffer.empty[Declaration.Data]
      while pending.nonEmpty || pendingRepresentations.nonEmpty || pendingOperationDispatchers.nonEmpty do {
        while pending.nonEmpty do worker(pending.dequeue()).foreach(workers += _)
        while pendingRepresentations.nonEmpty do {
          val rep = pendingRepresentations.dequeue()
          declarations += dataDeclaration(rep)
          rep.block match {
            case _: BlockType.Function => workers += dispatcherDefinition(rep)
            case _: BlockType.Interface => ()
          }
        }
        while pendingOperationDispatchers.nonEmpty do
          workers += operationDispatcherDefinition(pendingOperationDispatchers.dequeue())
      }
      module.copy(
        declarations = module.declarations ++ declarations,
        definitions = originals ++ workers
      )
    }
  }

  private class Collector(module: ModuleDecl)(using Context) {
    private val callables = mutable.LinkedHashMap.empty[Callable, CallableInfo]
    private val operations = mutable.LinkedHashMap.empty[(Id, Id), Callable.Operation]
    private val calls = mutable.ListBuffer.empty[Call]
    private val escapes = mutable.ListBuffer.empty[Entry]
    private val functions = mutable.LinkedHashMap.empty[Id, FunctionDefinition]
    private val implementations = mutable.LinkedHashMap.empty[Id, ImplementationDefinition]
    private val anonymous = new IdentityHashMap[Block, BlockCase]
    private val boxFlows = mutable.ListBuffer.empty[BoxFlow]
    private val boxes = new IdentityHashMap[Expr.Box, Entry]
    private val unboxes = mutable.Set.empty[Id]
    private val directUnboxes = new IdentityHashMap[Expr.Box, Unit]
    private val unsafeBoxValues = mutable.Set.empty[Id]
    private val unsafeBoxes = new IdentityHashMap[Expr.Box, Unit]

    private val globalIds: Set[Id] =
      module.definitions.map(_.id).toSet ++
        module.externs.collect { case Extern.Def(id, _, _, _, _, _, _, _, _) => id }

    private var freshIndex = 0
    private def fresh(prefix: String): Id = {
      freshIndex += 1
      Id(s"${prefix}${freshIndex}")
    }

    private val initial: Map[Id, Entry] =
      module.externs.collect {
        case Extern.Def(id, _, _, _, _, _, _, _, _) => id -> Entry.Open
      }.toMap ++ module.definitions.collect {
        case Toplevel.Def(id, _: BlockLit) => id -> Entry.Known(BlockCase.Function(id))
        case Toplevel.Def(id, New(_)) => id -> Entry.Known(BlockCase.Implementation(id))
        case Toplevel.Def(id, Unbox(_)) => id -> Entry.Open
      }

    def result(): Constraints = {
      module.definitions.foreach(toplevel(_, initial))

      module.exports.foreach { id =>
        initial.get(id).foreach {
          case callee @ Entry.Known(BlockCase.Function(function)) =>
            escapes += callee
            callables.get(Callable.Function(function)).foreach { info =>
              calls += Call.Apply(callee, Vector.fill(info.explicitParameters)(Entry.Open))
            }
          case implementation @ Entry.Known(BlockCase.Implementation(_)) =>
            escapes += implementation
          case Entry.Open | Entry.Known(BlockCase.Open) | Entry.Project(_) => ()
        }
      }

      Constraints(
        callables.toMap,
        operations.toMap,
        calls.toList,
        escapes.toList,
        functions.toMap,
        implementations.toMap,
        anonymous,
        boxFlows.toList,
        boxes,
        unboxes.toSet,
        directUnboxes,
        unsafeBoxValues.toSet,
        unsafeBoxes
      )
    }

    private def orderedFreeBlocks(free: Free, self: Option[Id]): List[Id] =
      free.blocks.keySet
        .filterNot(id => globalIds.contains(id) || self.contains(id))
        .toList
        .sortBy(id => (id.name.name, id.id))

    private def orderedFreeValues(free: Free): Vector[ValueParam] =
      free.values.iterator
        .filterNot { case (id, _) => globalIds.contains(id) }
        .toVector
        .sortBy { case (id, _) => (id.name.name, id.id) }
        .map(ValueParam.apply)

    private def defineFunction(id: Id, literal: BlockLit, outer: Map[Id, Entry], topLevel: Boolean = false): Entry = {
      val owner = Callable.Function(id)
      val capturedIds = orderedFreeBlocks(literal.free, Some(id))
      val captured = capturedIds.map(block => outer.getOrElse(block, Entry.Open)).toVector
      val freeValues = orderedFreeValues(literal.free)
      functions(id) = FunctionDefinition(literal, freeValues, topLevel)
      callables(owner) = CallableInfo(capturedIds.toVector, captured, literal.bparams.size)

      val parameters = literal.bparams.zipWithIndex.map { case (parameter, index) =>
        parameter.id -> Entry.Project(Projection(owner, captured.size + index))
      }.toMap
      val local = outer + (id -> Entry.Known(BlockCase.Function(id))) ++ parameters
      statement(literal.body, local)
      Entry.Known(BlockCase.Function(id))
    }

    private def defineImplementation(
      id: Id,
      impl: Implementation,
      outer: Map[Id, Entry],
      topLevel: Boolean = false
    ): Entry = {
      val receiver = Entry.Known(BlockCase.Implementation(id))
      val local = outer + (id -> receiver)
      val freeValues = orderedFreeValues(impl.free)
      val freeBlocks = orderedFreeBlocks(impl.free, Some(id))
      val captures = freeBlocks.map(block => outer.getOrElse(block, Entry.Open)).toVector
      implementations(id) = ImplementationDefinition(
        impl,
        freeBlocks.toVector,
        captures,
        freeValues,
        topLevel
      )

      impl.operations.foreach { operation =>
        val owner: Callable.Operation = Callable.Operation(id, operation.name)
        val capturedIds = orderedFreeBlocks(operation.free, Some(id))
        val captured = capturedIds.map(block => local.getOrElse(block, Entry.Open)).toVector
        callables(owner) = CallableInfo(capturedIds.toVector, captured, operation.bparams.size)
        operations(id -> operation.name) = owner

        val parameters = operation.bparams.zipWithIndex.map { case (parameter, index) =>
          parameter.id -> Entry.Project(Projection(owner, captured.size + index))
        }.toMap
        statement(operation.body, local ++ parameters)
      }
      receiver
    }

    private def define(id: Id, block: Block, env: Map[Id, Entry], topLevel: Boolean = false): Entry = block match {
      case literal: BlockLit => defineFunction(id, literal, env, topLevel)
      case New(impl) => defineImplementation(id, impl, env, topLevel)
      case variable: BlockVar => value(variable, env)
      case Unbox(expr) =>
        unbox(expr, env)
        Entry.Open
    }

    private def toplevel(definition: Toplevel, env: Map[Id, Entry]): Unit = definition match {
      case Toplevel.Def(id, block) => define(id, block, env, topLevel = true)
      case Toplevel.Val(_, binding) => statement(binding, env)
    }

    private def anonymousValue(block: Block, prefix: String)(define: Id => Entry): Entry = {
      Option(anonymous.get(block)).map(Entry.Known.apply).getOrElse {
        val entry = define(fresh(prefix))
        entry match {
          case Entry.Known(blockCase) => anonymous.put(block, blockCase)
          case _ => ()
        }
        entry
      }
    }

    private def value(block: Block, env: Map[Id, Entry]): Entry = block match {
      case BlockVar(id, _, _) => env.getOrElse(id, Entry.Open)
      case literal: BlockLit =>
        anonymousValue(literal, "lambda")(defineFunction(_, literal, env))
      case block @ New(impl) =>
        anonymousValue(block, "implementation")(defineImplementation(_, impl, env))
      case Unbox(expr) =>
        unbox(expr, env)
        Entry.Open
    }

    /** Analyze a statement and, when present, connect its result to a local
      * boxed-value binder. Threading this destination through the fine-grained
      * CBV spine avoids a second traversal and makes the value-flow boundary
      * explicit.
      */
    private def statement(
      stmt: Stmt,
      env: Map[Id, Entry],
      boxedDestination: Option[Id] = None
    ): Unit = stmt match {
      case Stmt.Def(id, binding, body) =>
        val entry = define(id, binding, env)
        statement(body, env + (id -> entry), boxedDestination)

      case Stmt.Let(id, binding, body) =>
        binding.tpe match {
          case _: ValueType.Boxed => boxFlows += BoxFlow(boxSource(binding, env), id)
          case _ => expression(binding, env)
        }
        statement(body, env, boxedDestination)

      case Stmt.ImpureApp(_, callee, _, vargs, bargs, body) =>
        vargs.foreach(expression(_, env))
        calls += Call.Apply(value(callee, env), bargs.map(value(_, env)).toVector)
        statement(body, env, boxedDestination)

      case Stmt.Return(expr) => boxedDestination match {
        case Some(destination) =>
          val source = boxSource(expr, env)
          boxFlows += BoxFlow(source, destination)
          if source == BoxSource.Open then expression(expr, env)
        case None => expression(expr, env)
      }

      case Stmt.Val(id, binding, body) =>
        binding.tpe match {
          case _: ValueType.Boxed => statement(binding, env, Some(id))
          case _ => statement(binding, env)
        }
        statement(body, env, boxedDestination)

      case Stmt.App(callee, _, vargs, bargs) =>
        vargs.foreach(expression(_, env))
        calls += Call.Apply(value(callee, env), bargs.map(value(_, env)).toVector)
        boxedDestination.foreach(destination => boxFlows += BoxFlow(BoxSource.Open, destination))

      case Stmt.Invoke(receiver, method, _, _, vargs, bargs) =>
        vargs.foreach(expression(_, env))
        calls += Call.Invoke(value(receiver, env), method, bargs.map(value(_, env)).toVector)
        boxedDestination.foreach(destination => boxFlows += BoxFlow(BoxSource.Open, destination))

      case Stmt.If(cond, thn, els) =>
        expression(cond, env)
        statement(thn, env, boxedDestination)
        statement(els, env, boxedDestination)

      case Stmt.Match(scrutinee, _, clauses, default) =>
        expression(scrutinee, env)
        clauses.foreach { case (_, clause) => scope(clause, env, boxedDestination) }
        default.foreach(statement(_, env, boxedDestination))

      case Stmt.Region(body) => scope(body, env, boxedDestination)

      case Stmt.Alloc(id, init, _, body) =>
        expression(init, env)
        statement(body, env + (id -> Entry.Open), boxedDestination)

      case Stmt.Var(ref, init, _, body) =>
        expression(init, env)
        statement(body, env + (ref -> Entry.Open), boxedDestination)

      case Stmt.Get(_, _, _, _, body) => statement(body, env, boxedDestination)

      case Stmt.Put(_, _, value, body) =>
        expression(value, env)
        statement(body, env, boxedDestination)

      // A shift can return from the reset without following an ordinary
      // `return` in its body. Without an effect-sensitive control-flow proof,
      // the result of a reset is therefore an open boxed value.
      case Stmt.Reset(body) =>
        scope(body, env, boxedDestination)
        boxedDestination.foreach(destination =>
          boxFlows += BoxFlow(BoxSource.Open, destination))

      case Stmt.Shift(_, continuation, body) =>
        statement(body, env + (continuation.id -> Entry.Open))
        boxedDestination.foreach(destination => boxFlows += BoxFlow(BoxSource.Open, destination))

      case Stmt.Resume(_, body) => statement(body, env, boxedDestination)
      case Stmt.Hole(_, _) =>
        boxedDestination.foreach(destination => boxFlows += BoxFlow(BoxSource.Open, destination))
    }

    /** Analyze an immediately entered structural block without giving it a
      * callable identity. Its block parameters are runtime-provided.
      */
    private def scope(
      literal: BlockLit,
      env: Map[Id, Entry],
      boxedDestination: Option[Id] = None
    ): Unit = {
      val parameters = literal.bparams.map(_.id -> Entry.Open).toMap
      statement(literal.body, env ++ parameters, boxedDestination)
    }

    private def expression(expr: Expr, env: Map[Id, Entry]): Unit = expr match {
      case Expr.ValueVar(id, _: ValueType.Boxed) => unsafeBoxValues += id
      case Expr.ValueVar(_, _) | Expr.Literal(_, _) => ()
      case Expr.PureApp(_, _, vargs) => vargs.foreach(expression(_, env))
      case Expr.Make(_, _, _, vargs) => vargs.foreach(expression(_, env))
      case box @ Expr.Box(block, _) =>
        val entry = value(block, env)
        boxes.put(box, entry)
        unsafeBoxes.put(box, ())
        escapes += entry
    }

    private def unbox(expr: Expr, env: Map[Id, Entry]): Unit = expr match {
      case Expr.ValueVar(id, _: ValueType.Boxed) => unboxes += id
      case box @ Expr.Box(block, _) =>
        boxes.put(box, value(block, env))
        directUnboxes.put(box, ())
      case other => expression(other, env)
    }

    private def boxSource(expr: Expr, env: Map[Id, Entry]): BoxSource = expr match {
      case box @ Expr.Box(block, _) =>
        val entry = value(block, env)
        boxes.put(box, entry)
        escapes += entry
        BoxSource.Block(box, entry)
      case Expr.ValueVar(id, _: ValueType.Boxed) => BoxSource.Value(id)
      case _ => BoxSource.Open
    }
  }
}
