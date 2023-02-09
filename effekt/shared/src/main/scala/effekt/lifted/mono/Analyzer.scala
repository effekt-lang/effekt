package effekt
package lifted
package mono

import scala.collection.immutable.ListMap
import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR, TODO, NOT_SUPPORTED }
import kiama.util.Memoiser
import scala.collection.mutable


class FlowAnalysis(

  // block binders like `def`, lambdas, or block parameters.
  binders: mutable.Map[Id, FlowType] = mutable.Map.empty,

  // every (block-) subtree is annotated with its flow type
  annotations: Memoiser[lifted.Block, FlowType] = Memoiser.makeIdMemoiser(),

  interfaces: mutable.Map[Id, InterfaceDeclaration] = mutable.Map.empty,

  // lexical scoping: maps evidence parameters like ev104 to concrete evidence (e.g., <α17._2, Try, Try>)
  parameters: mutable.Map[Id, Ev] = mutable.Map.empty,

  // collected constraints
  var constraints: List[Constraint] = Nil
) {
  def addDefinition(id: Id, tpe: FlowType): Unit =
    assert(!binders.isDefinedAt(id), s"Cannot add signature for block twice: ${id}")
    binders.update(id, tpe)

  def addInterface(id: Id, decl: InterfaceDeclaration): Unit =
    assert(!interfaces.isDefinedAt(id), s"Cannot add signature for interface twice: ${id}")
    interfaces.update(id, decl)

  def annotateTree(tree: lifted.Block, tpe: FlowType): Unit = annotations.put(tree, tpe)

  def annotatedType(tree: lifted.Block): FlowType =
    annotations.getOrDefault(tree, INTERNAL_ERROR(s"Cannot get signature for tree: ${tree}"))

  def annotatedFunctionType(tree: lifted.Block): FlowType.Function = annotatedType(tree) match {
    case f: FlowType.Function => f
    case i: FlowType.Interface => INTERNAL_ERROR(s"Expected function type, but got: ${i}")
  }

  def annotatedInterfaceType(tree: lifted.Block): FlowType.Interface = annotatedType(tree) match {
    case f: FlowType.Function => INTERNAL_ERROR(s"Expected interface type, but got: ${f}")
    case i: FlowType.Interface => i
  }

  def flowTypeForBinder(term: Id): FlowType =
    binders.getOrElse(term, INTERNAL_ERROR(s"Cannot get flowtype for binder: ${term}"))

  def interfaceDeclarationFor(typeName: Id): InterfaceDeclaration =
    interfaces.getOrElse(typeName, INTERNAL_ERROR(s"Cannot find interface type for ${typeName}"))

  def bindEvidence(id: Id, ev: Ev): Unit =
    parameters.update(id, ev)

  def addConstraint(lower: FlowType, upper: FlowType): Unit =
    constraints = Constraint.B(lower, upper) :: constraints

  def addConstraint(lower: Evidences, upper: Evidences): Unit =
    constraints = Constraint.E(lower, upper) :: constraints

  def evidenceFor(param: Id): Ev =
    parameters.getOrElse(param, INTERNAL_ERROR(s"Cannot find evidence for ${param}"))

}

enum Constraint {
  // α <: [<α17._2, Try, Try>, <>]
  case E(lower: Evidences, upper: Evidences)
  // α() <: [<>]()
  case B(lower: FlowType, upper: FlowType)

  def show: String = this match {
    case E(l, u) => l.show + " <: " + u.show
    case B(l, u) => l.show + " <: " + u.show
  }
}

def analyze(mod: ModuleDecl)(using C: ErrorReporter, F: FlowAnalysis): Unit = {
  mod.decls.foreach(analyze)
  analyzeDefinitions(mod.definitions)

  // TODO replace by an analysis whether evidence is used at all, or not.
  // F.addConstraint(F.flowTypeFor(main), FlowType.Function(Evidences.Concrete(List(Ev(Nil))), Nil))
}

def preanalyze(d: Definition)(using C: ErrorReporter, F: FlowAnalysis): Unit =
  d match {
    case Definition.Def(id, block) => F.addDefinition(id, preanalyze(block))
    case Definition.Let(id, binding) => ()
  }

def traverse(d: Definition)(using C: ErrorReporter, F: FlowAnalysis): Unit =
  d match {
    case Definition.Def(id, block) => traverse(block)
    case Definition.Let(id, binding) => analyze(binding)
  }

def analyzeDefinitions(definitions: List[Definition])(using C: ErrorReporter, F: FlowAnalysis): Unit = {
   definitions.foreach(preanalyze)
   definitions.foreach(traverse)
}

def bindBlockParams(params: List[Param])(using C: ErrorReporter, F: FlowAnalysis): List[FlowType] =
  params.collect { case Param.BlockParam(id, tpe) =>
    val bft = freshFlowType(tpe)
    F.addDefinition(id, bft)
    bft
  }

// analyses the block, but does not go into the body to enable mutual recursion
def preanalyze(b: Block)(using C: ErrorReporter, F: FlowAnalysis): FlowType = b match {
  case Block.BlockVar(id, annotatedType) => F.flowTypeForBinder(id)
  case Block.BlockLit(tparams, params, body) =>
    val BlockType.Function(tparams, _, vparams, bparams, ret) = b.tpe : @unchecked
    // Step 1) Map unification variables to evidence parameters:
    //   ev142 -> α17._2
    val eps = params.collect { case Param.EvidenceParam(id) => id }
    val ev: Evidences.FlowVar = Evidences.fresh(eps.size)
    eps.zipWithIndex.foreach { case (id, idx) =>
      F.bindEvidence(id, Ev(List(Lift.Var(ev, idx))))
    }

    // Step 2) Create flow types for block parameters
    val bfts = bindBlockParams(params)

    FlowType.Function(ev, tparams, vparams, bfts, ret)


  case Block.Member(b, field, annotatedTpe) =>
    analyze(b) match {
      case f: FlowType.Function => C.panic("Should not happen: member selection on function type")
      case FlowType.Interface(id, _) =>
        F.interfaceDeclarationFor(id).operations(field)
    }

  case Block.New(impl) => analyze(impl)

  case Block.Unbox(e) =>
    analyze(e)
    freshFlowType(b.tpe) // TODO not really supported atm
} match { case ftpe => F.annotateTree(b, ftpe); ftpe }

def analyze(impl: Implementation)(using C: ErrorReporter, F: FlowAnalysis): FlowType.Interface = impl match {
  case Implementation(BlockType.Interface(name, targs), operations) =>
    val sig = F.interfaceDeclarationFor(name)
    operations foreach {
      case Operation(name, implementation) =>
        val declaredType = sig.operations(name)
        val implementedType = analyze(implementation)
        F.addConstraint(implementedType, declaredType)
    }
    FlowType.Interface(name, targs)
}

// Actually traverses the block
def traverse(b: Block)(using C: ErrorReporter, F: FlowAnalysis): Unit =
  b match {
    case Block.BlockVar(id, annotatedType) => ()
    case Block.BlockLit(tparams, params, body) => analyze(body)
    case Block.Member(b, field, annotatedTpe) => traverse(b)
    case Block.Unbox(e) => analyze(e)
    case Block.New(impl) => ()  // TODO
  }

def analyze(b: Block)(using C: ErrorReporter, F: FlowAnalysis): FlowType = {
  val tpe = preanalyze(b); traverse(b); tpe
}


def analyze(s: Stmt)(using C: ErrorReporter, F: FlowAnalysis): Unit = s match {
  case Stmt.Scope(definitions, body) =>
    analyzeDefinitions(definitions)
    analyze(body)
  case Stmt.Val(id, binding, body) =>
    analyze(binding)
    analyze(body)
  case Stmt.App(b, targs, args) =>
    // Step 1) fully analyze block
    val FlowType.Function(evs, _, _, bps, _) = analyze(b) : @unchecked

    // Step 2) gather evidence arguments of application
    val evidenceArgs = args.collect {
      case Evidence(scopes) => Ev(scopes.flatMap(e => F.evidenceFor(e).lifts))
    }
    F.addConstraint(evs, Evidences.Concrete(evidenceArgs))

    // visit expression arguments
    args.collect { case e: Expr => analyze(e) }

    // Step 3) infer types of block arguments
    // TODO right now, we skip visiting the expression arguments.
    val blockArgs = args.collect { case b: Block => analyze(b) }

    // Step 4) unify inferred function type with given arguments
    (bps zip blockArgs) foreach {
      case (bp, ba) => F.addConstraint(ba, bp)
    }

  case Stmt.If(cond, thn, els) =>
    analyze(cond); analyze(thn); analyze(els)
  case Stmt.Match(scrutinee, clauses, default) =>
    analyze(scrutinee);
    clauses.foreach {
      case (id, block) => analyze(block)
    }
    default.foreach(analyze)

  // For now, capabilities are treated nominally. If we want to be more precise,
  // we would need to relate the handler here with the capability.
  case Stmt.Try(Block.BlockLit(Nil, ev :: caps, body), handler) =>
    F.bindEvidence(ev.id, Ev(List(Lift.Try())))
    bindBlockParams(caps)

    analyze(body)
    handler.foreach(analyze)

  case Stmt.Return(e) => analyze(e)
  case Stmt.Hole() => ()

  case Stmt.Region(body) => () // TODO
  case Stmt.State(id, init, region, body) => () // TODO
  case _ => INTERNAL_ERROR(s"Not covered: ${s}")
}

def analyze(a: Argument)(using C: ErrorReporter, F: FlowAnalysis): Unit = a match {
  case expr: Expr => analyze(expr)
  case block: Block => analyze(block)
  case e: Evidence => INTERNAL_ERROR("Should not happen since this function is only called on pure applications which do not take evidence.")
}

def analyze(e: Expr)(using C: ErrorReporter, F: FlowAnalysis): Unit = e match {
  case Expr.ValueVar(id, annotatedType) => ()
  case Expr.Literal(value, annotatedType) => ()

  // pure applications don't take evidence, so we do not need to consider the flow here.
  // this also means we do not need to bind externs and constructors (at the moment).
  // TODO what is with control externs?
  case Expr.PureApp(b, targs, args) => args.foreach(analyze)

  case Expr.Select(target, field, annotatedType) => analyze(target)
  case Expr.Box(b) => analyze(b)
  case Expr.Run(s) => analyze(s)
}

def analyze(d: Declaration)(using C: ErrorReporter, F: FlowAnalysis): Unit =
  d match {
    case d: Declaration.Data => ()
    case Declaration.Interface(id, tparams, properties) =>
      val props = properties.map {
        p => (p.id, freshFlowType(p.tpe))
      }
      assert(props.distinct.size == props.size, "duplicate definition of operation")

      F.addInterface(id, InterfaceDeclaration(props.toMap))
  }

def freshFlowType(tpe: BlockType)(using C: ErrorReporter, F: FlowAnalysis): FlowType = tpe match {
  case BlockType.Function(tparams, eparams, vparams, bparams, result) =>
    FlowType.Function(Evidences.fresh(eparams.size), tparams, vparams, bparams.map(freshFlowType), result)
  case BlockType.Interface(name, targs) =>
    FlowType.Interface(name, targs)
}
