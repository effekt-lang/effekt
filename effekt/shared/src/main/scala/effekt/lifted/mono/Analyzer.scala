package effekt
package lifted
package mono

import scala.collection.immutable.ListMap
import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR, TODO, NOT_SUPPORTED }

enum Provenance {
  // e.g. def >>>foo<<<() = ...
  case BlockDefinition(termId: Id)
  // e.g. >>>{ () => ... }<<<
  case BlockTree(tree: lifted.Block)
  // e.g. interface >>>Exc<<< { ... }
  case InterfaceDeclaration(typeId: Id)

  override def equals(obj: Any): Boolean = (this, obj) match {
    case (Provenance.BlockDefinition(id1), Provenance.BlockDefinition(id2)) => id1 == id2
    case (Provenance.InterfaceDeclaration(id1), Provenance.InterfaceDeclaration(id2)) => id1 == id2
    case (Provenance.BlockTree(tree1), Provenance.BlockTree(tree2)) => tree1 eq tree2
    case _ => false
  }

  override def hashCode(): Int = this match {
    case Provenance.BlockDefinition(id) => id.hashCode
    case Provenance.InterfaceDeclaration(id) => id.hashCode
    case Provenance.BlockTree(tree) => System.identityHashCode(tree)
  }
}

class FlowAnalysis(
  // block binders like `def`, lambdas, or block parameters.
  var types: ListMap[Provenance, FlowType] = ListMap.empty,
  //  var signatures: ListMap[Id, FlowType] = ListMap.empty,
  //  var interfaces: ListMap[Id, FlowType.Interface] = ListMap.empty,

  // lexical scoping: maps evidence parameters like ev104 to concrete evidence (e.g., <α17._2, Try, Try>)
  var parameters: ListMap[Id, Ev] = ListMap.empty,
  var constraints: List[Constraint] = Nil
) {
  def addDefinition(id: Id, tpe: FlowType): Unit =
    val key = Provenance.BlockDefinition(id)
    assert(!types.isDefinedAt(key), s"Cannot add signature for block twice: ${id}")
    types += (key -> tpe)

  def addInterface(id: Id, tpe: FlowType.Interface): Unit =
    val key = Provenance.InterfaceDeclaration(id)
    assert(!types.isDefinedAt(key), s"Cannot add signature for interface twice: ${id}")
    types += (key, tpe)

  def annotateTree(tree: lifted.Block, tpe: FlowType): Unit =
    val key = Provenance.BlockTree(tree)
    assert(!types.isDefinedAt(key), s"Cannot add signature for same tree twice: ${tree}")
    types += (key, tpe)

  def annotatedType(tree: lifted.Block): FlowType =
    val key = Provenance.BlockTree(tree)
    assert(types.isDefinedAt(key), s"Cannot get signature for tree: ${tree}")
    types(key)

  def flowTypeForDefinition(term: Id): FlowType =
    val key = Provenance.BlockDefinition(term)
    assert(types.isDefinedAt(key), s"Cannot find flow type for ${term}")
    types(key)

  def interfaceFor(typeName: Id): FlowType.Interface =
    val key = Provenance.InterfaceDeclaration(typeName)
    assert(types.isDefinedAt(key), s"Cannot find interface type for ${typeName}")
    types(key) match {
      case f : FlowType.Function => INTERNAL_ERROR("Not an interface")
      case i : FlowType.Interface => i
    }

  // for debugging only
  def interfaces = types.collect { case (Provenance.InterfaceDeclaration(id), tpe) => id -> tpe }
  def definitions = types.collect { case (Provenance.BlockDefinition(id), tpe) => id -> tpe }
  def trees = types.toList.collect { case (Provenance.BlockTree(tree), tpe) => tree -> tpe }

  def bindEvidence(id: Id, ev: Ev): Unit =
    parameters = parameters + (id -> ev)

  def addConstraint(lower: FlowType, upper: FlowType) =
    constraints = Constraint.B(lower, upper) :: constraints

  def evidenceFor(param: Id): Ev =
    assert(parameters.isDefinedAt(param), s"Cannot find evidence for ${param}")
    parameters(param)
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
  case Block.BlockVar(id, annotatedType) => F.flowTypeForDefinition(id)
  case Block.BlockLit(tparams, params, body) =>
    // Step 1) Map unification variables to evidence parameters:
    //   ev142 -> α17._2
    val eps = params.collect { case Param.EvidenceParam(id) => id }
    val ev: Evidences.FlowVar = Evidences.fresh(eps.size)
    eps.zipWithIndex.foreach { case (id, idx) =>
      F.bindEvidence(id, Ev(List(Lift.Var(ev, idx))))
    }

    // Step 2) Create flow types for block parameters
    val bfts = bindBlockParams(params)

    FlowType.Function(ev, bfts)


  case Block.Member(b, field, annotatedTpe) =>
    analyze(b) match {
      case FlowType.Function(evidences, bparams) => C.panic("Should not happen: member selection on function type")
      case FlowType.Interface(id, operations) => operations(field)
    }

  case Block.New(impl) => analyze(impl)

  case Block.Unbox(e) =>
    analyze(e)
    freshFlowType(b.tpe) // TODO not really supported atm
} match { case ftpe => F.annotateTree(b, ftpe); ftpe }

def analyze(impl: Implementation)(using C: ErrorReporter, F: FlowAnalysis): FlowType.Interface = impl match {
  case Implementation(BlockType.Interface(name, targs), operations) =>
    val sig = F.interfaceFor(name)
    operations foreach {
      case Operation(name, implementation) =>
        val declaredType = sig.operations(name)
        val implementedType = analyze(implementation)
        F.addConstraint(implementedType, declaredType)
    }
    sig
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
    val fun = analyze(b)

    // Step 2) gather evidence arguments of application
    val evidenceArgs = args.collect {
      case Evidence(scopes) => Ev(scopes.flatMap(e => F.evidenceFor(e).lifts))
    }

    // Step 3) infer types of block arguments
    val blockArgs = args.collect {
      case b: Block => analyze(b)
    }
    // Step 4) unify inferred function type with given arguments
    val arg = FlowType.Function(Evidences.Concrete(evidenceArgs), blockArgs)
    F.addConstraint(fun, arg)

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

      F.addInterface(id, FlowType.Interface(id, props.toMap))
  }

def freshFlowType(tpe: BlockType)(using C: ErrorReporter, F: FlowAnalysis): FlowType = tpe match {
  case BlockType.Function(tparams, eparams, vparams, bparams, result) =>
    FlowType.Function(Evidences.fresh(eparams.size), bparams.map(freshFlowType))
  case BlockType.Interface(name, targs) =>
    F.interfaceFor(name)
}