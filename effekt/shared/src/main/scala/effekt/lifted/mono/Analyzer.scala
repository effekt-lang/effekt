package effekt
package lifted
package mono

import effekt.symbols.builtins.TState
import scala.collection.immutable.ListMap
import effekt.util.messages.{ ErrorReporter, FIXME, INTERNAL_ERROR, NOT_SUPPORTED, TODO }
import kiama.util.Memoiser

import scala.collection.mutable


class FlowAnalysis(

  // block binders like `def` and block parameters.
  val binders: mutable.Map[Id, FlowType] = mutable.Map.empty,

  // every (block-) subtree is annotated with its flow type
  annotations: Memoiser[lifted.Block, FlowType] = Memoiser.makeIdMemoiser(),

  // maps interface id to the flowtypes of operations
  interfaces: mutable.Map[Id, InterfaceDeclaration] = mutable.Map.empty,

  // lexical scoping: maps evidence parameters like ev104 to concrete evidence (e.g., <α17._2, Try, Try>)
  parameters: mutable.Map[Id, Ev] = mutable.Map.empty,

  // collected constraints
  var constraints: List[Constraint] = Nil,

  // generated unification variables
  var variables: Set[Evidences.FlowVar] = Set.empty
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

  def freshVariable(arity: Int, origin: Any): Evidences.FlowVar =
    val x = Evidences.fresh(arity, origin)
    variables = variables + x
    x
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
  mod.externs.foreach(analyze)
  analyzeDefinitions(mod.definitions)

  // TODO replace by an analysis whether evidence is used at all, or not.
  // F.addConstraint(F.flowTypeFor(main), FlowType.Function(Evidences.Concrete(List(Ev(Nil))), Nil))
}

def analyze(e: Extern)(using C: ErrorReporter, F: FlowAnalysis): Unit = e match {
  // extern def foo(ev: EV) { f: (EV) => Unit } = ...
  //                     |        ^
  //                     \________/
  // We have to assume that the evidence will flow directly into f.
  case Extern.Def(id, tparams, params, ret, body) =>
    // FOR NOW: we only add empty evidence
    val x: Evidences.FlowVar = F.freshVariable(0, e)
    val vparams = params.collect { case p : Param.ValueParam => p.tpe }
    val bfts = params.collect { case p : Param.BlockParam => freshFlowType(p.tpe) }
    val ftpe = FlowType.Function(x, tparams, vparams, bfts, ret)
    F.addDefinition(id, ftpe)


    //    val BlockType.Function(tparams, _, vparams, bparams, ret) = b.tpe : @unchecked
    //    val eps = params.collect { case Param.EvidenceParam(id) => id }
    //    val bfts = bindBlockParams(params)

    // TODO continue here...

    // externs do not receive evidence themselves -- maybe for their block params?
    //val ev: Evidences.FlowVar = Evidences.fresh(eps.size)

  case Extern.Include(contents) => ()
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
    val x: Evidences.FlowVar = F.freshVariable(eps.size, b)
    eps.zipWithIndex.foreach { case (id, idx) =>
      F.bindEvidence(id, Ev(List(Lift.Var(x, idx))))
    }

    // Step 2) Create flow types for block parameters
    val bfts = bindBlockParams(params)

    FlowType.Function(x, tparams, vparams, bfts, ret)


  case Block.Member(b, field, annotatedTpe) =>
    analyze(b) match {
      case f: FlowType.Function => C.panic("Should not happen: member selection on function type")
      case FlowType.Interface(id, _) =>
        F.interfaceDeclarationFor(id).operations(field)
    }

  case Block.New(Implementation(BlockType.Interface(name, targs), operations)) =>
    FlowType.Interface(name, targs)

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
    case Block.New(impl) => analyze(impl)
  }

def analyze(b: Block)(using C: ErrorReporter, F: FlowAnalysis): FlowType = {
  val tpe = preanalyze(b); traverse(b); tpe
}


def analyze(l: lifted.Lift)(using C: ErrorReporter, F: FlowAnalysis): List[Lift] = l match {
  case lifted.Lift.Var(ev) => F.evidenceFor(ev).lifts
  case lifted.Lift.Try() => List(Lift.Try())
  case lifted.Lift.Reg() => List(Lift.Reg())
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
      case Evidence(lifts) => Ev(lifts.flatMap(analyze))
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

  case Stmt.Shift(ev, body) => analyze(body)

  case Stmt.Return(e) => analyze(e)
  case Stmt.Hole() => ()

  case Stmt.Region(Block.BlockLit(Nil, ev :: caps, body)) =>
    F.bindEvidence(ev.id, Ev(List(Lift.Reg())))
    bindBlockParams(caps)
    analyze(body)

  // def x: { def get(ev): T; def put(ev, T): Unit }
  case Stmt.Alloc(id, init, region, ev, body) =>
    //
    //    val stateType = init.tpe
    //
    //    // TODO maybe add a fresh interface for every cell?
    //    //   Now elaboration would need to create instances of it.
    //    val refId = Id("Ref")
    //    val getVar = Evidences.fresh(1)
    //    val getTpe = FlowType.Function(getVar, Nil, Nil, Nil, stateType)
    //    val putTpe = FlowType.Function(getVar, Nil, List(stateType), Nil, lifted.Type.TUnit)
    //    F.addInterface(refId, InterfaceDeclaration(Map(TState.get -> getTpe, TState.put -> putTpe)))
    //
    //    val bft = freshFlowType(BlockType.Interface(refId, List(init.tpe)))
    //    F.addDefinition(id, bft)
    //
    //    analyze(init)
    //    analyze(body)
    //    FIXME((), "implement")
    FIXME((), "Implement monomorphization for region based state")

  case Stmt.Var(init, Block.BlockLit(Nil, List(ev, x), body)) =>
    analyze(init)
    F.bindEvidence(ev.id, Ev(List(Lift.Reg())))
    bindBlockParams(List(x))
    analyze(body)

  case Stmt.Get(x, ev, tpe) =>
    ()

  case Stmt.Put(x, ev, value) =>
     analyze(value)

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
  case Expr.Make(data, tag, args) => args.foreach(analyze)

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
    val x = F.freshVariable(eparams.size, tpe)
    FlowType.Function(x, tparams, vparams, bparams.map(freshFlowType), result)
  case BlockType.Interface(name, targs) =>
    FlowType.Interface(name, targs)
}
