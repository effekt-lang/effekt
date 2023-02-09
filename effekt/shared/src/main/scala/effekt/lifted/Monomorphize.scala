package effekt
package lifted

import scala.collection.immutable.ListMap
import core.Id
import effekt.context.Context
import effekt.util.Structural
import effekt.util.messages.{ CompilerPanic, ErrorReporter, PlainTextError }
import kiama.util.Severities

def TODO(msg: String = ""): Nothing =
  val explanation = if (msg.isEmpty) "Not implemented, yet" else s"Not implemented, yet: ${msg}"
  throw CompilerPanic(PlainTextError(explanation, None, Severities.Error))

def NOT_SUPPORTED(msg: String = ""): Nothing =
  val explanation = if (msg.isEmpty) "Currently not supported" else s"Currently not supported: ${msg}"
  throw CompilerPanic(PlainTextError(explanation, None, Severities.Error))

def INTERNAL_ERROR(msg: String = ""): Nothing =
  val explanation = if (msg.isEmpty) "Internal compiler error" else s"Internal compiler error: ${msg}"
  throw CompilerPanic(PlainTextError(explanation, None, Severities.Error))

// TODO in test14.effekt the following looks suspicious:
//    α112(α113()) <: α1()
//
// a113 is resume! This will be fixed by adding an explicit shift.
//
// we do not deal with bidirectional effects, atm

/**
 * One entry in one particular evidence:
 *
 *     <α17._2, Try, Try>
 *              ^^^
 */
enum Lift {
  case Try()
  case Reg()
  case Var(x: Evidences.FlowVar, selector: Int)

  def show: String = this match {
    case Try() => "Try"
    case Reg() => "Reg"
    case Var(x, selector) => x.show + "._" + selector
  }
}

case class Ev(lifts: List[Lift]) {
  def show: String =  s"<${lifts.map(_.show).mkString(", ")}>"
}
object Ev {
  val Zero = Ev(Nil)
  def zero(arity: Int): Evidences.Concrete = Evidences.Concrete((0 until arity).map(_ => Zero).toList)
}

// evidences on a function are either a variable or a vector of concrete evidences
enum Evidences {
  // e.g. [<α17._2, Try, Try>, <>]
  case Concrete(evs: List[Ev])
  // e.g. α17
  case FlowVar(id: Int, arity: Int)

  def show: String = this match {
    case Evidences.Concrete(evs) => s"[${evs.map(_.show).mkString(", ")}]"
    case Evidences.FlowVar(id, arity) => s"α${id}"
  }
}
object Evidences {
  var last = -1
  def fresh(arity: Int): Evidences.FlowVar =
    last += 1
    Evidences.FlowVar(last, arity)
}

enum FlowType {
  // Structural (we ignore value parameters and return for now since we disregard boxing)
  case Function(evidences: Evidences, bparams: List[FlowType])

  // Nominal (there exists one such type for each id)
  case Interface(id: Id, operations: Map[Id, FlowType])

  def show: String = this match {
    case FlowType.Function(evs, bparams) => s"${evs.show}(${bparams.map(_.show).mkString(", ")})"
    case FlowType.Interface(id, operations) =>
      val prettyOperations = operations.map { case (id, tpe) => id.toString + ": " + tpe.show  }.mkString(", ")
      s"${id.toString} { ${prettyOperations} }"
  }
}

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


// {} <: _ <: { [<>], [<Try>] }
case class Bounds(lower: Set[Evidences], upper: Set[Evidences])

// TODO always add ?main <:< [<>]
//  we could also assume that all functions that do not have bounds, are bounded with [<>]

type Bisubstitution = Map[Evidences.FlowVar, Bounds]

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


object Monomorphize extends Structural {

  val phaseName = "lift-inference"

  def run(mod: ModuleDecl)(using C: Context): ModuleDecl = {
    given analysis: FlowAnalysis()
    Evidences.last = -1;
    analyze(mod)

    val defTypes = analysis.definitions.map {
      case (id, flowType) => s"${id}: ${flowType.show}"
    }.mkString("\n")

    val interfaceTypes = analysis.interfaces.map {
      case (id, flowType) => s"${id}: ${flowType.show}"
    }.mkString("\n")

    val treeTypes = analysis.trees.map {
      case (t, flowType) => flowType.show
    }.mkString("\n")

    val constrs = analysis.constraints.map {
      c => c.show
    }.mkString("\n")

    val solved = solve(analysis.constraints)
    val cleaned = cleanup(substitution(solved.toList))

    val subst = cleaned.toList.sortBy(_._1.id).map {
      case (x, Bounds(lower, upper)) =>  lower.map(_.show).mkString(", ") + " <: " + x.show + " <: " + upper.map(_.show).mkString(", ")
    }.mkString("\n")

    given TransformationContext(analysis, cleaned, Map.empty)

    val newDeclarations = mod.decls.map(elaborate).map {
      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    }.mkString("\n")

    val newDefinitions = mod.definitions.map(elaborate).map {
      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    }.mkString("\n")

    C.debug(s"""|Solved:
        |-------
        |${subst}
        |
        |Elaborated Declarations:
        |------------------------
        |${newDeclarations}
        |
        |Elaborated Definitions:
        |------------------------
        |${ newDefinitions }
        |
        |Interfaces:
        |-----------
        |${interfaceTypes}
        |
        |Definitions:
        |-----------
        |${defTypes}
        |
        |Trees:
        |------
        |${treeTypes}
        |
        |Constraints:
        |-----------
        |${constrs}""".stripMargin)

    mod
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

  case class Solver(constraints: List[Constraint], seen: Set[Constraint], subst: Bisubstitution) {
    def step(): Solver = constraints match {
      case head :: tail if seen contains head => Solver(tail, seen, subst)

      case Constraint.B(i1: FlowType.Interface, i2: FlowType.Interface) :: rest =>
        assert(i1 == i2, s"The two interfaces are not the same! ${i1} and ${i2}")
        Solver(rest, seen, subst)

      case (c @ Constraint.B(FlowType.Function(ev1, bparams1), FlowType.Function(ev2, bparams2))) :: rest =>
        val evidenceFlow = Constraint.E(ev1, ev2)
        val paramsFlow = bparams2.zip(bparams1).map { case (c2, c1) => Constraint.B(c2, c1) }
        Solver(evidenceFlow :: paramsFlow ++ rest, seen + c, subst)

      case Constraint.E(Evidences.Concrete(evs1), Evidences.Concrete(evs2)) :: rest =>
        Solver(rest, seen, subst)

      case (c @ Constraint.E(x: Evidences.FlowVar, y)) :: rest =>
        val xbounds = subst.getOrElse(x, Bounds(Set.empty, Set.empty))
        Solver(xbounds.lower.map(Constraint.E(_, y)).toList ++ rest, seen + c, subst + (x -> Bounds(xbounds.lower, xbounds.upper + y)))

      case (c @ Constraint.E(x, y: Evidences.FlowVar)) :: rest =>
        val ybounds = subst.getOrElse(y, Bounds(Set.empty, Set.empty))
        Solver(ybounds.upper.map(Constraint.E(x, _)).toList ++ rest, seen + c, subst + (y -> Bounds(ybounds.lower + x, ybounds.upper)))

      case c :: rest =>
        sys error s"Mismatch: ${c}"

      case Nil => this
    }
  }

  def solve(cs: List[Constraint]): Map[Evidences.FlowVar, Bounds] =
    var solver = Solver(cs, Set.empty, Map.empty)
    while (solver.constraints.nonEmpty) {
      solver = solver.step()
    }
    solver.subst

  // a1 <: [<a2.0>]
  // a2 <: [<>]


  // A very naive implementation, that is quadradic in the number of unification variables
  // it also does not detect "stack shape polymorphic recursion", which needs to be implemented separately.
  def substitution(from: List[(Evidences.FlowVar, Bounds)]): Bisubstitution = from match {
    case (x, bounds) :: rest =>
      val subst = substitution(rest)
      val updatedBounds = Substitution(subst).substitute(bounds)
      substitute(x, updatedBounds, subst) + (x -> updatedBounds)
    case Nil => Map.empty
  }

  def substitute(x: Evidences.FlowVar, bounds: Bounds, into: Bisubstitution): Bisubstitution =
    into.map { case (y, ybounds) => y -> substitute(x, bounds, ybounds) }

  def substitute(x: Evidences.FlowVar, bounds: Bounds, into: Bounds): Bounds =
    Bounds(substitute(x, bounds.lower, into.lower), substitute(x, bounds.upper, into.upper))

  def substitute(x: Evidences.FlowVar, choices: Set[Evidences], into: Set[Evidences]): Set[Evidences] =
    into.flatMap(evs => substitute(x, choices, evs))

  // [<>, <a1.0>] [a1 !-> { [<>], [<Try>] }]  =  [<>, <>],  [<>, <Try>]
  def substitute(x: Evidences.FlowVar, choices: Set[Evidences], into: Evidences): Set[Evidences] = into match {
    case Evidences.Concrete(evs) => choices.map { c =>
      Evidences.Concrete(evs.map { ev => substituteSingleChoice(x, c, ev) })
    }
    case y : Evidences.FlowVar if x == y => choices
    case y : Evidences.FlowVar => Set(y)
  }

  // <Try, ?a.0>[?a !-> [<Try>, <>]]  = <Try, <Try>> = <Try, Try>
  def substituteSingleChoice(x: Evidences.FlowVar, ev: Evidences, into: Ev): Ev =
    Ev(into.lifts.flatMap(l => substituteSingleChoice(x, ev, l)))

  def substituteSingleChoice(x: Evidences.FlowVar, ev: Evidences, into: Lift): List[Lift] = into match {
    case Lift.Var(y, selector) if x == y => ev match {
      case Evidences.Concrete(evs) => evs(selector).lifts
      case z : Evidences.FlowVar => List(Lift.Var(z, selector))
    }
    case other => List(other)
  }


  def freeVars(l: Lift): Set[Evidences.FlowVar] = l match {
    case Lift.Var(x, selector) => Set(x)
    case _ => Set.empty
  }

  def freeVars(ev: Ev): Set[Evidences.FlowVar] = ev.lifts.toSet.flatMap(freeVars)
  def freeVars(evs: Evidences): Set[Evidences.FlowVar] = evs match {
    case Evidences.Concrete(evs) => evs.toSet.flatMap(freeVars)
    case x : Evidences.FlowVar => Set(x)
  }

  // Parallel substitution
  // TODO we probably do not need to substitute into the lower bounds, since we never use them.
  class Substitution(subst: Bisubstitution) {

    def substitute(into: Bisubstitution): Bisubstitution =
      into.map { case (y, ybounds) => y -> substitute(ybounds) }

    def substitute(into: Bounds): Bounds =
      Bounds(substitute(into.lower, false), substitute(into.upper, true))

    def substitute(into: Set[Evidences], upper: Boolean): Set[Evidences] =
      into.flatMap(evs => substitute(evs, upper))

    // [<>, <a1.0>] [a1 !-> { [<>], [<Try>] }]  =  [<>, <>],  [<>, <Try>]
    def substitute(into: Evidences, upper: Boolean): Set[Evidences] = into match {
      case Evidences.Concrete(evs) =>
        val free = freeVars(into)
        val defined = free intersect subst.keySet
        var result = Set(evs)
        // now we have to apply all substitutions
        defined.foreach { x =>
          val bounds = if (upper) subst(x).upper else subst(x).lower
          result = for {
            choice <- bounds
            evs <- result
          } yield evs.map(ev => substituteSingleChoice(x, choice, ev))
        }
        result.map(Evidences.Concrete.apply)

      case y : Evidences.FlowVar if subst.isDefinedAt(y) =>
        if (upper) subst(y).upper else subst(y).lower
      case y : Evidences.FlowVar => Set(y)
    }
  }


  def isZero(evs: Evidences): Boolean = evs match {
    case Evidences.Concrete(evs) => evs.forall(ev => ev.lifts.isEmpty)
    case Evidences.FlowVar(id, arity) => true
  }

  // post processing step: drop all bounds that still mention unification variables
  // we drop all bindings for unification variables with empty bounds or ONLY zero evidence
  def cleanup(subst: Bisubstitution): Bisubstitution = subst.map {
    case (x, Bounds(lower, upper)) =>
      x -> Bounds(lower.filter(e => freeVars(e).isEmpty), upper.filter(e => freeVars(e).isEmpty))
  } filterNot {
    case (x, Bounds(lower, upper)) => lower.forall(isZero) && upper.forall(isZero)
  }

  class TransformationContext(flow: FlowAnalysis, substitution: Bisubstitution, choices: Map[Id, Ev]) {
    export flow.*
    // Mapping the old operation id to the specialized one (e.g., id * [ev] -> id)
    private var specialization: Map[(Id, Evidences.Concrete), Id] = Map.empty

    def withChoices(newChoices: List[(Id, Ev)]): TransformationContext = TransformationContext(flow, substitution, choices ++ newChoices.toMap)

    def specializationFor(id: Id, ev: Evidences.Concrete): Id =
      val key = (id, ev)
      if (specialization.isDefinedAt(key)) {
        specialization(key)
      } else {
        // e.g. for "f" with [<>, <Try>], we will have "f$0$1"
        val newSpecialization = Id(id.name.name + "$" + ev.evs.map(_.lifts.size).mkString("$"))
        specialization = specialization.updated(key, newSpecialization)
        newSpecialization
      }

    def configurations(termId: Id): Set[Evidences.Concrete] = configurations(flow.flowTypeForDefinition(termId))

    def configurations(ftpe: FlowType): Set[Evidences.Concrete] = ftpe match {
      case FlowType.Function(evidences, bparams) => configurations(evidences)
      case FlowType.Interface(id, operations) =>
        INTERNAL_ERROR("Interfaces are treated nominal and do not have a set of configurations (only their operations do)")
    }

    def configurations(ev: Evidences): Set[Evidences.Concrete] = ev match {
      case Evidences.Concrete(evs) => INTERNAL_ERROR("We only look up specializations on block symbols with unification variables")
      case x : Evidences.FlowVar => solutionsFor(x)
    }

    def solutionsFor(x: Evidences.FlowVar): Set[Evidences.Concrete] =
      substitution.get(x).map { evs =>
        evs.upper.collect {
          case ev: Evidences.Concrete => ev
        }
      // not found in solution: use zero everywhere
      } getOrElse { Set(Ev.zero(x.arity)) }

    def currentChoiceFor(evidenceParam: Id): Ev = choices.getOrElse(evidenceParam, Ev.Zero)
  }

  def elaborate(tpe: BlockType, ftpe: FlowType)(using T: TransformationContext): BlockType = (tpe, ftpe) match {
    case (BlockType.Function(tps, eps, vps, bps, res), FlowType.Function(ev, bps2)) =>
      BlockType.Function(tps, Nil, vps, (bps zip bps2).map { case (b, f) => elaborate(b, f) }, res)
    // interfaces are nominal and don't change right now.
    case (BlockType.Interface(id, args), _) => tpe
    case _ => INTERNAL_ERROR(s"Illegal combination of types and flow-types: ${tpe} and ${ftpe}")
  }

  def elaborate(d: Declaration)(using T: TransformationContext): Declaration = d match {
    case Declaration.Interface(id, tparams, properties) =>
      // e.g. Cap { op1: a17(); op2: a18() }
      val ftpe = T.interfaceFor(id)

      val newProps = properties.flatMap {
        case Property(id, tpe) =>
          // e.g. a17()
          val flowType = ftpe.operations(id)

          // e.g. [<>], [<Try>]
          val configs = flowType match {
            case FlowType.Function(ev, Nil) =>
              T.configurations(ev)
            case FlowType.Function(ev, _) => NOT_SUPPORTED("monomorphization for bidirectional effects")
            case _: FlowType.Interface => NOT_SUPPORTED("monomorphization for nested objects / modules")
          }

          configs.map { config =>
            val newId = T.specializationFor(id, config)
            val newTpe = elaborate(tpe, flowType)
            Property(newId, newTpe)
          }
      }
      Declaration.Interface(id, tparams, newProps)


    // we do not need to elaborate data, since we don't support first class functions, yet.
    case d @ Declaration.Data(id, tparams, constructors) => d
  }

  def elaborate(param: Param)(using T: TransformationContext): Either[Id, Param] = param match {
    case p: Param.EvidenceParam => Left(p.id)
    case p: Param.ValueParam => Right(p)
    case Param.BlockParam(id, tpe) =>
      Right(Param.BlockParam(id, elaborate(tpe, T.flowTypeForDefinition(id))))
  }

  def elaborate(d: Definition)(using T: TransformationContext): Definition = d match {
    // TODO we need to factor this out to elaborate(Block)
    case Definition.Def(id, block) => block match {
      case Block.BlockLit(tparams, params, body) =>
        val (evidenceIds, remainingParams) = params.partitionMap(elaborate)

        // now we need to generate multiple copies of this block:
        val variants = T.configurations(id).toList.map { config =>
          val currentChoices = evidenceIds.zip(config.evs)
          given TransformationContext = T.withChoices(currentChoices)

          config -> (Block.BlockLit(tparams, remainingParams, elaborate(body)) : Block.BlockLit)
        }

        val transformedBlock = variants match {
          // only one variant, a function suffices.
          case List((config, block)) => block
          case _ =>
            val interfaceType: BlockType.Interface = BlockType.Interface(id, Nil) // TODO generate interface and do not use the termlevel id
            val operations = variants map {
              case (ev, impl) => Operation(T.specializationFor(id, ev), impl)
            }
            Block.New(Implementation(interfaceType, operations))
        }
        Definition.Def(id, transformedBlock)

      case Block.New(impl) => d // TODO

      // we do not need to touch variables
      case Block.BlockVar(id, annotatedType) => d

      // unsupported cases
      case Block.Member(b, field, annotatedTpe) =>
        INTERNAL_ERROR("Not supported yet, monomorphized selection of block members (don't know which specialization to select)")
      case Block.Unbox(e) =>
        INTERNAL_ERROR("Not supported: monomorphization of box / unbox")
    }
    case Definition.Let(id, binding) => Definition.Let(id, elaborate(binding))
  }

  enum ElaboratedType {
    case Function(tpe: BlockType)
    // operations here maps the old id*configuration to the new operation id and its type.
    case Interface(id: Id, operations: Map[(Id, Evidences), (Id, BlockType)])
  }

  // TODO share with Def
  def elaborate(b: Block)(using T: TransformationContext): Block = b match {
    case Block.BlockLit(tparams, params, body) =>
      // e.g. a13()
      val ftpe = T.annotatedType(b) match {
        case f: FlowType.Function => f
        case i: FlowType.Interface => INTERNAL_ERROR("Should be a function type")
      }

      val (evidenceIds, remainingParams) = params.partitionMap(elaborate)

      // now we need to generate multiple copies of this block:
      val variants = T.configurations(ftpe).toList.map { config =>
        val currentChoices = evidenceIds.zip(config.evs)
        given TransformationContext = T.withChoices(currentChoices)

        config -> (Block.BlockLit(tparams, remainingParams, elaborate(body)) : Block.BlockLit)
      }

      // TODO add a global map from
      // FlowType -> ElaboratedType

      variants match {
        // only one variant, a function suffices.
        case List((config, block)) => block
        case _ =>
          val interfaceType: BlockType.Interface = BlockType.Interface(Id("TODO_NEW_NAME"), Nil) // TODO generate interface and do not use the termlevel id
          // TODO
          val operations = variants map {
            case (ev, impl) =>
              val newName = Id("TODO_NEW_OPERATION_NAME") // T.specializationFor(id, ev)
              Operation(newName, impl)
          }
          Block.New(Implementation(interfaceType, operations))
      }

    case Block.New(impl) => b // TODO

    // we do not need to touch variables
    case Block.BlockVar(id, annotatedType) => b

    // unsupported cases
    case Block.Member(receiver, field, annotatedTpe) =>
      // TODO check correctness (should only work with nested blocks, not methods)
      Block.Member(elaborate(receiver), field, elaborate(annotatedTpe, T.annotatedType(b)))

    case Block.Unbox(e) =>
      INTERNAL_ERROR("Not supported: monomorphization of box / unbox")
  }

  def elaborate(impl: Implementation): Implementation = impl // TODO


  

  def elaborate(s: Stmt)(using T: TransformationContext): Stmt = s match {

    // [[ f(ev1, n) ]] = f(n)  if only one specialization
    // [[ f(ev1, n) ]] = f.apply$1(n)  else
    case Stmt.App(b, targs, args) =>
      val ftpe = T.annotatedType(b) match {
        case f: FlowType.Function => f
        case _ => INTERNAL_ERROR("Can only call functions")
      }

      val (evidenceArgs, remainingArgs) = args partitionMap {
        case e: Evidence => Left(e.scopes.map(T.currentChoiceFor))
        case e: Expr => Right(elaborate(e))
        case b: Block => Right(elaborate(b))
      }

      // find configurations
      T.configurations(ftpe.evidences).toList match {
        case List(config) => Stmt.App(elaborate(b), targs, remainingArgs)
        case configs =>
          // TODO maybe we can store a mapping from flowtype to elaborated type and search for the variant there?
          //  EVERYTHING SHOULD BE TYPE BASED!
          ???
      }

    // [[ try { (EV, exc) => ...  } with Exc { ... } ]] = try { exc => [[ ... ]] } with [[ Exc { ... } ]]
    case Stmt.Try(Block.BlockLit(tparams, params, body), handler) =>
      val (evidenceIds, remainingParams) = params.partitionMap(elaborate)
      val elaboratedBody = {
        given TransformationContext = T.withChoices(evidenceIds.map(id => id -> Ev(List(Lift.Try()))))
        elaborate(body)
      }
      Stmt.Try(Block.BlockLit(tparams, remainingParams, elaboratedBody), handler.map(elaborate))

    case Stmt.Try(_, _) => INTERNAL_ERROR("unreachable")

    // structural cases
    case Stmt.Scope(definitions, body) => Stmt.Scope(definitions.map(elaborate), elaborate(body))
    case Stmt.Return(e) => Stmt.Return(elaborate(e))
    case Stmt.Val(id, binding, body) => Stmt.Val(id, elaborate(binding), elaborate(body))
    case Stmt.If(cond, thn, els) => Stmt.If(elaborate(cond), elaborate(thn), elaborate(els))
    case Stmt.Match(scrutinee, clauses, default) => Stmt.Match(elaborate(scrutinee), clauses map {
        case (id, Block.BlockLit(tparams, params, body)) => (id, Block.BlockLit(tparams, params, elaborate(body)))
      }, default.map(elaborate))
    case Stmt.State(id, init, region, body) => TODO("Support mutable state")
    case Stmt.Region(body) => TODO("Support regions")
    case Stmt.Hole() => Stmt.Hole()
  }

  def elaborate(e: Expr)(using T: TransformationContext): Expr = e match {
    case Expr.ValueVar(id, annotatedType) => e
    case Expr.Literal(value, annotatedType) => e
    case Expr.PureApp(b, targs, args) => e // we do not touch pure applications
    case Expr.Select(target, field, annotatedType) => elaborate(target)
    case Expr.Box(b) => NOT_SUPPORTED("Elaboration of boxed blocks not supported, yet.")
    case Expr.Run(s) => Expr.Run(elaborate(s))
  }
}
