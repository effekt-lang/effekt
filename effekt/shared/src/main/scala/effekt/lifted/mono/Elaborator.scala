package effekt
package lifted
package mono

import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR, TODO, NOT_SUPPORTED }
import scala.collection.mutable

enum ElaboratedType {
  case Single()
  // id: type id of the interface (e.g. Function)
  // tparams: value types that have been abstracted over by the interface
  // variants: map from evidence configuration to new operation name
  // tpe: type of a single operation (all equal for the variants)
  // abstractedParams: names that are abstracted over b
  case Multi(id: Id, tparams: List[Id], variants: Map[Evidences, Id], tpe: BlockType)
}

class TransformationContext(
  flow: FlowAnalysis,
  substitution: Bisubstitution,
  choices: Map[Id, Ev] = Map.empty,

  // for every class of function type, we have one elaborated type that we use.
  var functions: Equivalences,
  types: mutable.Map[Node, ElaboratedType] = mutable.Map.empty,

  // additionally generated interfaces
  val interfaces: mutable.Map[Id, Declaration.Interface] = mutable.Map.empty,
  specialization: mutable.Map[(Id, Evidences.Concrete), Id] = mutable.Map.empty
) {
  export flow.*

  def withChoices(newChoices: List[(Id, Ev)]): TransformationContext =
    TransformationContext(flow, substitution, choices ++ newChoices.toMap, functions, types, interfaces, specialization)

  def currentChoiceFor(evidenceParam: Id): Ev = choices.getOrElse(evidenceParam, Ev.Zero)

  def specializationFor(id: Id, ev: Evidences.Concrete): Id =
    val key = (id, ev)
    if (specialization.isDefinedAt(key)) {
      specialization(key)
    } else {
      // e.g. for "f" with [<>, <Try>], we will have "f$0$1"
      val newSpecialization = Id(id.name.name + "$" + ev.evs.map(_.lifts.size).mkString("$"))
      specialization.update(key, newSpecialization)
      newSpecialization
    }

  private def relatedFunctions(ftpe: FlowType.Function): Set[FlowType.Function] =
    functionsOf(classOf(ftpe))

  private def classOf(ftpe: FlowType.Function): Node =
    functions.getOrElse(ftpe, {
      val n = new Node
      functions = functions + (ftpe -> n)
      n
    })

  // TODO cache
  private def functionsOf(cls: Node): Set[FlowType.Function] = functions.collect {
    case (ftpe, other) if other == cls => ftpe
  }.toSet

  def elaboratedType(ftpe: FlowType.Function): ElaboratedType = {
    val cls = classOf(ftpe)
    if types.isDefinedAt(cls) then return types(cls)

    val FlowType.Function(evidences, tparams, vparams, bparams, ret) = ftpe

    val configs = configurations(evidences).toList

    // 1) single specialization: nothing to do, if there is only one specialization
    if (configs.size == 1) {
      val elaborated = ElaboratedType.Single()
      types.update(cls, elaborated)
      return elaborated
    }

    // 2) multiple specializations: we have to generate a new interface
    val interfaceName = Id("Function")

    // Let's assume we have functions { Int => Int, T => Int }, then we need to abstract over
    // the first argument: Function[A] { def apply$1(A): Int }
    val funs = relatedFunctions(ftpe).toList
    def allEqual(ps: List[ValueType]): Boolean = ps.toSet.size == 1


    var abtractedTParams: List[Id] = Nil
    val abstractedVparams = funs.map { f => f.vparams }.transpose.map {
      case ps if allEqual(ps) => ps.head
      case ps => val fresh = Id("A"); abtractedTParams = abtractedTParams :+ fresh; ValueType.Var(fresh)
    }
    val abstractedReturn = if allEqual(funs.map { f => f.result }) then ret else {
      val fresh = Id("B"); abtractedTParams = abtractedTParams :+ fresh; ValueType.Var(fresh)
    }

    // TODO there could be differences in the blockparams that we would need to abstract over as well:
    val abstractedType = BlockType.Function(tparams, Nil, abstractedVparams, bparams.map(elaborate), abstractedReturn)

    val variants = configs map { case e @ Evidences.Concrete(evs) =>
      // e.g. for "f" with [<>, <Try>], we will have "f$0$1"
      val variantName = Id("apply" + evs.map(_.lifts.size).mkString("$"))
      (e, variantName)
    }

    interfaces.update(interfaceName, Declaration.Interface(interfaceName, abtractedTParams, variants.map {
      case (_, name) => Property(name, abstractedType)
    }))

    val elaborated = ElaboratedType.Multi(interfaceName, abtractedTParams, variants.toMap, abstractedType)

    types.update(cls, elaborated)
    elaborated
  }


  // TODO we could cache this
  def elaborate(ftpe: FlowType): BlockType = ftpe match {
    case f @ FlowType.Function(evidences, tparams, vparams, bparams, ret) => elaboratedType(f) match {
      // we use the original type (structurally typed)
      case ElaboratedType.Single() => BlockType.Function(tparams, Nil, vparams, bparams.map(elaborate), ret)
      // TODO, here we need to compute the type arguments properly!
      case ElaboratedType.Multi(id, tparams, variants, tpe) => BlockType.Interface(id, vparams :+ ret)
    }
    case FlowType.Interface(id, targs) => BlockType.Interface(id, targs)
  }

  def configurations(termId: Id): Set[Evidences.Concrete] = configurations(flow.flowTypeForBinder(termId))

  def configurations(ftpe: FlowType): Set[Evidences.Concrete] = ftpe match {
    case f @ FlowType.Function(evidences, _, _, bparams, _) =>
      // find all functions of a class and union their configurations
      functionsOf(classOf(f)).flatMap(f => configurations(f.evidences))
    case FlowType.Interface(id, targs) =>
      INTERNAL_ERROR("Interfaces are treated nominal and do not have a set of configurations (only their operations do)")
  }

  def configurations(ev: Evidences): Set[Evidences.Concrete] = ev match {
    case Evidences.Concrete(evs) => INTERNAL_ERROR("We only look up specializations on block symbols with unification variables")
    case x : Evidences.FlowVar => solutionsFor(x)
  }

  private def solutionsFor(x: Evidences.FlowVar): Set[Evidences.Concrete] =
    substitution.get(x).map { evs =>
      evs.upper.collect {
        case ev: Evidences.Concrete => ev
      }
    // not found in solution: use zero everywhere
    } getOrElse { Set(Ev.zero(x.arity)) }
}

def elaborate(m: ModuleDecl)(using T: TransformationContext): ModuleDecl = {
  val decls = m.decls.map(elaborate)
  val defns = m.definitions.map(elaborate)

  val additionalDecls = T.interfaces.values.toList
  ModuleDecl(m.path, m.imports, decls ++ additionalDecls, m.externs, defns, m.exports)
}

def elaborate(d: Declaration)(using T: TransformationContext): Declaration = d match {
  case Declaration.Interface(id, tparams, properties) =>
    // e.g. Cap { op1: a17(); op2: a18() }
    val decl = T.interfaceDeclarationFor(id)

    val newProps = properties.flatMap {
      case Property(id, tpe) =>
        // e.g. a17()
        val flowType = decl.operations(id)

        // e.g. [<>], [<Try>]
        val configs = flowType match {
          case FlowType.Function(ev, _, _, Nil, _) =>
            T.configurations(ev)
          case FlowType.Function(ev, _, _, _, _) => NOT_SUPPORTED("monomorphization for bidirectional effects")
          case _: FlowType.Interface => NOT_SUPPORTED("monomorphization for nested objects / modules")
        }

        configs.map { config =>
          val newId = T.specializationFor(id, config)
          val newTpe = T.elaborate(flowType) // TODO check!
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
    Right(Param.BlockParam(id, T.elaborate(T.flowTypeForBinder(id))))
}

def elaborate(d: Definition)(using T: TransformationContext): Definition = d match {
  // TODO we need to factor this out to elaborate(Block)
  case Definition.Def(id, block) => Definition.Def(id, elaborate(block))
  case Definition.Let(id, binding) => Definition.Let(id, elaborate(binding))
}

def elaborateBlockLit(b: Block.BlockLit)(using T: TransformationContext): List[(Evidences.Concrete, Block.BlockLit)] =
  b match {
    case Block.BlockLit(tparams, params, body) =>
      val (evidenceIds, remainingParams) = params.partitionMap(elaborate)

      val ftpe = T.annotatedType(b)

      // now we need to generate multiple copies of this block:
      T.configurations(ftpe).toList.map { config =>
        val currentChoices = evidenceIds.zip(config.evs)
        given TransformationContext = T.withChoices(currentChoices)

        config -> (Block.BlockLit(tparams, remainingParams, elaborate(body)) : Block.BlockLit)
      }
  }

def elaborate(b: Block)(using T: TransformationContext): Block = b match {
  case b @ Block.BlockLit(tparams, params, body) =>
    // now we need to generate multiple copies of this block:
    val variants = elaborateBlockLit(b)

    val ftpe = T.annotatedFunctionType(b)

    (T.elaboratedType(ftpe), variants) match {
      case (ElaboratedType.Single(), List((config, block))) => block
      case (ElaboratedType.Multi(id, tparams, ops, tpe), variants) =>
        val operations = variants map {
          case (ev, impl) => Operation(ops(ev), impl)
        }
        Block.New(Implementation(BlockType.Interface(id, Nil), operations))
      case _ => INTERNAL_ERROR("should never happen")
    }

  case Block.New(impl) => Block.New(elaborate(impl))

  // we do not need to touch variables
  case Block.BlockVar(id, annotatedType) => b

  // unsupported cases
  case Block.Member(receiver, field, annotatedTpe) =>
    // TODO check correctness (should only work with nested blocks, not methods)
    Block.Member(elaborate(receiver), field, T.elaborate(T.annotatedType(b)))

  case Block.Unbox(e) =>
    INTERNAL_ERROR("Not supported: monomorphization of box / unbox")
}

def elaborate(impl: Implementation)(using T: TransformationContext): Implementation = impl match {
  case Implementation(interface, operations) =>
    val newOps = operations.flatMap {
      case Operation(name, block) =>
        elaborateBlockLit(block) map {
          case (ev, impl) =>
            val newName = T.specializationFor(name, ev)
            Operation(newName, impl)
        }
    }
    Implementation(interface, newOps)
}




def elaborate(s: Stmt)(using T: TransformationContext): Stmt = s match {

  // [[ b.m(ev1, n) ]] = [[b]].m$[[ev1]]([[n]])
  case Stmt.App(m @ Block.Member(b, field, _), targs, args) =>

    val (evidenceArgs, remainingArgs) = args partitionMap {
      case e: Evidence => Left(elaborate(e))
      case e: Expr => Right(elaborate(e))
      case b: Block => Right(elaborate(b))
    }

    val specializedTarget = T.specializationFor(field, Evidences.Concrete(evidenceArgs))

    Stmt.App(Block.Member(elaborate(b), specializedTarget, T.elaborate(T.annotatedType(m))), targs, remainingArgs)

  // [[ f(ev1, n) ]] = f(n)  if only one specialization
  // [[ f(ev1, n) ]] = f.apply$1(n)  else
  case Stmt.App(b, targs, args) =>
    val ftpe = T.annotatedFunctionType(b)

    val (evidenceArgs, remainingArgs) = args partitionMap {
      case e: Evidence => Left(elaborate(e))
      case e: Expr => Right(elaborate(e))
      case b: Block => Right(elaborate(b))
    }

    // find configurations
    val target = T.elaboratedType(ftpe) match {
      case ElaboratedType.Single() => elaborate(b)
      case ElaboratedType.Multi(id, tparams, variants, tpe) =>
        // TODO here we need to choose the correct type instantation, not the abstracted one:
        Block.Member(elaborate(b), variants(Evidences.Concrete(evidenceArgs)), tpe)
    }

    Stmt.App(target, targs, remainingArgs)


  // [[ try { (EV, exc) => ...  } with Exc { ... } ]] = try { exc => [[ ... ]] } with [[ Exc { ... } ]]
  case Stmt.Try(Block.BlockLit(tparams, params, body), handler) =>
    val (evidenceIds, remainingParams) = params.partitionMap(elaborate)
    val elaboratedBody = {
      given TransformationContext = T.withChoices(evidenceIds.map(id => id -> Ev(List(Lift.Try()))))
      elaborate(body)
    }
    Stmt.Try(Block.BlockLit(tparams, remainingParams, elaboratedBody), handler.map(elaborate))

  case Stmt.Try(_, _) => INTERNAL_ERROR("unreachable")

  case Stmt.Shift(ev, body) =>
    Stmt.Shift(translate(elaborate(ev)), elaborate(body).asInstanceOf)

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

def elaborate(e: Evidence)(using T: TransformationContext): Ev = Ev(e.lifts.flatMap(elaborate))

def elaborate(l: lifted.Lift)(using T: TransformationContext): List[Lift] = l match {
  case lifted.Lift.Var(ev) => T.currentChoiceFor(ev).lifts
  case lifted.Lift.Try() => List(Lift.Try())
  case lifted.Lift.Reg() => List(Lift.Reg())
}
def translate(e: Ev): lifted.Evidence = Evidence(e.lifts map {
  case Lift.Try() => lifted.Lift.Try()
  case Lift.Reg() => lifted.Lift.Reg()
  case x: Lift.Var => INTERNAL_ERROR("Should not occur anymore after monomorphization!")
})


def elaborate(e: Expr)(using T: TransformationContext): Expr = e match {
  case Expr.ValueVar(id, annotatedType) => e
  case Expr.Literal(value, annotatedType) => e
  case Expr.PureApp(b, targs, args) => e // we do not touch pure applications
  case Expr.Select(target, field, annotatedType) => elaborate(target)
  case Expr.Box(b) => NOT_SUPPORTED("Elaboration of boxed blocks not supported, yet.")
  case Expr.Run(s) => Expr.Run(elaborate(s))
}
