package effekt
package lifted
package mono

import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR, TODO, NOT_SUPPORTED }

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

  def configurations(termId: Id): Set[Evidences.Concrete] = configurations(flow.flowTypeForBinder(termId))

  def configurations(ftpe: FlowType): Set[Evidences.Concrete] = ftpe match {
    case FlowType.Function(evidences, bparams) => configurations(evidences)
    case FlowType.Interface(id) =>
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
    val decl = T.interfaceDeclarationFor(id)

    val newProps = properties.flatMap {
      case Property(id, tpe) =>
        // e.g. a17()
        val flowType = decl.operations(id)

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
    Right(Param.BlockParam(id, elaborate(tpe, T.flowTypeForBinder(id))))
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