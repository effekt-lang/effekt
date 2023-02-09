package effekt
package lifted
package mono

import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR, TODO, NOT_SUPPORTED }
import scala.collection.mutable

enum ElaboratedType {
  case Single(tpe: BlockType)
  // operations here maps the old id*configuration to the new operation id and its type.
  // the interface id can be ignored for operations, only relevant for new nominal types (function types)
  case Multi(id: Id, variants: Map[Evidences, (Id, BlockType)])
}

class TransformationContext(
  flow: FlowAnalysis,
  substitution: Bisubstitution,
  choices: Map[Id, Ev] = Map.empty,
  types: mutable.Map[FlowType, ElaboratedType] = mutable.Map.empty,

  // additionally generated interfaces
  val interfaces: mutable.Map[Id, Declaration.Interface] = mutable.Map.empty
) {
  export flow.*
  // Mapping the old operation id to the specialized one (e.g., id * [ev] -> id)
  private val specialization: mutable.Map[(Id, Evidences.Concrete), Id] = mutable.Map.empty

  def withChoices(newChoices: List[(Id, Ev)]): TransformationContext = TransformationContext(flow, substitution, choices ++ newChoices.toMap, types, interfaces)

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

  def elaboratedType(ftpe: FlowType): ElaboratedType = {
    if types.isDefinedAt(ftpe) then return types(ftpe)

    val elaborated = ftpe match {
      // TODO we have to be careful not to call this on operations.
      case f @ FlowType.Function(evidences, tparams, vparams, bparams, ret) =>
        generateVariants("apply", f) match {
          case List((_, name, tpe)) => ElaboratedType.Single(tpe)
          case multiple =>
            val interfaceName = Id("Function")
            interfaces.update(interfaceName, Declaration.Interface(interfaceName, Nil, multiple.map {
              case (_, name, tpe) => Property(name, tpe)
            }))

            ElaboratedType.Multi(interfaceName, multiple.map {
              case (e, name, tpe) => e -> (name, tpe)
            }.toMap)
        }

      case FlowType.Interface(id, targs) =>
        ElaboratedType.Single(BlockType.Interface(id, targs))
    }

    types.update(ftpe, elaborated)
    elaborated
  }

  def generateVariants(originalName: String, ftpe: FlowType.Function): List[(Evidences.Concrete, Id, BlockType)] =
    ftpe match {
      case FlowType.Function(evidences, tparams, vparams, bparams, ret) =>
        configurations(evidences).toList map { case e @ Evidences.Concrete(evs) =>
          // e.g. for "f" with [<>, <Try>], we will have "f$0$1"
          val variantName = Id(originalName + evs.map(_.lifts.size).mkString("$"))
          (e, variantName, BlockType.Function(tparams, Nil, vparams, bparams.map(elaborate), ret))
        }
    }

  def elaborate(ftpe: FlowType): BlockType = elaboratedType(ftpe) match {
    case ElaboratedType.Single(tpe) => tpe
    case ElaboratedType.Multi(id, variants) => BlockType.Interface(id, Nil)
  }

  def configurations(termId: Id): Set[Evidences.Concrete] = configurations(flow.flowTypeForBinder(termId))

  def configurations(ftpe: FlowType): Set[Evidences.Concrete] = ftpe match {
    case FlowType.Function(evidences, _, _, bparams, _) => configurations(evidences)
    case FlowType.Interface(id, targs) =>
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
  case (BlockType.Function(tps, eps, vps, bps, res), FlowType.Function(ev, _, _, bps2, _)) =>
    BlockType.Function(tps, Nil, vps, (bps zip bps2).map { case (b, f) => elaborate(b, f) }, res)
  // interfaces are nominal and don't change right now.
  case (BlockType.Interface(id, args), _) => tpe
  case _ => INTERNAL_ERROR(s"Illegal combination of types and flow-types: ${tpe} and ${ftpe}")
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

    val ftpe = T.annotatedType(b)

    (T.elaboratedType(ftpe), variants) match {
      case (ElaboratedType.Single(tpe), List((config, block))) => block
      case (ElaboratedType.Multi(id, ops), variants) =>
        val operations = variants map {
          case (ev, impl) =>
            val (specializedVariant, tpe) = ops(ev)
            Operation(specializedVariant, impl)
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
    Block.Member(elaborate(receiver), field, elaborate(annotatedTpe, T.annotatedType(b)))

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
      case e: Evidence => Left(Ev(e.scopes.flatMap(T.currentChoiceFor(_).lifts)))
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
      case e: Evidence => Left(Ev(e.scopes.flatMap(T.currentChoiceFor(_).lifts)))
      case e: Expr => Right(elaborate(e))
      case b: Block => Right(elaborate(b))
    }

    // find configurations
    val target = T.elaboratedType(ftpe) match {
      case ElaboratedType.Single(tpe) => elaborate(b)
      case ElaboratedType.Multi(id, variants) =>
        val (specializedTarget, tpe) = variants(Evidences.Concrete(evidenceArgs))
        Block.Member(elaborate(b), specializedTarget, tpe)
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
