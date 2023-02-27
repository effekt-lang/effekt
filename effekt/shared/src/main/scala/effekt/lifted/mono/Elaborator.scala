package effekt
package lifted
package mono

import effekt.util.messages.{ ErrorReporter, FIXME, INTERNAL_ERROR, NOT_SUPPORTED, TODO }

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
  val flow: FlowAnalysis,
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

    val configs = configurations(ftpe).toList

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
  val extns = m.externs.map(elaborate)

  val additionalDecls = T.interfaces.values.toList
  ModuleDecl(m.path, m.imports, decls ++ additionalDecls, extns, defns, m.exports)
}

def elaborate(e: Extern)(using T: TransformationContext): Extern = e match {
  case Extern.Def(id, tparams, params, ret, body) =>
    // We cannot monomorphize externs.
    // So what happens if they take a block param? Monomorphization changes the calling convention
    // of block params; but externs cannot know / account for this.
    // We could generate bridges (like Java does for erasure).
    //
    // Most important use case: extern interfaces (they are fully transparent and won't be monomorphized)
    // So eventually we do not care.
    // So for now, we simply convert the types of parameters and drop evidence parameters

    val ftpe = T.flow.flowTypeForBinder(id).asInstanceOf[FlowType.Function]

    val vps = params collect {
      case p: Param.ValueParam => p
    }

    val bids = params collect {
      case p : Param.BlockParam => p.id
    }

    val bps = (bids zip ftpe.bparams) map {
      case (id, tpe) => Param.BlockParam(id, T.elaborate(tpe))
    }
    Extern.Def(id, tparams, vps ++ bps, ret, body)
  case i @ Extern.Include(contents) => i
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
    // we have to establish a canonical ordering, such that implementations align with declarations
    // otherwise this results in flaky ordering problems since Sets do not have a guaranteed order of traversal
    }.sortBy(_.id.id)
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
      case c => INTERNAL_ERROR(s"should never happen: ${c}")
    }

  case Block.New(impl) => Block.New(elaborate(impl))

  // we do not need to touch variables
  // TODO what about their types.
  case Block.BlockVar(id, annotatedType) => b

  // unsupported cases
  case Block.Member(receiver, field, annotatedTpe) =>
    // TODO check correctness (should only work with nested blocks, not methods)
    Block.Member(elaborate(receiver), field, T.elaborate(T.annotatedType(b)))

  case Block.Unbox(e) => Block.Unbox(elaborate(e)) // Not really supported
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
    // we have to establish a canonical ordering (see counterpart in elaborate(Declaration)
    }.sortBy(_.name.id)
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

    // TODO how do we get the proper method type?
    //   calling `T.elaborate(T.annotatedType(m))` will potentially return an interface, not function type
    //   since we are not supposted to elaborate method types.

    val methodType = T.annotatedType(m) match {
      case FlowType.Function(evidences, tparams, vparams, bparams, result) =>
        BlockType.Function(tparams, Nil, vparams, bparams.map(T.elaborate), result)
      case FlowType.Interface(id, targs) => INTERNAL_ERROR("Cannot be an interface type")
    }

    Stmt.App(Block.Member(elaborate(b), specializedTarget, methodType), targs, remainingArgs)

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
        val variant = variants.getOrElse(Evidences.Concrete(evidenceArgs),
          INTERNAL_ERROR(s"Cannot find ${id} variant for ${evidenceArgs.map(_.show)}, got variants: ${variants.keys.map(_.show).mkString(", ")}. \n\nCall: ${s}"))
        Block.Member(elaborate(b), variant, tpe)
    }

    Stmt.App(target, targs, remainingArgs)


  // [[ try { (EV, exc) => ...  } with Exc { ... } ]] = try { exc => [[ ... ]] } with [[ Exc { ... } ]]
  case Stmt.Try(Block.BlockLit(tparams, params, body), handler) =>
    val (evidenceIds, remainingParams) = params.partitionMap(elaborate)
    val elaboratedBody = {
      given TransformationContext = T.withChoices(evidenceIds.map(id => id -> Ev(List(Lift.Try()))))
      elaborate(body)
    }
    val capabilities = handler.map { h => Block.New(elaborate(h)) }

    val newBody = FIXME(Stmt.App(Block.BlockLit(tparams, remainingParams, elaboratedBody), Nil, capabilities), "this could be a def")
    Stmt.Reset(newBody)

  case Stmt.Try(_, _) => INTERNAL_ERROR("unreachable, body should always be a blocklit.")

  case Stmt.Reset(body) => Stmt.Reset(elaborate(body))

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
  case Stmt.Alloc(id, init, region, ev, body) =>
    FIXME(Stmt.Alloc(id, elaborate(init), region, ev, elaborate(body)), "Support mutable state")

  case Stmt.Var(init, BlockLit(Nil, List(ev, x), body)) =>
    val elaboratedInit = elaborate(init)
    val elaboratedBody = {
      given TransformationContext = T.withChoices(List(ev.id -> Ev(List(Lift.Reg()))))
      elaborate(body)
    }
    Stmt.Var(elaboratedInit, BlockLit(Nil, List(x), elaboratedBody))

  case Stmt.Var(_, _) => INTERNAL_ERROR("unreachable, body of var should always be a blocklit.")

  case Stmt.Get(x, ev, tpe) =>
    Stmt.Get(x, translate(elaborate(ev)), tpe)

  case Stmt.Put(x, ev, value) =>
    Stmt.Put(x, translate(elaborate(ev)), elaborate(value))

  case Stmt.Region(Block.BlockLit(tparams, params, body)) =>
    val (evidenceIds, remainingParams) = params.partitionMap(elaborate)
    val elaboratedBody = {
      given TransformationContext = T.withChoices(evidenceIds.map(id => id -> Ev(List(Lift.Reg()))))
      elaborate(body)
    }
    Stmt.Region(Block.BlockLit(Nil, remainingParams, elaboratedBody))

  case Stmt.Region(_) => INTERNAL_ERROR("unreachable, body of region should always be a blocklit.")
  case Stmt.Hole() => Stmt.Hole()
}

def elaborate(e: Evidence)(using T: TransformationContext): Ev = Ev(e.lifts.flatMap(elaborate))

def elaborate(l: lifted.Lift)(using T: TransformationContext): List[Lift] = l match {
  case lifted.Lift.Var(ev) => T.currentChoiceFor(ev).lifts
  case lifted.Lift.Try() => List(Lift.Try())
  case lifted.Lift.Reg() => List(Lift.Reg())
}
def translate(e: Ev): lifted.Evidence = Evidence(e.lifts flatMap {
  case Lift.Try() => List(lifted.Lift.Try())

  case Lift.Reg() => FIXME(List(lifted.Lift.Reg()), "Here we need to pass Reg again, once we assign it the correct runtime semantics.") // lifted.Lift.Reg()
  case x: Lift.Var => INTERNAL_ERROR("Should not occur anymore after monomorphization!")
})


def elaborate(e: Expr)(using T: TransformationContext): Expr = e match {
  case Expr.ValueVar(id, annotatedType) => e
  case Expr.Literal(value, annotatedType) => e
  case Expr.PureApp(b, targs, args) => e // we do not touch pure applications
  case Expr.Select(target, field, annotatedType) => Expr.Select(elaborate(target), field, annotatedType)
  case Expr.Box(b) => Expr.Box(elaborate(b))
  case Expr.Run(s) => Expr.Run(elaborate(s))
}
