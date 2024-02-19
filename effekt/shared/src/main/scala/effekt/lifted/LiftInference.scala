package effekt
package lifted

import effekt.Phase
import effekt.context.Context
import effekt.lifted
import effekt.core
import effekt.symbols.{ Symbol, builtins }
import effekt.context.assertions.*
import effekt.util.messages.ErrorReporter

object LiftInference extends Phase[CoreTransformed, CoreLifted] {

  val phaseName = "lift-inference"

  private def ErrorReporter(using E: ErrorReporter): ErrorReporter = E

  def env(using env: Environment): Environment = env

  def run(input: CoreTransformed)(using Context): Option[CoreLifted] =
    given Environment = Environment(Map.empty)
    val transformed = transform(input.core)
    Some(CoreLifted(input.source, input.tree, input.mod, transformed))

  // TODO either resolve and bind imports or use the knowledge that they are toplevel!
  def transform(mod: core.ModuleDecl)(using Environment, ErrorReporter): ModuleDecl = {
    val env = pretransform(mod.definitions)
    val definitions = mod.definitions.map(d => transform(d)(using env, ErrorReporter))
    ModuleDecl(mod.path, mod.includes, mod.declarations.map(transform), mod.externs.map(transform), definitions, mod.exports)
  }

  def transform(declaration: core.Declaration): lifted.Declaration = declaration match {
    case core.Declaration.Data(id, tparams, constructors) =>
      Declaration.Data(id, tparams, constructors.map(transform))
    case core.Declaration.Interface(id, tparams, properties) =>
      Declaration.Interface(id, tparams, properties.map(transform))
  }

  def transform(prop: core.Property): lifted.Property = prop match {
    case core.Property(id, tpe) => lifted.Property(id, transform(tpe))
  }

  def transform(prop: core.Field): lifted.Field = prop match {
    case core.Field(id, tpe) => lifted.Field(id, transform(tpe))
  }

  def transform(constructor: core.Constructor): lifted.Constructor = constructor match {
    case core.Constructor(id, fields) => lifted.Constructor(id, fields.map(transform))
  }

  def transform(param: core.Param): Param = param match {
    case core.ValueParam(id, tpe) => ValueParam(id, transform(tpe))
    case core.BlockParam(id, tpe, capt) => BlockParam(id, transform(tpe))
  }

  def transform(tpe: core.ValueType): lifted.ValueType = tpe match {
    // [[ X ]] = X
    case core.ValueType.Var(name) => lifted.ValueType.Var(name)
    // [[ List[Int] ]] = List[ [[Int]] ]
    case core.ValueType.Data(name, targs) => lifted.ValueType.Data(name, targs.map(transform))
    // Here we simply loose the capture information
    // [[ S at C ]] = box [[ S ]]
    case core.ValueType.Boxed(tpe, capt) => lifted.ValueType.Boxed(transform(tpe))
  }
  def transform(tpe: core.BlockType): lifted.BlockType = tpe match {
    // [[ [A](Int){f: Exc} => Int ]] = [A](EV, EV, [[Int]]){[[Exc]]} => [[Int]]
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      // here we turn cparams into evidence parameters (not necessary, only for debugging)
      val eparams = EvidenceType() :: cparams.map(p => EvidenceType())
      lifted.BlockType.Function(tparams, eparams, vparams.map(transform), bparams.map(transform), transform(result))
    // [[ State[Int] ]] = State[ [[Int]] ]
    case core.BlockType.Interface(name, targs) =>
      lifted.BlockType.Interface(name, targs.map(transform))
  }

  def transform(interface: core.BlockType.Interface): lifted.BlockType.Interface = interface match {
    case core.BlockType.Interface(name, targs) => lifted.BlockType.Interface(name, targs.map(transform))
  }

  def transform(tree: core.Block)(using Environment, ErrorReporter): lifted.Block = tree match {
    case b @ core.BlockLit(tps, cps, vps, bps, body) => liftBlockLitTo(b)
    case core.Member(body, id, tpe) => Member(transform(body), id, transform(tpe))
    case core.BlockVar(b, tpe, capt) => BlockVar(b, transform(tpe))
    // TODO check whether this makes sense here.
    case core.Unbox(b) => Unbox(transform(b))

    case core.New(impl) => New(transform(impl))
  }

  def transform(h: core.Implementation)(using Environment, ErrorReporter): Implementation = h match {
    case core.Implementation(interface, clauses) =>
      Implementation(transform(interface), clauses.map {
        case core.Operation(op, tps, cps, vps, bps, Some(resume), body) =>
          val ev = EvidenceSymbol()
          Operation(op, BlockLit(tps, Param.EvidenceParam(ev) :: (vps ++ bps).map(transform),
            // TODO what about evidence for resume?
            Stmt.Shift(Evidence(List(Lift.Var(ev))), Block.BlockLit(Nil, List(transform(resume)), transform(body)))))
        case core.Operation(op, tps, cps, vps, bps, None, body) =>
          Operation(op, liftBlockLitTo(core.Block.BlockLit(tps, cps, vps, bps, body)))
      })
  }

  def transform(tree: core.Extern)(using Environment, ErrorReporter): lifted.Extern = tree match {
    case core.Extern.Def(id, tps, cps, vps, bps, ret, capt, body) =>
      val self = Param.EvidenceParam(EvidenceSymbol()) // will never be used!
      val eparams = bps map {
        case core.BlockParam(id, tpe, capt) => Param.EvidenceParam(EvidenceSymbol())
      }
      Extern.Def(id, tps, vps.map(transform) ++ bps.map(transform), transform(ret),
        Template(body.strings, body.args.map(transform)))
    case core.Extern.Include(contents) =>
      Extern.Include(contents)
  }

  def transform(p: core.Param.ValueParam): lifted.Param.ValueParam = p match {
    case core.Param.ValueParam(id, tpe) => lifted.Param.ValueParam(id, transform(tpe))
  }
  def transform(p: core.Param.BlockParam): lifted.Param.BlockParam = p match {
    case core.Param.BlockParam(id, tpe, capt) => lifted.Param.BlockParam(id, transform(tpe))
  }

  def transform(tree: core.Definition)(using Environment, ErrorReporter): lifted.Definition = tree match {
    case core.Definition.Def(id, block) =>
      Definition.Def(id, transform(block))
    case core.Definition.Let(id, binding) =>
      Definition.Let(id, transform(binding))
  }

  def transform(tree: core.Stmt)(using Environment, ErrorReporter): Stmt = tree match {
    case core.Try(core.BlockLit(tparams, _, _, params, body), handler) =>

      // (1) Transform handlers first in unchanged environment.
      val transformedHandler = handler.map { transform }

      var environment = env

      // evidence for the region body itself
      val selfEvidence = EvidenceSymbol()

      environment = environment.adapt(Lift.Var(selfEvidence))

      // introduce one evidence symbol per blockparam
      val transformedParams = params map {
        case p @ core.BlockParam(id, tpe, capt) =>
          environment = environment.bind(id)
          transform(p)
      }

      // [[ try { {cap}... => s } with ... ]] = try { [ev]{cap}... => s } with ...
      val transformedBody = transform(body)(using environment, ErrorReporter) // lift is provided by the handler runtime

      Try(lifted.BlockLit(tparams, Param.EvidenceParam(selfEvidence) :: transformedParams, transformedBody), transformedHandler)

    case core.Try(_, _) => ErrorReporter.panic("Should not happen. Handle always take block literals as body.")

    // [[ region { {cap}... => s } ]] = region { [ev]{cap}... => s }
    case core.Region(core.BlockLit(tparams, _, _, params, body)) =>
      var environment = env

      // evidence for the region body itself
      val selfEvidence = EvidenceSymbol()

      environment = environment.adapt(Lift.Var(selfEvidence))

      // introduce one evidence symbol per blockparam
      val transformedParams = params map {
        case p @ core.BlockParam(id, tpe, capt) =>
          environment = environment.bind(id)
          transform(p)
      }
      Region(lifted.BlockLit(tparams, Param.EvidenceParam(selfEvidence) :: transformedParams, transform(body)(using environment, ErrorReporter)))

    case core.Region(_) => ErrorReporter.panic("Should not happen. Regions always take block literals as body.")

    case core.App(b: core.Block, targs, vargs, bargs) =>

      // evidence for the function itself
      val ev = env.evidenceFor(b)
      val vargsT = vargs map transform
      val (blockEv, bargsT) = transform(bargs)

      // adds evidence parameters for block arguments
      App(transform(b), targs.map(transform), (ev :: blockEv) ++ vargsT ++ bargsT)

    case core.Get(id, capt, tpe) =>
      Get(id, env.evidenceFor(id), transform(tpe))

    case core.Put(id, capt, value) =>
      Put(id, env.evidenceFor(id), transform(value))

    case core.Scope(definitions, rest) =>
      val env = pretransform(definitions)
      val body = transform(rest)(using env, ErrorReporter)

      Scope(definitions.map(d => transform(d)(using env, ErrorReporter)), body)

    case core.Val(id, binding, body) =>
      Val(id, transform(binding), transform(body))

    case core.Var(id, init, capture, body) =>
      val stateEvidence = EvidenceSymbol()
      val environment = env.adapt(Lift.Var(stateEvidence)).bind(id)
      val stateCapability = lifted.Param.BlockParam(id, lifted.Type.TState(transform(init.tpe)))
      val transformedBody = transform(body)(using environment, ErrorReporter)

      Var(transform(init), lifted.BlockLit(Nil, List(Param.EvidenceParam(stateEvidence), stateCapability),
        transformedBody))

    case core.Alloc(id, init, region, body) =>
      // here the fresh cell uses the same evidence as the region it is allocated into
      val environment = env.bind(id, env.evidenceFor(region).lifts)

      Alloc(id, transform(init), region, env.evidenceFor(region),
        transform(body)(using environment, ErrorReporter))

    case core.Match(scrutinee, clauses, default) =>
      Match(transform(scrutinee),
        clauses.map { case (c, b) => (c.asConstructor, transformBody(b)) },
        default.map { s => transform(s) })

    case core.If(cond, thn, els) =>
      If(transform(cond), transform(thn), transform(els))

    case core.Return(e) =>
      Return(transform(e))

    case core.Hole() => Hole()
  }

  def transform(tree: core.Pure)(using Environment, ErrorReporter): Expr = tree match {
    case core.Literal(value, tpe) =>
      Literal(value, transform(tpe))

    case core.ValueVar(sym, tpe) =>
      ValueVar(sym, transform(tpe))

    case core.PureApp(b, targs, args: List[core.Expr]) =>
      PureApp(transform(b), targs.map(transform), args map transform)

    case core.Make(data, tag, args: List[core.Expr]) =>
      Make(transform(data).asInstanceOf, tag, args map transform)

    case core.Select(target, field, tpe) =>
      Select(transform(target), field, transform(tpe))

    case core.Box(b, _) =>
      Box(transform(b))
  }

  def transform(tree: core.Expr)(using Environment, ErrorReporter): Expr = tree match {
    case core.DirectApp(b: core.Block, targs, vargs, bargs) =>
      val (ev, bargsT) = transform(bargs)
      PureApp(transform(b), targs.map(transform), ev ++ vargs.map(transform) ++ bargsT)

    case core.Run(s) =>
      Run(transform(s))

    case p: core.Pure => transform(p)
  }

  /**
   *  [[ (a){f,...} -> b ]] = [ev,ev_f,...](a){f,...} -> b
   */
  def liftBlockLitTo(b: core.BlockLit)(using Environment, ErrorReporter): BlockLit = b match {
    case core.BlockLit(tps, cps, vps, bps, body) =>
      var environment = env

      // evidence for the block itself
      val selfEvidence = EvidenceSymbol()

      environment = environment.adapt(Lift.Var(selfEvidence))

      // introduce one evidence symbol per blockparam
      val evidenceParams = bps map {
        case core.BlockParam(id, tpe, capt) =>
          val ev = EvidenceSymbol()
          environment = environment.bind(id, Lift.Var(ev))
          Param.EvidenceParam(ev)
      }

      assert(evidenceParams.size == cps.size)

      val transformedParams = vps.map(transform) ++ bps.map(transform)

      BlockLit(tps, Param.EvidenceParam(selfEvidence) :: evidenceParams ++ transformedParams, transform(body)(using environment, ErrorReporter))
  }

  /**
   * Don't transform the block itself, but only the body. Used for local abstractions like match clauses where
   * we know the evidence is Here.
   */
  def transformBody(tree: core.BlockLit)(using Environment, ErrorReporter): BlockLit = tree match {
    case core.BlockLit(tps, cps, vps, bps, body) =>
      BlockLit(tps, (vps ++ bps) map transform, transform(body))
  }

  def transform(args: List[core.Block])(using Environment, ErrorReporter): (List[Evidence], List[Argument]) = {
    var evidence: List[Evidence] = Nil
    val transformedArgs = args map {
      case b: core.BlockVar =>
        evidence = env.evidenceFor(b) :: evidence
        BlockVar(b.id, transform(b.tpe))
      case b: core.Block =>
        evidence = Here() :: evidence
        transform(b)
    }
    (evidence.reverse, transformedArgs)
  }

  /**
   * Traverses the statement to look for function definitions.
   *
   * Important for mutually (and self) recursive functions.
   *
   * TODO add mutual blocks to core and lifted. This way we know exactly what to pretransform.
   */
  def pretransform(s: List[core.Definition])(using env: Environment, E: ErrorReporter): Environment = s match {
    case core.Definition.Def(id, block) :: rest =>
      // will this ever be non-empty???
      val extendedEnv = env.bind(id, env.evidenceFor(block).lifts)
      pretransform(rest)(using extendedEnv, E)
    // even if defs cannot be mutually recursive across lets, we still have to pretransform them.
    case core.Definition.Let(id, _) :: rest =>
      pretransform(rest)
    case Nil => env
  }


  case class Environment(env: Map[Symbol, List[Lift]]) {
    def bind(s: Symbol) = copy(env = env + (s -> Nil))
    def bind(s: Symbol, ev: List[Lift]) = copy(env = env + (s -> ev))
    def bind(s: Symbol, init: Lift) = copy(env = env + (s -> List(init)))
    def adapt(a: Lift) = copy(env = env.map { case (s, as) => s -> (a :: as) })

    def evidenceFor(id: core.Id): Evidence = Evidence(env.getOrElse(id, Nil))

    def evidenceFor(b: core.Block)(using ErrorReporter): Evidence = b match {
      case core.BlockVar(_, tpe, _) if tpe == core.Type.TRegion => Here()
      case b: core.BlockVar => evidenceFor(b.id) //.map { x => Evidence(x) }
      case b: core.BlockLit   => Here()
      case core.Member(b, id, tpe) => evidenceFor(b)
      // TODO check whether this makes any sense
      case b: core.Unbox      => Here()
      case b: core.New => Here()
    }
  }
}
