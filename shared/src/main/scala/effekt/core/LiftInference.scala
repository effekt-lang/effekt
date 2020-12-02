package effekt
package core

import effekt.symbols.Symbol
import effekt.context.Context

class LiftInference extends Phase[ModuleDecl, ModuleDecl] {

  def run(mod: ModuleDecl)(implicit C: Context): Option[ModuleDecl] =
    Some(transform(mod)(Environment(Map.empty), C))

  // TODO either resolve and bind imports or use the knowledge that they are toplevel!
  def transform(mod: ModuleDecl)(implicit env: Environment, C: Context): ModuleDecl =
    mod.copy(defs = transform(mod.defs))

  // [[ a -> b ]] = [ev] -> a -> b if a is value
  // [[ a -> b ]] = [ev] -> a -> [ev] -> b if a is block
  def transform(tree: Block, self: Option[Symbol] = None)(implicit env: Environment, C: Context): Block = tree match {
    case BlockLit(params, body) =>

      val selfExtendedEnv = self.map { sym => env.bind(sym) }.getOrElse { env }
      val (extendedParams, extendedEnv) = transform(params, env)

      BlockLit(extendedParams, transform(body)(extendedEnv, C))

    case Member(body, field) => Member(transform(body), field)
    case Extern(params, body) =>
      // For extern functions we don't want to add an evidence parameter.
      // Except for `measure`, which takes a block and will be called with evidence
      // for this block
      val (extendedParams, _) = transform(params, env)
      Extern(extendedParams.tail, body)
    case e => e
  }

  def transform(params: List[Param], env: Environment): (List[Param], Environment) = {
    val emptyParams: List[Param] = Nil
    val selfScope = ScopeId()
    val adaptedEnv = env.adapt(ScopeVar(selfScope))

    val (extendedParams, extendedEnv) = params.foldRight((emptyParams, adaptedEnv)) {
      case (BlockParam(p), (params, env)) =>
        val paramScope = ScopeId();
        (BlockParam(p) :: ScopeParam(paramScope) :: params, env.bind(p, ScopeVar(paramScope)))
      case (ValueParam(p), (params, env)) =>
        (ValueParam(p) :: params, env)
      case (ScopeParam(_), _) =>
        sys error "should not happen"
    }
    (ScopeParam(selfScope) :: extendedParams, extendedEnv)
  }

  def transform(tree: Stmt)(implicit env: Environment, C: Context): Stmt = tree match {
    case Def(id, block, rest) =>
      Def(id, transform(block, Some(id)), transform(rest)(env.bind(id), C))

    case Val(id, binding, body) =>
      Val(id, transform(binding), transform(body))

    case State(id, get, put, init, bodyBlock) => bodyBlock match {
      case BlockLit(params, body) =>
        val stateScope = ScopeId()
        val adaptedEnv = env.adapt(ScopeVar(stateScope))
        val extendedEnv = params.foldRight(adaptedEnv) { (p, e) =>
          e.bind(p.id)
        }
        val transformedBody = transform(body)(extendedEnv, C)
        State(id, get, put, transform(init), BlockLit(ScopeParam(stateScope) :: params, transformedBody))
      case _ => ???
    }

    case Data(id, ctors, rest) =>
      Data(id, ctors, transform(rest))

    case Record(id, fields, rest) =>
      Record(id, fields, transform(rest))

    case Handle(bodyBlock, handler) => bodyBlock match {
      case BlockLit(params, body) => {
        val handlerScope = ScopeId()
        val adaptedEnv = env.adapt(ScopeVar(handlerScope))
        val extendedEnv = params.foldRight(adaptedEnv) { (p, e) =>
          e.bind(p.id)
        }
        val transformedBody = transform(body)(extendedEnv, C)
        // lift is provided by the handler runtime
        val transformedHandler = handler.map { transform }
        Handle(
          BlockLit(ScopeParam(handlerScope) :: params, transformedBody),
          transformedHandler
        )
      }
      case _ => ???
    }

    case App(b: Block, args: List[Argument]) => b match {
      case b: Extern => App(b, expandArguments(args))
      // TODO also for "pure" and "toplevel" functions
      case b: BlockVar if b.id.builtin => App(b, expandArguments(args))

      // [[ Eff.op(arg) ]] = Eff.op(ev,arg)
      // [[ fun(arg) ]] = fun(ev,arg)
      case b => App(transform(b), env.evidenceFor(b) :: expandArguments(args))
    }

    case Match(scrutinee, clauses) =>
      Match(scrutinee, clauses.map { case (p, b) => (p, transformBody(b)) })

    case If(cond, thn, els) =>
      If(cond, transform(thn), transform(els))

    case While(cond, body) =>
      While(transform(cond), transform(body))

    case Ret(e) =>
      Ret(e)

    case Exports(path, exports) =>
      Exports(path, exports)

    case Include(contents, rest) => Include(contents, transform(rest))

    case Hole                    => Hole
  }

  /**
   * Don't transform the block itself, but only the body. Used for local abstractions like match clauses where
   * we know the evidence is Here.
   */
  def transformBody(tree: BlockLit)(implicit env: Environment, C: Context): BlockLit = tree match {
    case BlockLit(params, body) =>
      BlockLit(params, transform(body))
  }

  // expand the list of arguments to also pass evidence
  def expandArguments(args: List[Argument])(implicit env: Environment, C: Context): List[Argument] = {
    val emptyArgs: List[Argument] = Nil
    args.foldRight(emptyArgs) {
      case (b: BlockVar, args) => b :: env.evidenceFor(b) :: args
      case (b: Block, args)    => transform(b) :: env.evidenceFor(b) :: args
      case (other, args)       => other :: args
    }
  }

  def transform(h: Handler)(implicit env: Environment, C: Context): Handler = h match {
    case Handler(id, clauses) =>
      Handler(id, clauses.map {
        case (op, BlockLit(params, body)) =>
          (op, BlockLit(ScopeParam(ScopeId()) :: params, transform(body)))
      })
  }

  case class Environment(env: Map[Symbol, List[Scope]]) {
    def bind(s: Symbol) = copy(env = env + (s -> Nil))
    def bind(s: Symbol, init: Scope) = copy(env = env + (s -> List(init)))
    def adapt(a: Scope) = copy(env = env.map { case (s, as) => s -> (a :: as) })

    def evidenceFor(b: Block): Scope = b match {
      case b: BlockVar => env.getOrElse(b.id, Nil) match {
        case Nil      => Here()
        case s :: Nil => s
        case scopes   => Nested(scopes)
      }
      case b: BlockLit   => Here()
      case Member(b, id) => evidenceFor(b)
      case b: Extern     => sys error "Cannot provide scope evidence for built in function"
    }
  }
}
