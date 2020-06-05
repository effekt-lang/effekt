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

  def transform(tree: Block, self: Option[Symbol] = None)(implicit env: Environment, C: Context): Block = tree match {
    case BlockDef(params, body) =>
      val id = ScopeId()

      val ownBindings = self.map { sym => env.bind(sym) }.getOrElse { env }

      // recursive functions need to bind the own id
      val extendedEnv = params.foldLeft(ownBindings.adapt(ScopeVar(id))) {
        case (env, BlockParam(p)) => env.bind(p)
        case (env, ValueParam(p)) => env
        case (env, ScopeParam(p)) => env
      }
      BlockDef(ScopeParam(id) :: params, transform(body)(extendedEnv, C))
    case Member(body, id) =>
      Member(transform(body), id)
    case e => e
  }

  def transform(tree: Stmt)(implicit env: Environment, C: Context): Stmt = tree match {
    case Def(id, block, rest) =>
      Def(id, transform(block, Some(id)), transform(rest)(env.bind(id), C))

    case Val(id, binding, body) =>
      Val(id, transform(binding), transform(body))

    case Var(id, binding, body) =>
      Var(id, transform(binding), transform(body))

    case Data(id, ctors, rest) =>
      Data(id, ctors, transform(rest))

    case Record(id, fields, rest) =>
      Record(id, fields, transform(rest))

    case Handle(body, handler) =>
      val transformedBody = transform(body) // lift is provided by the handler runtime
      val transformedHandler = handler.map { transform }
      Handle(transformedBody, transformedHandler)

    case App(b: Block, args: List[Argument]) => b match {
      case b: Extern => App(b, liftArguments(args))
      // TODO also for "pure" and "toplevel" functions
      case b: BlockVar if b.id.builtin => App(b, liftArguments(args))
      case b => App(transform(b), env.evidenceFor(b) :: liftArguments(args))
    }

    case Match(scrutinee, clauses) =>
      Match(scrutinee, clauses.map { case (p, b) => (p, transform(b)) }) // the implementation of match should provide evidence

    case If(cond, thn, els) =>
      If(cond, transform(thn), transform(els))
    case While(cond, body) =>
      While(transform(cond), transform(body))
    case Ret(e) =>
      Ret(e)
    case Exports(path, exports) =>
      Exports(path, exports)

    case i: Include => i
    case Hole       => Hole
  }

  // apply lifts to the arguments if it is block variables
  // this is the same as eta expanding them, since then the lifts would be composed for the call
  def liftArguments(args: List[Argument])(implicit env: Environment, C: Context): List[Argument] = args map {
    case b: BlockVar => env.evidenceFor(b) match {
      case Here() => b
      case ev     => Lifted(ev, b)
    }
    case b: Block => transform(b)
    case other    => other
  }

  def transform(h: Handler)(implicit env: Environment, C: Context): Handler = h match {
    case Handler(id, clauses) =>
      Handler(id, clauses.map {
        // effect operations should never take any evidence as they are guaranteed (by design) to be evaluated in
        // their definition context.
        case (op, BlockDef(params, body)) => (op, BlockDef(params, transform(body)))
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
      case b: BlockDef   => Here()
      case Member(b, id) => evidenceFor(b)
      case b: Extern     => sys error "Cannot provide scope evidence for built in function"
      case b: Lifted     => sys error "Should not happen"
    }
  }
}

//
// f...               f = f
// handle {           f = lift(f)
//   def g[ev7] {     f = lift(f)
//     handle {       f = lift(lift(f))
//       f
//     }
//   }
//
// }

//
// f...               f @ List(), ...
// handle {           f @ List(Lift), ...
//   def g[ev7] {     f @ List(ev7, Lift), ...
//     handle {       f @ List(Lift, ev7, Lift), ...
//       f[compose(Lift, ev7, Lift)]()
//     }
//   }
//
// }

// f...
// handle {       f @ Lift
//   map { ev3 => f @ ev3, Lift
//     handle {   f @ Lift, ev3, Lift
//       f[Lift, ev3, Lift]()
//     }
//   }
// }
