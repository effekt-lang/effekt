package effekt
package core

import effekt.symbols.Symbol
import effekt.context.Context

class LiftInference extends Phase[ModuleDecl, ModuleDecl] {

  def run(mod: ModuleDecl)(implicit C: Context): Option[ModuleDecl] =
    Some(transform(mod)(Environment(Map.empty)))

  // TODO either resolve and bind imports or use the knowledge that they are toplevel!
  def transform(mod: ModuleDecl)(implicit env: Environment): ModuleDecl =
    mod.copy(defs = transform(mod.defs))

  def transform(tree: Block)(implicit env: Environment): Block = tree match {
    case BlockDef(params, body) =>
      val id = ScopeId()
      val extendedEnv = params.foldLeft(env.adapt(ScopeVar(id))) {
        case (env, BlockParam(p)) => env.bind(p)
        case (env, ValueParam(p)) => env
        case (env, ScopeParam(p)) => env
      }
      BlockDef(ScopeParam(id) :: params, transform(body)(extendedEnv))
    case Member(body, id) =>
      Member(transform(body), id)
    case e => e
  }

  def transform(tree: Stmt)(implicit env: Environment): Stmt = tree match {
    case Def(id, block, rest) =>
      Def(id, transform(block), transform(rest)(env.bind(id)))

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
      case b: Extern => App(b, args)
      case b         => App(transform(b), env.evidenceFor(b) :: args)
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

  def transform(h: Handler)(implicit env: Environment): Handler = h match {
    case Handler(id, clauses) =>
      Handler(id, clauses.map { case (op, impl) => (op, transform(impl).asInstanceOf[BlockDef]) })
  }

  case class Environment(env: Map[Symbol, List[Scope]]) {
    def bind(s: Symbol) = copy(env = env + (s -> Nil))
    def adapt(a: Scope) = copy(env = env.map { case (s, as) => s -> (a :: as) })

    def evidenceFor(b: Block): Scope = b match {
      case b: BlockVar => env.getOrElse(b.id, Nil) match {
        case Nil    => Here()
        case scopes => Nested(scopes)
      }
      case b: BlockDef   => Here()
      case Member(b, id) => evidenceFor(b)
      case b: Extern     => sys error "Cannot provide scope evidence for built in function"
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
