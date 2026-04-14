package effekt
package cpsds

import effekt.core.Id

class TestRenamer {

  private var scopes: List[Map[Id, Id]] = List.empty
  private var toplevelScope: Map[Id, Id] = Map.empty
  private var counter: Int = 0

  private def freshIdFor(id: Id): Id = {
    val n = counter
    counter += 1
    Id(id.name.name, n)
  }

  private def withBindings[R](ids: List[Id])(f: => R): R = {
    val before = scopes
    try {
      val newScope = ids.map(x => x -> freshIdFor(x)).toMap
      scopes = newScope :: scopes
      f
    } finally { scopes = before }
  }

  private def withBinding[R](id: Id)(f: => R): R = withBindings(List(id))(f)

  private def resolveId(id: Id): Id = {
    scopes.collectFirst {
      case bnds if bnds.contains(id) => bnds(id)
    }.getOrElse {
      if toplevelScope.contains(id) then toplevelScope(id)
      else id
    }
  }

  // --- Rewrite ---

  def rewrite(id: Id): Id = resolveId(id)

  def rewrite(e: Expr): Expr = e match {
    case Expr.Variable(id) => Expr.Variable(rewrite(id))
    case Expr.Literal(_, _) => e
    case Expr.Make(data, tag, vargs) => Expr.Make(data, rewrite(tag), vargs.map(rewrite))
    case Expr.Abort => e
    case Expr.Return => e
    case Expr.Toplevel => e
  }

  def rewrite(s: Stmt): Stmt = s match {
    case Stmt.Def(id, params, body, rest) =>
      withBinding(id) {
        val newId = rewrite(id)
        withBindings(params) {
          Stmt.Def(newId, params.map(rewrite), rewrite(body), rewrite(rest))
        }
      }

    case Stmt.New(id, interface, operations, rest) =>
      withBinding(id) {
        Stmt.New(rewrite(id), interface, operations.map(rewrite), rewrite(rest))
      }

    case Stmt.Let(id, binding, rest) =>
      val newBinding = rewrite(binding)
      withBinding(id) {
        Stmt.Let(rewrite(id), newBinding, rewrite(rest))
      }

    case Stmt.App(id, args, direct) =>
      Stmt.App(rewrite(id), args.map(rewrite), direct)

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(rewrite(id), rewrite(method), args.map(rewrite))

    case Stmt.Run(id, callee, args, purity, rest) =>
      val newCallee = rewrite(callee)
      val newArgs = args.map(rewrite)
      withBinding(id) {
        Stmt.Run(rewrite(id), newCallee, newArgs, purity, rewrite(rest))
      }

    case Stmt.If(cond, thn, els) =>
      Stmt.If(rewrite(cond), rewrite(thn), rewrite(els))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(rewrite(scrutinee),
        clauses.map { case (tag, cl) => (rewrite(tag), rewrite(cl)) },
        default.map(rewrite))

    case Stmt.Region(id, ks, rest) =>
      val newKs = rewrite(ks)
      withBinding(id) {
        Stmt.Region(rewrite(id), newKs, rewrite(rest))
      }

    case Stmt.Alloc(id, init, region, rest) =>
      val newInit = rewrite(init)
      val newRegion = rewrite(region)
      withBinding(id) {
        Stmt.Alloc(rewrite(id), newInit, newRegion, rewrite(rest))
      }

    case Stmt.Var(id, init, ks, rest) =>
      val newInit = rewrite(init)
      val newKs = rewrite(ks)
      withBinding(id) {
        Stmt.Var(rewrite(id), newInit, newKs, rewrite(rest))
      }

    case Stmt.Dealloc(ref, rest) =>
      Stmt.Dealloc(rewrite(ref), rewrite(rest))

    case Stmt.Get(ref, id, rest) =>
      val newRef = rewrite(ref)
      withBinding(id) {
        Stmt.Get(newRef, rewrite(id), rewrite(rest))
      }

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(rewrite(ref), rewrite(value), rewrite(rest))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      val newKs1 = rewrite(ks1)
      val newK1 = rewrite(k1)
      withBindings(List(p, ks, k)) {
        Stmt.Reset(rewrite(p), rewrite(ks), rewrite(k), rewrite(body), newKs1, newK1)
      }

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      val newPrompt = rewrite(prompt)
      val newKs1 = rewrite(ks1)
      val newK1 = rewrite(k1)
      withBindings(List(resume, ks, k)) {
        Stmt.Shift(newPrompt, rewrite(resume), rewrite(ks), rewrite(k), rewrite(body), newKs1, newK1)
      }

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      val newR = rewrite(r)
      val newKs1 = rewrite(ks1)
      val newK1 = rewrite(k1)
      withBindings(List(ks, k)) {
        Stmt.Resume(newR, rewrite(ks), rewrite(k), rewrite(body), newKs1, newK1)
      }

    case h: Stmt.Hole => h
  }

  def rewrite(op: Operation): Operation = op match {
    case Operation(name, params, body) =>
      withBindings(params) {
        Operation(rewrite(name), params.map(rewrite), rewrite(body))
      }
  }

  def rewrite(cl: Clause): Clause = cl match {
    case Clause(params, body) =>
      withBindings(params) {
        Clause(params.map(rewrite), rewrite(body))
      }
  }

  def rewrite(d: ToplevelDefinition): ToplevelDefinition = d match {
    case ToplevelDefinition.Def(id, params, body) =>
      withBindings(params) {
        ToplevelDefinition.Def(rewrite(id), params.map(rewrite), rewrite(body))
      }

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      withBindings(List(ks, k)) {
        ToplevelDefinition.Val(rewrite(id), rewrite(ks), rewrite(k), rewrite(binding))
      }

    case ToplevelDefinition.Let(id, binding) =>
      ToplevelDefinition.Let(rewrite(id), rewrite(binding))
  }

  private def collectToplevelIds(m: ModuleDecl): List[Id] =
    m.definitions.map {
      case ToplevelDefinition.Def(id, _, _) => id
      case ToplevelDefinition.Val(id, _, _, _) => id
      case ToplevelDefinition.Let(id, _) => id
    }

  def apply(m: ModuleDecl): ModuleDecl = {
    counter = 0
    scopes = List.empty
    toplevelScope = collectToplevelIds(m).map(id => id -> freshIdFor(id)).toMap
    m.copy(definitions = m.definitions.map(rewrite))
  }

  def apply(s: Stmt): Stmt = {
    counter = 0
    scopes = List.empty
    toplevelScope = Map.empty
    rewrite(s)
  }
}
