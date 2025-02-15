package effekt
package machine

object DirectStyle {

  private var last = 0
  private def fresh() = { last +=1; s"l_${last}" }

  def rewrite(p: Program): Program =
    val Program(decl, stmt) = p
    Program(decl, rewrite(stmt))

  def rewrite(s: Statement): Statement = s match {
    case Statement.PushFrame(Clause(parameters, body), rest) =>
      val rewrittenBody = rewrite(body)
      val rewrittenRest = rewrite(rest)

      if canBeDirect(rewrittenRest) then
        val env: Environment = analysis.freeVariables(rewrittenBody).filterNot { v => parameters.contains(v) }.toList
        val l = Label(fresh(), env ++ parameters)
        Def(l, rewrittenBody, toDirectStyle(rest, env, parameters, l))
      else
        PushFrame(Clause(parameters, rewrittenBody), rewrittenRest)

    // Congruences
    case Statement.Def(label, body, rest) => Def(label, rewrite(body), rewrite(rest))
    case Statement.Jump(label) => Jump(label)
    case Statement.Substitute(bindings, rest) => Substitute(bindings, rewrite(rest))
    case Statement.Construct(name, tag, arguments, rest) => Construct(name, tag, arguments, rewrite(rest))
    case Statement.Switch(scrutinee, clauses, default) =>
      Switch(scrutinee,
             clauses.map { case (idx, Clause(env, body)) => (idx, Clause(env, rewrite(body))) },
             default.map { case Clause(env, body) => Clause(env, rewrite(body)) })
    case Statement.New(name, operations, rest) => New(name, operations, rewrite(rest))
    case Statement.Invoke(receiver, tag, arguments) => Invoke(receiver, tag, arguments)
    case Statement.Var(name, init, returnType, rest) => Var(name, init, returnType, rewrite(rest))
    case Statement.LoadVar(name, ref, rest) => LoadVar(name, ref, rewrite(rest))
    case Statement.StoreVar(ref, value, rest) => StoreVar(ref, value, rewrite(rest))
    case Statement.Return(arguments) => Return(arguments)
    case Statement.Reset(name, frame, rest) => Reset(name, frame, rewrite(rest))
    case Statement.Resume(stack, rest) => Resume(stack, rewrite(rest))
    case Statement.Shift(name, prompt, rest) => Shift(name, prompt, rewrite(rest))
    case Statement.ForeignCall(name, builtin, arguments, rest) => ForeignCall(name, builtin, arguments, rewrite(rest))
    case Statement.LiteralInt(name, value, rest) => LiteralInt(name, value, rewrite(rest))
    case Statement.LiteralDouble(name, value, rest) => LiteralDouble(name, value, rewrite(rest))
    case Statement.LiteralUTF8String(name, utf8, rest) => LiteralUTF8String(name, utf8, rewrite(rest))
    case Statement.Hole => Hole
  }

  def toDirectStyle(stmt: Statement, env: Environment, params: Environment, label: Label): Statement = stmt match {
    case Statement.Return(arguments) => Substitute(env.map(x => x -> x) ++ params.zip(arguments), Jump(label))
    case Statement.Hole => stmt
    case Statement.Jump(label) => stmt
    case Statement.Invoke(receiver, tag, arguments) => stmt

    case Statement.PushFrame(frame, rest) => stmt

    case Statement.Reset(name, frame, rest) => Reset(name, frame, rewrite(rest))
    case Statement.Resume(stack, rest) => Resume(stack, rewrite(rest))
    case Statement.Shift(name, prompt, rest) => Shift(name, prompt, rewrite(rest))

    // Congruences
    case Statement.Def(label2, body, rest) => Def(label2, body, toDirectStyle(rest, env, params, label))
    case Statement.Substitute(bindings, rest) => Substitute(bindings, toDirectStyle(rest, env, params, label))
    case Statement.Construct(name, tag, arguments, rest) => Construct(name, tag, arguments, toDirectStyle(rest, env, params, label))
    case Statement.Switch(scrutinee, clauses, default) =>
      Switch(scrutinee,
             clauses.map { case (idx, Clause(env, body)) => (idx, Clause(env, toDirectStyle(body, env, params, label))) },
             default.map { case Clause(env, body) => Clause(env, toDirectStyle(body, env, params, label)) })
    case Statement.New(name, operations, rest) => New(name, operations, toDirectStyle(rest, env, params, label))
    case Statement.Var(name, init, returnType, rest) => Var(name, init, returnType, toDirectStyle(rest, env, params, label))
    case Statement.LoadVar(name, ref, rest) => LoadVar(name, ref, toDirectStyle(rest, env, params, label))
    case Statement.StoreVar(ref, value, rest) => StoreVar(ref, value, toDirectStyle(rest, env, params, label))
    case Statement.ForeignCall(name, builtin, arguments, rest) => ForeignCall(name, builtin, arguments, toDirectStyle(rest, env, params, label))
    case Statement.LiteralInt(name, value, rest) => LiteralInt(name, value, toDirectStyle(rest, env, params, label))
    case Statement.LiteralDouble(name, value, rest) => LiteralDouble(name, value, toDirectStyle(rest, env, params, label))
    case Statement.LiteralUTF8String(name, utf8, rest) => LiteralUTF8String(name, utf8, toDirectStyle(rest, env, params, label))
  }

  def canBeDirect(stmt: Statement): Boolean = stmt match {
    case Statement.Return(arguments) => true
    case Statement.Hole => true

    case Statement.Jump(label) => false
    case Statement.Invoke(receiver, tag, arguments) => false
    case Statement.PushFrame(frame, rest) => false

    case Statement.Reset(name, frame, rest) => false // canBeDirect(rest) // ???
    case Statement.Resume(stack, rest) => false // canBeDirect(rest) // ???
    case Statement.Shift(name, prompt, rest) => false // canBeDirect(rest) // ???

    // Congruences
    case Statement.Def(label, body, rest) => canBeDirect(rest)
    case Statement.Substitute(bindings, rest) => canBeDirect(rest)
    case Statement.Construct(name, tag, arguments, rest) => canBeDirect(rest)
    case Statement.Switch(scrutinee, clauses, default) =>
      clauses.forall { case (idx, Clause(_, body)) => canBeDirect(body) } && default.forall { case Clause(_, body) => canBeDirect(body) }
    case Statement.New(name, operations, rest) => canBeDirect(rest)
    case Statement.Var(name, init, returnType, rest) => canBeDirect(rest)
    case Statement.LoadVar(name, ref, rest) => canBeDirect(rest)
    case Statement.StoreVar(ref, value, rest) => canBeDirect(rest)
    case Statement.ForeignCall(name, builtin, arguments, rest) => canBeDirect(rest)
    case Statement.LiteralInt(name, value, rest) => canBeDirect(rest)
    case Statement.LiteralDouble(name, value, rest) => canBeDirect(rest)
    case Statement.LiteralUTF8String(name, utf8, rest) => canBeDirect(rest)
  }
}
