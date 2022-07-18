package effekt.machine

def freeVariables(clause: Clause): Set[Variable] =
  clause match {
    case Clause(parameters, body) => freeVariables(body) -- parameters.toSet
  }

def freeVariables(statement: Statement): Set[Variable] =
  statement match {
    case Jump(Label(_, environment)) =>
      environment.toSet
    case Substitute(bindings, rest) =>
      freeVariables(rest) -- bindings.map(_._1).toSet ++ bindings.map(_._2).toSet
    case PushFrame(frame, rest) =>
      freeVariables(frame) ++ freeVariables(rest)
    case Return(environment) =>
      environment.toSet
    case Run(_, environment, continuation) =>
          environment.toSet ++ continuation.flatMap(freeVariables)
  }
