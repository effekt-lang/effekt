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


def substitute(value: Variable)(using substitution: Map[Variable, Variable]): Variable =
  substitution.getOrElse(value, value)

def substitute(environment: Environment)(using substitution: Map[Variable, Variable]): Environment =
  environment.map(substitute)

def substitute(clause: Clause)(using substitution: Map[Variable, Variable]): Clause =
  clause match {
    case Clause(parameters, statement) =>
      Clause(parameters, substitute(statement)(using substitution.removedAll(parameters)))
  }

def substitute(statement: Statement)(using substitution: Map[Variable, Variable]): Statement =
  statement match {
    case Jump(label) =>
      // TODO are these correct?
      Jump(label)
    case Substitute(bindings, statement) =>
      // TODO are these correct?
      Substitute(bindings.map { case (variable -> value) => variable -> substitute(value) }, statement)
    case PushFrame(clause, statement) =>
      PushFrame(substitute(clause), substitute(statement))
    case Return(environment) =>
      Return(substitute(environment))
    case Run(command, environment, clauses) =>
      Run(command, substitute(environment), clauses.map(substitute))
  }

