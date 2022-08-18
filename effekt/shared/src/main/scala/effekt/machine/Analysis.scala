package effekt.machine.analysis

import effekt.machine.*

def freeVariables(clauses: List[Clause]): Set[Variable] = {
  clauses.flatMap(freeVariables).toSet
}

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
    case Let(name, tag, values, rest) =>
      Set.from(values) ++ (freeVariables(rest) -- Set(name))
    case Switch(value, clauses) =>
      Set(value) ++ freeVariables(clauses)
    case New(name, clauses, rest) =>
      freeVariables(clauses) ++ (freeVariables(rest) -- Set(name))
    case Invoke(value, tag, values) =>
      Set(value) ++ Set.from(values)
    case PushFrame(frame, rest) =>
      freeVariables(frame) ++ freeVariables(rest)
    case Return(values) =>
      Set.from(values)
    case NewStack(name, frame, rest) =>
      freeVariables(frame) ++ (freeVariables(rest) -- Set(name))
    case PushStack(value, rest) =>
      Set(value) ++ freeVariables(rest)
    case PopStack(name, rest) =>
      freeVariables(rest) -- Set(name)
    case Run(_, environment, continuation) =>
      environment.toSet ++ continuation.flatMap(freeVariables)
  }

def substitute(subst: Substitution, v: Variable): Variable =
  subst.findLast { case (k, _) => k == v }.map { _._2 }.getOrElse(v)

def substitute(subst: Substitution, into: Substitution): Substitution = into map {
  case (k, v) => (k, substitute(subst, v))
}

def substitute(subst: Substitution, stmt: Statement): Statement = stmt match {
  case Def(label, body, rest) => substitute(subst, stmt)
  case Jump(label) => Substitute(subst, Jump(label))
  // we float substitutions downwards...
  case Substitute(bindings, rest) => substitute(substitute(subst, bindings) ++ subst, rest)
  case Let(name, tag, values, rest) => ???
  case Switch(value, clauses) => ???
  case New(name, clauses, rest) => ???
  case Invoke(value, tag, values) => ???
  case PushFrame(frame, rest) => ???
  case Return(environment) => ???
  case NewStack(name, frame, rest) => ???
  case PushStack(value, rest) => ???
  case PopStack(name, rest) => ???
  case Run(command, environment, continuation) => ???
}