package effekt
package machine
package analysis

def freeVariables(clauses: List[Clause]): Set[Variable] = {
  clauses.flatMap(freeVariables).toSet
}

def freeVariables(clause: Clause): Set[Variable] =
  clause match {
    case Clause(parameters, body) => freeVariables(body) -- parameters.toSet
  }

def freeVariables(statement: Statement): Set[Variable] =
  statement match {
    case Def(_, _, rest) =>
      freeVariables(rest)
    case Jump(Label(_, environment)) =>
      environment.toSet
    case Substitute(bindings, rest) =>
      freeVariables(rest) -- bindings.map(_._1).toSet ++ bindings.map(_._2).toSet
    case Construct(name, tag, values, rest) =>
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
    case LiteralInt(name, value, rest) =>
      freeVariables(rest) - name
    case LiteralDouble(name, value, rest) =>
      freeVariables(rest) - name
    case LiteralUTF8String(name, utf8, rest) =>
      freeVariables(rest) - name
    case ForeignCall(name, builtin, arguments, rest) =>
      arguments.toSet ++ freeVariables(rest) - name
  }
