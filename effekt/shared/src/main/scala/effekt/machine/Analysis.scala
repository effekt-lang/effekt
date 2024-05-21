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

def freeVariables(taggedClause: (Int, Clause)): Set[Variable] = freeVariables(taggedClause._2)

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
    case Switch(value, clauses, default: Option[Clause]) =>
      Set(value) ++ clauses.flatMap { case (tag, branch) => freeVariables(branch) } ++ default.map(freeVariables).getOrElse(Set.empty)
    case New(name, clauses, rest) =>
      freeVariables(clauses) ++ (freeVariables(rest) -- Set(name))
    case Invoke(value, tag, values) =>
      Set(value) ++ Set.from(values)
    case Allocate(name, init, region, rest) =>
      freeVariables(rest) ++ Set(init, region) -- Set(name)
    case Load(name, ref, ev, rest) =>
      Set(ref, ev) ++ freeVariables(rest) -- Set(name)
    case Store(ref, value, ev, rest) =>
      Set(ref, value, ev) ++ freeVariables(rest)
    case PushFrame(frame, rest) =>
      freeVariables(frame) ++ freeVariables(rest)
    case Return(values) =>
      Set.from(values)
    case NewStack(name, prompt, frame, rest) =>
      freeVariables(frame) ++ (freeVariables(rest) -- Set(name)) ++ Set(prompt)
    case PushStack(value, rest) =>
      Set(value) ++ freeVariables(rest)
    case PopStacks(name, n, rest) =>
      freeVariables(rest) -- Set(name) ++ Set(n)
    case PopStacksPrompt(name, prompt, rest) =>
      freeVariables(rest) -- Set(name) ++ Set(prompt)
    case FreshPrompt(name, rest) =>
      freeVariables(rest) -- Set(name)
    case ComposeEvidence(name, ev1, ev2, rest) =>
      freeVariables(rest) -- Set(name) ++ Set(ev1, ev2)
    case LiteralInt(name, value, rest) =>
      freeVariables(rest) - name
    case LiteralDouble(name, value, rest) =>
      freeVariables(rest) - name
    case LiteralUTF8String(name, utf8, rest) =>
      freeVariables(rest) - name
    case LiteralEvidence(name, ev, rest) =>
      freeVariables(rest) - name
    case ForeignCall(name, builtin, arguments, rest) =>
      arguments.toSet ++ freeVariables(rest) - name
    case Hole => Set.empty
  }
