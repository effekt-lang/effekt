package effekt
package machine
package analysis

import scala.collection.immutable.ListSet

// Free variables in order of first occurrence for stability (used by codegen to determine param/arg/env order)
def freeVariables(clauses: List[Clause]): ListSet[Variable] = {
  ListSet.from(clauses.flatMap(freeVariables))
}

def freeVariables(clause: Clause): ListSet[Variable] =
  clause match {
    case Clause(parameters, body) => freeVariables(body) -- parameters.toSet
  }

def freeVariables(taggedClause: (Int, Clause)): ListSet[Variable] = freeVariables(taggedClause._2)

def freeVariables(statement: Statement): ListSet[Variable] =
  statement match {
    case Jump(_, arguments) =>
      ListSet.from(arguments)
    case Construct(name, tag, values, rest) =>
      ListSet.from(values) ++ (freeVariables(rest) -- Set(name))
    case Switch(value, clauses, default: Option[Clause]) =>
      ListSet(value) ++ clauses.flatMap(freeVariables) ++ default.map(freeVariables).getOrElse(ListSet.empty)
    case New(name, clauses, rest) =>
      freeVariables(clauses) ++ (freeVariables(rest) -- Set(name))
    case Invoke(value, tag, values) =>
      ListSet(value) ++ ListSet.from(values)
    case Var(name, init, returnTypes, rest) =>
      ListSet(init) ++ (freeVariables(rest) -- Set(name))
    case LoadVar(name, ref, rest) =>
      ListSet(ref) ++ (freeVariables(rest) -- Set(name))
    case StoreVar(ref, value, rest) =>
      ListSet(ref, value) ++ freeVariables(rest)
    case PushFrame(frame, rest) =>
      freeVariables(frame) ++ freeVariables(rest)
    case Return(values) =>
      ListSet.from(values)
    case Reset(prompt, frame, rest) =>
      freeVariables(frame) ++ (freeVariables(rest) -- Set(prompt))
    case Resume(value, rest) =>
      ListSet(value) ++ freeVariables(rest)
    case Shift(name, prompt, rest) =>
      ListSet(prompt) ++ (freeVariables(rest) -- Set(name))
    case LiteralInt(name, value, rest) =>
      freeVariables(rest) - name
    case LiteralByte(name, value, rest) =>
      freeVariables(rest) - name
    case LiteralDouble(name, value, rest) =>
      freeVariables(rest) - name
    case LiteralUTF8String(name, utf8, rest) =>
      freeVariables(rest) - name
    case ForeignCall(name, builtin, arguments, rest) =>
      ListSet.from(arguments) ++ (freeVariables(rest) - name)
    case ForeignJump(builtin, arguments) =>
      ListSet.from(arguments)
    case Coerce(name, value, rest) =>
      ListSet(value) ++ (freeVariables(rest) - name)
    case Hole(_) => ListSet.empty
  }
