package effekt
package machine

/**
 * A module declaration
 */
case class Program(declarations: List[Declaration], program: Statement)

/**
 * Toplevel declarations
 */
enum Declaration {
  case DefineForeign(returns: Type, name: String, parameters: Environment, body: String)
  case Include(contents: String)
}
export Declaration.*

/**
  * Names
  */
case class Variable(name: String, tpe: Type)

case class Label(name: String, environment: Environment)

case class Clause(parameters: Environment, body: Statement)

type Environment = List[Variable]

type Substitution = List[(Variable, Variable)]


/**
 * Statements
 */
enum Statement {
  case Def(label: Label, body: Statement, rest: Statement)
  case Jump(label: Label)
  case Substitute(bindings: Substitution, rest: Statement)

  case Let(name: Variable, tag: Int, values: Environment, rest: Statement)
  case Switch(value: Variable, clauses: List[Clause])

  case New(name: Variable, clauses: List[Clause], rest: Statement)
  case Invoke(value: Variable, tag: Int, values: Environment)

  case PushFrame(frame: Clause, rest: Statement)
  case Return(environment: Environment)
  case NewStack(name: Variable, frame: Clause, rest: Statement)
  case PushStack(value: Variable, rest: Statement)
  case PopStack(name: Variable, rest: Statement)

  case Run(command: Command, environment: Environment, continuation: List[Clause])
}
export Statement.*

enum Command {
  case CallForeign(name: String)
  case LiteralInt(n: Int)
}
export Command.*

/**
 * Types
 */
enum Type {
  case Positive(alternatives: List[Signature])
  case Negative(alternatives: List[Signature])
  case Primitive(name: String)
}
export Type.*

type Signature = List[Type]
