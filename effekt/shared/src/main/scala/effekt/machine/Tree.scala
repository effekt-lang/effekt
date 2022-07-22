package effekt
package machine

/**
 * A module declaration
 */
case class Program(declarations: List[Declaration], program: Statement)

/**
 * Toplevel declarations
 */
sealed trait Declaration

case class DefineForeign(returns: Type, name: String, parameters: Environment, body: String) extends Declaration
case class Include(contents: String) extends Declaration

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
sealed trait Statement
case class Def(label: Label, body: Statement, rest: Statement) extends Statement
case class Jump(label: Label) extends Statement
case class Substitute(bindings: Substitution, rest: Statement) extends Statement

case class Let(name: Variable, tag: Int, environment: Environment, rest: Statement) extends Statement
case class Switch(value: Variable, clauses: List[Clause]) extends Statement

case class New(name: Variable, clauses: List[Clause], rest: Statement) extends Statement
case class Invoke(value: Variable, tag: Int, environment: Environment) extends Statement

case class PushFrame(frame: Clause, rest: Statement) extends Statement
case class Return(environment: Environment) extends Statement
case class Run(command: Command, environment: Environment, continuation: List[Clause]) extends Statement

sealed trait Command
case class CallForeign(name: String) extends Command
case class LiteralInt(n: Int) extends Command

/**
 * Types
 */
sealed trait Type
case class Positive(alternatives: List[Signature]) extends Type
case class Negative(alternatives: List[Signature]) extends Type
case class Primitive(name: String) extends Type

type Signature = List[Type]

