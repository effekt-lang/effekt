package effekt
package machine

/**
 * Variables stand for values
 */
case class Variable(name: String, tpe: Type)

/**
 * An environment is an ordered list of type-annotated variable names.
 * 
 * Used to represent contexts, free variables, parameters, ...
 */
type Environment = List[Variable]

/**
 * Labels
 * 
 *   def l = { s1 }; s2
 * 
 * Here l is the label and [[environment]] is the list of free variables of s1.
 * It thus can be understood as the type of the label.
 */
case class Label(name: String, environment: Environment)

/**
 * Applying a substitution 
 *  
 *    List(x -> y)
 *
 * will replace all occurrences of x by y. Here x is a binding occurrence 
 * and y is a bound occurrence.
 */
type Substitution = List[(Variable, Variable)]

type Tag = Int


/**
 * A module declaration
 */
case class Program(declarations: List[Declaration], program: Statement)

/**
 * Toplevel declarations for FFI
 */
enum Declaration {
  case Extern(name: String, parameters: Environment, returnType: Type, body: String)
  case Include(contents: String)
}
export Declaration.*


/**
 * Clauses are parametrized statements 
 * 
 * e.g. { (x1...) => s }
 * 
 * The parameters (x1, etc.) are binding occurrences.
 * 
 *    Gamma ++ Delta |- s
 *    -----------------------
 *    Gamma |- { Delta => s }
 * 
 * In the typing rule Delta corresponds to [[parameters]].
 */
case class Clause(parameters: Environment, body: Statement)

/**
 * Statements
 */
enum Statement {

  /**
   * e.g. def l = { s1 }; s2
   */
  case Def(label: Label, body: Statement, rest: Statement)

  /**
   * e.g. jump l
   */
  case Jump(label: Label)

  /**
   * e.g. s[x1 -> v1, ...]
   */
  case Substitute(bindings: Substitution, rest: Statement)

  /**
   * e.g. let x = make C(v1, ...); s
   */
  case Construct(name: Variable, tag: Int, arguments: Environment, rest: Statement)

  /**
   * e.g. switch v { (x1, ...) => s1; ... }
   */
  case Switch(scrutinee: Variable, clauses: List[Clause])

  /**
   * e.g. let x = new { (x1, ...) => s1; ... }; s
   */
  case New(name: Variable, operations: List[Clause], rest: Statement)

  /**
   * e.g. v.m(v1, ...)
   */
  case Invoke(receiver: Variable, tag: Tag, arguments: Environment)

  /**
   * e.g. push { (x, ...) => s }; s
   */
  case PushFrame(frame: Clause, rest: Statement)

  /**
   * e.g. return (v1: t1, ...)
   */
  case Return(arguments: Environment)

  /**
   * e.g. let k = stack { (x, ...) => s }; s
   */
  case NewStack(name: Variable, frame: Clause, rest: Statement)

  /**
   * e.g. push k; s
   */
  case PushStack(stack: Variable, rest: Statement)

  /**
   * e.g. let k = shift0; s  
   */
  case PopStack(name: Variable, rest: Statement)

  /**
   * let x = #infix_add(v1, ...); s
   */
  case ForeignCall(name: Variable, builtin: String, arguments: Environment, rest: Statement)

  /**
   * let x = 42; s
   */
  case LiteralInt(name: Variable, value: Int, rest: Statement)
}
export Statement.*


/**
 * Types
 */
enum Type {
  case Positive(alternatives: List[Signature])
  case Negative(alternatives: List[Signature])
  case Stack()
  case Int()
}
export Type.{ Positive, Negative }


type Signature = List[Type]

object builtins {
  /**
   * Blocks types are interfaces with a single operation.
   */
  val Apply: Tag = 0

  val Unit: Tag = 0
  val UnitType = Positive(List(List()))

  val True: Tag = 1
  val False: Tag = 0
  val BooleanType = Positive(List(List(), List()))

  val SingletonRecord: Tag = 0
}