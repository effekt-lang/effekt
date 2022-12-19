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
 *    Gamma ++ Delta |- s
 *    -----------------------
 *    Gamma |- { Delta => s }
 *
 * In the typing rule Delta corresponds to [[parameters]].
 */
case class Clause(parameters: Environment, body: Statement)


/**
 * Statements
 *
 * ----------[[ effekt.machine.Statement ]]----------
 *
 *   ─ [[ Statement ]]
 *     │─ [[ Def ]]
 *     │─ [[ Jump ]]
 *     │─ [[ Substitute ]]
 *     │─ [[ Construct ]]
 *     │─ [[ Switch ]]
 *     │─ [[ New ]]
 *     │─ [[ Invoke ]]
 *     │─ [[ Allocate ]]
 *     │─ [[ Load ]]
 *     │─ [[ Store ]]
 *     │─ [[ PushFrame ]]
 *     │─ [[ Return ]]
 *     │─ [[ NewStack ]]
 *     │─ [[ PushStack ]]
 *     │─ [[ PopStacks ]]
 *     │─ [[ ForeignCall ]]
 *     │─ [[ ComposeEvidence ]]
 *     │─ [[ LiteralInt ]]
 *     │─ [[ LiteralDouble ]]
 *     │─ [[ LiteralUTF8String ]]
 *     │─ [[ LiteralEvidence ]]
 *
 * --------------------------------------------------
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
  case Switch(scrutinee: Variable, clauses: List[(Int, Clause)], default: Option[Clause])

  /**
   * e.g. let x = new { (x1, ...) => s1; ... }; s
   */
  case New(name: Variable, operations: List[Clause], rest: Statement)

  /**
   * e.g. v.m(v1, ...)
   */
  case Invoke(receiver: Variable, tag: Tag, arguments: Environment)

  /**
  *  e.g. Int x in r = 42; s
  */
  case Allocate(name: Variable, init: Variable, region: Variable, rest: Statement)

  /**
  * e.g. y = *x; s
  */
  case Load(name: Variable, ref: Variable, rest: Statement)

  /**
  * e.g. *x = 42; s
  */
  case Store(ref: Variable, value: Variable, rest: Statement)

  /**
   * e.g. push { (x, ...) => s }; s
   */
  case PushFrame(frame: Clause, rest: Statement)

  /**
   * e.g. return (v1: t1, ...)
   */
  case Return(arguments: Environment)

  /**
   * e.g. let k = stack with region r { (x, ...) => s }; s
   */
  case NewStack(name: Variable, region: Variable, frame: Clause, rest: Statement)

  /**
   * e.g. push k; s
   */
  case PushStack(stack: Variable, rest: Statement)

  /**
   * e.g. let k = shift0 (n+1); s
   * NOTE: Pops the stacks until the nth, i.e. the first n+1 ones
   */
  case PopStacks(name: Variable, n: Variable, rest: Statement)

  /**
   * let x = #infix_add(v1, ...); s
   */
  case ForeignCall(name: Variable, builtin: String, arguments: Environment, rest: Statement)

  /**
   * Evidence composition, i.e. currently:
   * let x = ev1 + ev2; s
   */
  case ComposeEvidence(name: Variable, ev1: Variable, ev2: Variable, rest: Statement)

  /**
   * let x = 42; s
   */
  case LiteralInt(name: Variable, value: Int, rest: Statement)

  case LiteralDouble(name: Variable, value: Double, rest: Statement)
  case LiteralUTF8String(name: Variable, utf8: Array[Byte], rest: Statement)
  case LiteralEvidence(name: Variable, value: Evidence, rest: Statement)
}
export Statement.*


/**
 * Types
 */
enum Type {
  case Extern(name: Predef.String)
  case Positive(name: Predef.String)
  case Negative(name: Predef.String)
  case Stack()
  case Reference(tpe: Type)
  case Region()
}
export Type.{ Positive, Negative }

type Evidence = Int
object builtins {

  val Evidence = Type.Extern("%Int 8")
  val Here: Evidence = 0
  val There: Evidence = 1

  /**
   * Blocks types are interfaces with a single operation.
   */
  val Apply: Tag = 0

  val Unit: Tag = 0
  val UnitType = Positive("Unit")

  val True: Tag = 1
  val False: Tag = 0
  val BooleanType = Positive("Boolean")

  val SingletonRecord: Tag = 0
}
