package effekt
package machine

import effekt.source.FeatureFlag

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
  case Extern(name: String, parameters: Environment, returnType: Type, async: Boolean, body: ExternBody)
  case Include(featureFlag: FeatureFlag, contents: String)
}
export Declaration.*

sealed trait ExternBody
object ExternBody {
  case class StringExternBody(featureFlag: FeatureFlag, contents: Template[Variable]) extends ExternBody
  case class Unsupported(err: util.messages.EffektError) extends ExternBody {
    def report(using E: util.messages.ErrorReporter): Unit = E.report(err)
  }
}

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
 *     │─ [[ LiteralInt ]]
 *     │─ [[ LiteralDouble ]]
 *     │─ [[ LiteralUTF8String ]]
 *     │─ [[ Hole ]]
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
   * e.g. let x = allocate(42, region); s
   */
  case Allocate(name: Variable, init: Variable, region: Variable, rest: Statement)

  /**
   * e.g. let y = load(x); s
   */
  case Load(name: Variable, reference: Variable, rest: Statement)

  /**
   * e.g. store(x, 42); s
   */
  case Store(reference: Variable, value: Variable, rest: Statement)

  /**
   * e.g. var x = 42; s
   */
  case Var(name: Variable, init: Variable, returnType: Type, rest: Statement)

  /**
   * e.g. let y = loadVar(x); s
   */
  case LoadVar(name: Variable, ref: Variable, rest: Statement)

  /**
   * e.g. storeVar(x, 42); s
   */
  case StoreVar(ref: Variable, value: Variable, rest: Statement)

  /**
   * e.g. push { (x, ...) => s }; s
   */
  case PushFrame(frame: Clause, rest: Statement)

  /**
   * e.g. return (v1: t1, ...)
   */
  case Return(arguments: Environment)

  /**
   * Creates a fresh prompt
   * let p = freshPrompt; s
   */
  case FreshPrompt(name: Variable, rest: Statement)

  /**
   * e.g. let k = stack(p) { (x, ...) => s }; s
   */
  case NewStack(name: Variable, prompt: Variable, frame: Clause, rest: Statement)

  /**
   * e.g. push k; s
   */
  case PushStack(stack: Variable, rest: Statement)

  /**
   * Pops stacks until it finds one labeled with `prompt`
   */
  case PopStacks(name: Variable, prompt: Variable, rest: Statement)

  /**
   * let x = #infix_add(v1, ...); s
   */
  case ForeignCall(name: Variable, builtin: String, arguments: Environment, rest: Statement)

  /**
   * let x = 42; s
   */
  case LiteralInt(name: Variable, value: Long, rest: Statement)

  /**
   * let x = 42.2; s
   */
  case LiteralDouble(name: Variable, value: Double, rest: Statement)

  /**
   * let x = "hello"; s
   */
  case LiteralUTF8String(name: Variable, utf8: Array[Byte], rest: Statement)

  /**
   * Statement that is executed when a Hole is encountered.
   */
  case Hole
}
export Statement.*


/**
 * Types
 */
enum Type {
  case Positive()
  case Negative()
  case Prompt()
  case Stack()
  case Int()
  case Byte()
  case Double()
  case String()
  case Reference(tpe: Type)
}
export Type.{ Positive, Negative }

object builtins {

  /**
   * Blocks types are interfaces with a single operation.
   */
  val Apply: Tag = 0

  val Unit: Tag = 0
  val UnitType = Positive()

  val True: Tag = 1
  val False: Tag = 0
  val BooleanType = Positive()

  val SingletonRecord: Tag = 0
}
