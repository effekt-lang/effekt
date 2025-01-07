package effekt
package core
package interpreter

import effekt.source.FeatureFlag

import scala.annotation.tailrec


type ~>[-A, +B] = PartialFunction[A, B]


type Address = Int
private var lastAddress: Address = 0
def freshAddress(): Address = { lastAddress += 1; lastAddress }

val GLOBAL_PROMPT = 0

class Reference(var value: Value)

enum Value {
  case Literal(value: Any)
  // TODO this could also be Pointer(Array | Ref)
  case Array(array: scala.Array[Value])
  case Ref(ref: Reference)
  case Data(data: ValueType.Data, tag: Id, fields: List[Value])
  case Boxed(block: Computation)
}
object Value {
  def Int(v: Long): Value = Value.Literal(v)
  def Bool(b: Boolean): Value = Value.Literal(b)
  def Unit(): Value = Value.Literal(())
  def Double(d: scala.Double): Value = Value.Literal(d)
  def String(s: java.lang.String): Value = Value.Literal(s)
}

def inspect(v: Value): String = v match {
  case Value.Literal(value) => value.toString
  case Value.Data(data, tag, fields) =>
    tag.name.name + "(" + fields.map(inspect).mkString(", ") + ")"
  case Value.Boxed(block) => block.toString
  case Value.Array(arr) => ???
  case Value.Ref(ref) => ???
}

enum Computation {
  case Closure(id: Id, env: Env)
  case Object(methods: Map[Id, BlockLit], env: Env)
  case Region(address: Address)
  case Prompt(address: Address)
  case Resumption(cont: Stack)
}

enum Env {
  case Top(functions: Map[Id, BlockLit], builtins: Map[Id, Builtin], declarations: List[core.Declaration])
  case Static(id: Id, block: BlockLit, rest: Env)
  case Dynamic(id: Id, block: Computation, rest: Env)
  case Let(id: Id, value: Value, rest: Env)

  def bind(id: Id, value: Value): Env = Let(id, value, this)
  def bind(id: Id, lit: BlockLit): Env = Static(id, lit, this)
  def bind(id: Id, block: Computation): Env = Dynamic(id, block, this)
  def bindValues(otherValues: List[(Id, Value)]): Env =
    otherValues.foldLeft(this) { case (env, (id, value)) => Let(id, value, env) }
  def bindBlocks(otherBlocks: List[(Id, Computation)]): Env =
    otherBlocks.foldLeft(this) { case (env, (id, block)) => Dynamic(id, block, env) }

  def lookupValue(id: Id): Value = {
    @tailrec
    def go(rest: Env): Value = rest match {
      case Env.Top(functions, builtins, declarations) => throw InterpreterError.NotFound(id)
      case Env.Static(id, block, rest) => go(rest)
      case Env.Dynamic(id, block, rest) => go(rest)
      case Env.Let(otherId, value, rest) => if (id == otherId) value else go(rest)
    }
    go(this)
  }

  def lookupBuiltin(id: Id): Builtin = {
    @tailrec
    def go(rest: Env): Builtin = rest match {
      case Env.Top(functions, builtins, declarations) => builtins.getOrElse(id, throw InterpreterError.NotFound(id))
      case Env.Static(id, block, rest) => go(rest)
      case Env.Dynamic(id, block, rest) => go(rest)
      case Env.Let(id, value, rest) => go(rest)
    }
    go(this)
  }

  def lookupStatic(id: Id): (BlockLit, Env) = this match {
    case Env.Top(functions, builtins, declarations) => (functions.getOrElse(id, throw InterpreterError.NotFound(id)), this)
    case Env.Static(other, block, rest) => if (id == other) (block, this) else rest.lookupStatic(id)
    case Env.Dynamic(other, block, rest) => rest.lookupStatic(id)
    case Env.Let(other, value, rest) => rest.lookupStatic(id)
  }
}

enum InterpreterError extends Throwable {
  case NotFound(id: Id)
  case NotAnExternFunction(id: Id)
  case MissingBuiltin(name: String)
  case RuntimeTypeError(msg: String)
  case NonExhaustive(missingCase: Id)
  case Hole()
  case NoMain()

  override def getMessage: String = this match {
    case InterpreterError.NotFound(id) => s"Not found: ${id}"
    case InterpreterError.NotAnExternFunction(id) => s"Not an extern function: ${id}"
    case InterpreterError.MissingBuiltin(name) => s"Missing builtin: ${name}"
    case InterpreterError.RuntimeTypeError(msg) => s"Runtime type error: ${msg}"
    case InterpreterError.NonExhaustive(missingCase) => s"Non exhaustive: ${missingCase}"
    case InterpreterError.Hole() => s"Reached hole"
    case InterpreterError.NoMain() => s"No main"
  }
}

enum Stack {
  case Empty
  case Segment(frames: List[Frame], prompt: Address, rest: Stack)
}
object Stack {
  val Toplevel = Stack.Segment(Nil, GLOBAL_PROMPT, Stack.Empty)
}
def show(stack: Stack): String = stack match {
  case Stack.Empty => "Empty"
  case Stack.Segment(frames, prompt, rest) =>
    s"${frames.map(show).mkString(" :: ")} :: p${prompt } :: ${show(rest)}"
}

def show(frame: Frame): String = frame match {
  case Frame.Var(x, value) => s"${util.show(x)}=${show(value)}"
  case Frame.Val(x, body, env) => s"val ${util.show(x)}"
  case Frame.Region(r, values) => s"region ${r} {${values.map {
    case (id, value) => s"${util.show(id)}=${show(value)}}"
  }.mkString(", ")}}"
}

def show(value: Value): String = inspect(value)

enum Frame {
  // mutable state
  case Var(x: Id, value: Value)
  // sequencing
  case Val(x: Id, body: Stmt, env: Env)
  // local regions
  case Region(r: Address, values: Map[Id, Value])
}

enum State {
  case Done(result: Value)
  case Step(stmt: Stmt, env: Env, stack: Stack)
}

class Interpreter(instrumentation: Instrumentation, runtime: Runtime) {

  // TODO maybe replace region values by integers instead of Id

  @tailrec
  private def returnWith(value: Value, env: Env, stack: Stack): State =
    @tailrec
    def go(frames: List[Frame], prompt: Address, stack: Stack): State =
      frames match {
        case Frame.Val(x, body, frameEnv) :: rest =>
          instrumentation.popFrame()
          State.Step(body, frameEnv.bind(x, value), Stack.Segment(rest, prompt, stack))
        // free the mutable state
        case Frame.Var(x, value) :: rest => go(rest, prompt, stack)
        // free the region
        case Frame.Region(x, values) :: rest => go(rest, prompt, stack)
        case Nil => returnWith(value, env, stack)
      }
    stack match {
      case Stack.Empty => State.Done(value)
      case Stack.Segment(frames, prompt, rest) =>
        go(frames, prompt, rest)
    }

  private def push(frame: Frame, stack: Stack): Stack = stack match {
    case Stack.Empty => ???
    case Stack.Segment(frames, prompt, rest) => Stack.Segment(frame :: frames, prompt, rest)
  }

  @tailrec
  private def findFirst[A](stack: Stack)(f: Frame ~> A): Option[A] =
    stack match {
      case Stack.Empty => None
      case Stack.Segment(frames, prompt, rest) =>
        @tailrec
        def go(frames: List[Frame]): Option[A] =
          frames match {
            case Nil => findFirst(rest)(f)
            case frame :: rest if f.isDefinedAt(frame) => Some(f(frame))
            case frame :: rest => go(rest)
          }
        go(frames)
    }

  def updateOnce(stack: Stack)(f: Frame ~> Frame): Stack =
    stack match {
      case Stack.Empty => ???
      case Stack.Segment(frames, prompt, rest) =>
        def go(frames: List[Frame], acc: List[Frame]): Stack =
          frames match {
            case Nil =>
              Stack.Segment(acc.reverse, prompt, updateOnce(rest)(f))
            case frame :: frames if f.isDefinedAt(frame) =>
              Stack.Segment(acc.reverse ++ (f(frame) :: frames), prompt, rest)
            case frame :: frames =>
              go(frames, frame :: acc)
          }
        go(frames, Nil)
    }

  @tailrec
  private def findFirst[T](env: Env)(f: Env ~> T): T = env match {
    case e if f.isDefinedAt(e) => f(e)
    case Env.Top(functions, builtins, declarations) => ???
    case Env.Static(id, block, rest) => findFirst(rest)(f)
    case Env.Dynamic(id, block, rest) => findFirst(rest)(f)
    case Env.Let(id, value, rest) => findFirst(rest)(f)
  }

  def step(s: State): State =
    instrumentation.step(s)
    s match {
      case State.Done(result) => s
      case State.Step(stmt, env, stack) => stmt match {
        // do not create a closure
        case Stmt.Def(id, block: Block.BlockLit, body) => State.Step(body, env.bind(id, block), stack)

        // create a closure
        case Stmt.Def(id, block, body) => State.Step(body, env.bind(id, eval(block, env)), stack)

        case Stmt.Let(id, tpe, binding, body) => State.Step(body, env.bind(id, eval(binding, env)), stack)

        case Stmt.Return(expr) =>
          val v = eval(expr, env)
          returnWith(v, env, stack)

        case Stmt.Val(id, annotatedTpe, binding, body) =>
          instrumentation.pushFrame()
          State.Step(binding, env, push(Frame.Val(id, body, env), stack))

        case Stmt.App(Block.BlockVar(id, _, _), targs, vargs, bargs) =>
          @tailrec
          def lookup(env: Env): (BlockLit, Env) = env match {
            case Env.Top(functions, builtins, declarations) =>
              instrumentation.staticDispatch(id)
              (functions.getOrElse(id, throw InterpreterError.NotFound(id)), env)
            case Env.Static(other, block, rest) if id == other =>
              instrumentation.staticDispatch(id)
              (block, env)
            case Env.Static(other, block, rest) => lookup(rest)
            case Env.Dynamic(other, block, rest) if id == other => block match {
              case Computation.Closure(target, env) =>
                instrumentation.dynamicDispatch(id)
                env.lookupStatic(target)
              case _ =>
                throw InterpreterError.RuntimeTypeError("Can only call functions")
            }
            case Env.Dynamic(other, block, rest) => lookup(rest)
            case Env.Let(other, value, rest) => lookup(rest)
          }

          val (Block.BlockLit(_, _, vparams, bparams, body), definitionEnv) = lookup(env)

          State.Step(
            body,
            definitionEnv
              .bindValues((vparams zip vargs).map { case (p, a) => p.id -> eval(a, env) })
              .bindBlocks((bparams zip bargs).map { case (p, a) => p.id -> eval(a, env) }),
            stack)

        case Stmt.App(callee, targs, vargs, bargs) => ???

        case Stmt.Invoke(b, method, methodTpe, targs, vargs, bargs) =>
          eval(b, env) match {
            case Computation.Object(methods, definitionEnv) =>
              val BlockLit(_, _, vparams, bparams, body) = methods.getOrElse(method, throw InterpreterError.NonExhaustive(method))
              instrumentation.dynamicDispatch(method)
              State.Step(
                body,
                definitionEnv
                  .bindValues((vparams zip vargs).map { case (p, a) => p.id -> eval(a, env) })
                  .bindBlocks((bparams zip bargs).map { case (p, a) => p.id -> eval(a, env) }),
                stack)
            case _ => throw InterpreterError.RuntimeTypeError("Can only call methods on objects")
          }

        case Stmt.If(cond, thn, els) =>
          instrumentation.branch()
          eval(cond, env) match {
            case As.Bool(true)  => State.Step(thn, env, stack)
            case As.Bool(false) => State.Step(els, env, stack)
            case v => throw InterpreterError.RuntimeTypeError(s"Expected Bool, but got ${v}")
          }

        case Stmt.Match(scrutinee, clauses, default) => eval(scrutinee, env) match {
          case Value.Data(data, tag, fields) =>
            @tailrec
            def search(clauses: List[(Id, BlockLit)], comparisons: Int): State = (clauses, default) match {
              case (Nil, None) =>
                throw InterpreterError.NonExhaustive(tag)
              case (Nil, Some(stmt)) =>
                instrumentation.patternMatch(comparisons)
                State.Step(stmt, env, stack)
              case ((id, BlockLit(tparams, cparams, vparams, bparams, body)) :: clauses, _) if id == tag =>
                instrumentation.patternMatch(comparisons)
                State.Step(body, env.bindValues(vparams.map(p => p.id) zip fields), stack)
              case (_ :: clauses, _) => search(clauses, comparisons + 1)
            }
            search(clauses, 0)

          case other => throw InterpreterError.RuntimeTypeError(s"Expected value of a data type, but got ${other}")
        }

        case Stmt.Region(Block.BlockLit(_, _, _, List(region), body)) =>
          val fresh = freshAddress()

          instrumentation.allocateRegion(fresh)

          State.Step(body, env.bind(region.id, Computation.Region(fresh)),
            push(Frame.Region(fresh, Map.empty), stack))

        // TODO make the type of Region more precise...
        case Stmt.Region(_) => ???

        case Stmt.Alloc(id, init, region, body) =>
          val value = eval(init, env)

          val address = findFirst(env) {
            case Env.Dynamic(id, Computation.Region(r), rest) => r
          }

          instrumentation.allocateVariableIntoRegion(id, address)

          val updated = updateOnce(stack) {
            case Frame.Region(r, values) if r == address =>
              Frame.Region(r, values.updated(id, value))
          }
          State.Step(body, env, updated)

        // TODO also use addresses for variables
        case Stmt.Var(id, init, capture, body) =>
          instrumentation.allocateVariable(id)
          State.Step(body, env, push(Frame.Var(id, eval(init, env)), stack))

        case Stmt.Get(id, annotatedCapt, annotatedTpe) =>
          instrumentation.readMutableVariable(id)
          val value = findFirst(stack) {
            case Frame.Var(other, value) if other == id => value
            case Frame.Region(_, values) if values.isDefinedAt(id) => values(id)
          } getOrElse ???

          returnWith(value, env, stack)

        case Stmt.Put(id, annotatedCapt, value) =>
          instrumentation.writeMutableVariable(id)
          val newValue = eval(value, env)
          val updated = updateOnce(stack) {
            case Frame.Var(other, value) if other == id =>
              Frame.Var(other, newValue)
            case Frame.Region(r, values) if values.isDefinedAt(id) =>
              Frame.Region(r, values.updated(id, newValue))
          }

          returnWith(Value.Literal(()), env, updated)

        case Stmt.Reset(BlockLit(_, _, _, List(prompt), body)) =>
          val freshPrompt = freshAddress()
          instrumentation.reset()
          State.Step(body, env.bind(prompt.id, Computation.Prompt(freshPrompt)),
            Stack.Segment(Nil, freshPrompt, stack))

        case Stmt.Reset(b) => ???

        case Stmt.Shift(prompt, BlockLit(tparams, cparams, vparams, List(resume), body)) =>
          instrumentation.shift()
          val address = findFirst(env) {
            case Env.Dynamic(id, Computation.Prompt(addr), rest) => addr
          }
          @tailrec
          def unwind(stack: Stack, cont: Stack): (Stack, Stack) = stack match {
            case Stack.Empty => ???
            case Stack.Segment(frames, prompt, rest) if prompt == address =>
              (Stack.Segment(frames, prompt, cont), rest)
            case Stack.Segment(frames, prompt, rest) =>
              unwind(rest, Stack.Segment(frames, prompt, cont))
          }
          val (cont, rest) = unwind(stack, Stack.Empty)

          State.Step(body, env.bind(resume.id, Computation.Resumption(cont)), rest)
        case Stmt.Shift(_, _) => ???


        case Stmt.Resume(k, body) =>
          instrumentation.resume()
          val cont = findFirst(env) {
            case Env.Dynamic(id, Computation.Resumption(k), rest) => k
          }
          @tailrec
          def rewind(k: Stack, onto: Stack): Stack = k match {
            case Stack.Empty => onto
            case Stack.Segment(frames, prompt, rest) =>
              rewind(rest, Stack.Segment(frames, prompt, onto))
          }
          State.Step(body, env, rewind(cont, stack))

        case Stmt.Hole() => throw InterpreterError.Hole()
      }
    }

  @tailrec
  private def run(s: State): Value = s match {
    case State.Done(result) => result
    case other => run(step(other))
  }

  def eval(b: Block, env: Env): Computation = b match {
    case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      @tailrec
      def go(env: Env): Computation = env match {
        case Env.Top(functions, builtins, declarations) => instrumentation.closure(); Computation.Closure(id, env)
        case Env.Static(other, block, rest) if other == id => instrumentation.closure(); Computation.Closure(id, env)
        case Env.Static(other, block, rest) => go(rest)
        case Env.Dynamic(other, block, rest) if other == id => block
        case Env.Dynamic(other, block, rest) => go(rest)
        case Env.Let(other, value, rest) => go(rest)
      }
      go(env)
    case b @ Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      val tmp = Id("tmp")
      instrumentation.closure()
      Computation.Closure(tmp, env.bind(tmp, b))
    case Block.Unbox(pure) => eval(pure, env) match {
      case Value.Boxed(block) => block
      case other => throw InterpreterError.RuntimeTypeError(s"Expected boxed block, but got ${other}")
    }
    case Block.New(Implementation(interface, operations)) =>
      instrumentation.closure()
      Computation.Object(operations.map {
        case Operation(id, tparams, cparams, vparams, bparams, body) =>
          id -> (BlockLit(tparams, cparams, vparams, bparams, body): BlockLit)
      }.toMap, env)
  }

  def eval(e: Expr, env: Env): Value = e match {
    case DirectApp(b, targs, vargs, Nil) => env.lookupBuiltin(b.id) match {
      case Builtin(name, impl) =>
        val arguments = vargs.map(a => eval(a, env))
        instrumentation.builtin(name)
        try { impl(runtime)(arguments) } catch { case e => sys error s"Cannot call ${b} with arguments ${arguments.map {
          case Value.Literal(l) => s"${l}: ${l.getClass.getName}"
          case other => other.toString
        }.mkString(", ")}" }
    }
    case DirectApp(b, targs, vargs, bargs) => ???
    case Pure.ValueVar(id, annotatedType) => env.lookupValue(id)
    case Pure.Literal(value, annotatedType) => Value.Literal(value)
    case Pure.PureApp(x, targs, vargs) => env.lookupBuiltin(x.id) match {
      case Builtin(name, impl) =>
        val arguments = vargs.map(a => eval(a, env))
        instrumentation.builtin(name)
        try { impl(runtime)(arguments) } catch { case e => sys error s"Cannot call ${x} with arguments ${arguments.map {
          case Value.Literal(l) => s"${l}: ${l.getClass.getName}"
          case other => other.toString
        }.mkString(", ")}" }
    }
    case Pure.Make(data, tag, vargs) =>
      val result: Value.Data = Value.Data(data, tag, vargs.map(a => eval(a, env)))
      instrumentation.allocate(result)
      result
    case Pure.Select(target, field, annotatedType) =>
      @tailrec
      def declarations(env: Env): List[Declaration] = env match {
        case Env.Top(functions, builtins, declarations) => declarations
        case Env.Static(id, block, rest) => declarations(rest)
        case Env.Dynamic(id, block, rest) => declarations(rest)
        case Env.Let(id, value, rest) => declarations(rest)
      }
      val decls = DeclarationContext(declarations(env), Nil)

      // TODO clean this mess up!
      val fieldSymbol = decls.findField(field).getOrElse(???)
      val constrSymbol = decls.findConstructor(fieldSymbol).getOrElse(???)
      val index = constrSymbol.fields.indexOf(fieldSymbol)

      instrumentation.fieldLookup(field)

      eval(target, env) match {
        case Value.Data(data, tag, fields) => fields(index)
        case _ => ???
      }

    case Pure.Box(b, annotatedCapture) => Value.Boxed(eval(b, env))
  }

  def run(main: Id, m: ModuleDecl): Unit = {

    // TODO toplevel val definitinos
    val mainFun = m.definitions.collectFirst {
      case Toplevel.Def(id, b: BlockLit) if id == main => b
    }.getOrElse { throw InterpreterError.NoMain() }

    val functions = m.definitions.collect { case Toplevel.Def(id, b: Block.BlockLit) => id -> b }.toMap

    val builtinFunctions = m.externs.collect {
      case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture,
        ExternBody.StringExternBody(FeatureFlag.NamedFeatureFlag("jvm"), Template(name :: Nil, Nil))) =>
          id -> builtins.getOrElse(name, throw InterpreterError.MissingBuiltin(name))
    }.toMap

    val env = Env.Top(functions, builtinFunctions, m.declarations)

    val initial = State.Step(mainFun.body, env, Stack.Toplevel)

    run(initial)
  }
}
