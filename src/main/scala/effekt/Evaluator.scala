package effekt
package evaluator

import effekt.typer.Typer
import effekt.symbols._
import effekt.symbols.builtins._
import effekt.util.Thunk
import effekt.util.control
import effekt.util.control._
import org.bitbucket.inkytonik.kiama.util.Emitter
import org.bitbucket.inkytonik.kiama.util.Memoiser
import effekt.source.{ ArgSection, Assign, BlockArg, BooleanLit, Call, Clause, OpClause, DataDef, Def, DefStmt, DoubleLit, EffDef, Expr, ExprStmt, FunDef, Id, If, IntLit, MatchExpr, ModuleDecl, Return, Stmt, StringLit, Tree, TryHandle, UnitLit, ValDef, ValueArgs, Var, VarDef, While }
import effekt.util.messages.{ ErrorReporter, MessageBuffer }

import scala.collection.mutable

class Evaluator(types: TypesDB, val modules: mutable.Map[String, CompilationUnit]) {

  given Assertions

  val mainName = "main"

  /**
   * The universe of values
   */
  object values {

    sealed trait Value

    case class IntValue(value: Int) extends Value {
      override def toString = value.toString
    }

    case class BooleanValue(value: Boolean) extends Value {
      override def toString = value.toString
    }

    case class DoubleValue(value: Double) extends Value {
      override def toString = value.toString
    }

    case object UnitValue extends Value {
      override def toString = "()"
    }

    case class StringValue(value: String) extends Value {
      override def toString = value
    }

    // We are implementing blocks as closures, but eventually we would not need to
    case class Closure(f: List[Value] => control.Control[Value]) extends Value {
      override def toString = "<closure>"
    }

    case class Handler(prompt: Prompt, clauses: Map[Symbol, List[Value] => control.Control[Value]]) extends Value {
      override def toString = s"Handler(${clauses.keys}, $prompt)"
    }

    case class DataValue(tag: Symbol, args: List[Value]) extends Value {
      override def toString = s"${tag.name}(${ args.mkString(",") })"
    }
  }

  import values._

  // Cache for evaluated modules. This avoids evaluating transitive dependencies multiple times
  val evaluatedModules: Memoiser[CompilationUnit, Map[Symbol, Thunk[Value]]] = Memoiser.makeIdMemoiser()

  def run(cu: CompilationUnit, out: Emitter, buffer: MessageBuffer) = {
    val mainSym = cu.exports.terms.getOrElse(mainName, sys error "Cannot find main function")
    val mainFun = mainSym.asUserFunction

    val ctx = Context(mainFun.decl, buffer, cu, Map.empty)

    // TODO refactor and convert into checked error
    val userEffects = types.blockType(mainSym)(given ctx).ret.effects.effs.filterNot { _.builtin }
    if (userEffects.nonEmpty) {
      ctx.abort(s"Main has unhandled user effects: ${userEffects}!")
    }

    eval(cu, out, buffer)(mainSym).value.asInstanceOf[Closure].f(Nil).run()
  }

  def eval(cu: CompilationUnit, out: Emitter, buffer: MessageBuffer): Map[Symbol, Thunk[Value]] =
    evaluatedModules.getOrDefault(cu, {
      val env = cu.module.imports.foldLeft(builtins(out)) {
        case (env, source.Import(path)) =>
          val mod = modules(path)
          val res = eval(mod, out, buffer)
          env ++ res
      }
      val result = eval(cu.module.defs)(given Context(cu.module, buffer, cu, env))
      evaluatedModules.put(cu, result)
      result
    })

  class Prompt extends Capability { type Res = Value }

  type Eval[E <: Tree, R] = E => (given Context) => Control[R]

  val evalExpr: Eval[Expr, Value] = {
    case IntLit(n)     => pure(IntValue(n))
    case DoubleLit(n)  => pure(DoubleValue(n))
    case BooleanLit(b) => pure(BooleanValue(b))
    case UnitLit()     => pure(UnitValue)
    case StringLit(s)  => pure(StringValue(s))

    case Var(id) => id.symbol match {
      // use dynamic lookup on the stack for mutable variables
      case b: VarBinder => control.lookup(b)

      // otherwise fetch the value from the context
      case _ => pure { Context.get(id.symbol) }
    }

    case Assign(b, expr) => for {
      v <- evalExpr(expr)
      _ <- control.update(b.symbol, v)
    } yield UnitValue

    case If(cond, thn, els) => for {
      c <- evalExpr(cond)
      r <- if (c.asBoolean) evalStmt(thn) else evalStmt(els)
    } yield r

    case While(cond, block) => loop(cond, block)

    case Call(fun, _, args) => fun.symbol match {

      case op: EffectOp =>
        val effect = op.effect
        val handler = Context.handler(effect)
        val BlockType(_, params, ret / effs) = op.blockType

        evalArgSections(params, args).flatMap { argv =>
          use(handler.prompt) { k =>
            // package the continuation as a block --
            // it does not take additional arguments, since it does not have effects
            val resume = Closure { args => k(args.head) }
            handler.clauses(op)(argv :+ resume)
          }
        }

      case sym =>
        val BlockType(_, params, ret / effs) = sym.blockType
        evalArgSections(params, args).flatMap { argv =>
          supplyCapabilities(Context.closure(sym), argv, effs)
        }
    }

    case TryHandle(prog, clauses) =>

      val prompt = new Prompt

      val cs = clauses.map {
        case OpClause(op, params, body, resume) =>
          val ps = params.flatMap { _.params.map(_.id.symbol.asValueParam) } :+ resume.symbol
          val impl: List[Value] => control.Control[Value] = args =>
            Context.extendedWith(ps zip args) { evalStmt(body) }
          (op.symbol, impl)
      }.toMap

      val h = Handler(prompt, cs)

      // bind handler to every single effect it handles:
      val bindings = clauses.map { c => (c.op.symbol.asEffectOp.effect, h) }

      handle(prompt) {
        Context.extendedWith(bindings) {
          evalStmt(prog)
        }
      }

    case MatchExpr(sc, clauses) =>
      evalExpr(sc) flatMap {
        case DataValue(tag, args) =>
          val cl = clauses.find { cl =>
            cl.op.symbol == tag
          }.getOrElse(sys error s"Unmatched ${tag}")
          val ps = cl.params.flatMap { _.params.map(_.id.symbol.asValueParam) }
          Context.extendedWith(ps zip args) { evalStmt(cl.body) }
      }
  }

  def eval(funs: List[Def])(given Context): Map[Symbol, Thunk[Value]] = {
    lazy val ctx: Context = Context bindAll bindings
    lazy val bindings: Map[Symbol, Thunk[Value]] = funs.flatMap {
      case (f: FunDef) =>
        List(f.id.symbol -> Thunk { evalFunDef(f)(given ctx) })
      case (d: DataDef) => d.ctors.map { ctor =>
        ctor.id.symbol -> Thunk { evalConstructor(ctor) }
      }
      case _ => Nil
    }.toMap
    bindings
  }

  def evalConstructor(c: source.Constructor)(given Context): Value =
    Closure { args => pure(DataValue(c.id.symbol, args)) }

  def evalFunDef(f: FunDef)(given Context): Value = f match {
    case FunDef(name, _, params, ret, body) =>
      val sym = name.symbol.asFun
      val params = collectBinders(sym.params)
      bindCapabilities(params, types.blockType(sym).ret.effects, body)
  }

  val evalStmt: Eval[Stmt, Value] = {
    case DefStmt(ValDef(id, _, binding), rest) => for {
      v <- evalStmt(binding)
      r <- Context.extendedWith(id.symbol, v) { evalStmt(rest) }
    } yield r
    case DefStmt(VarDef(id, _, binding), rest) =>
      evalStmt(binding).flatMap { v =>
        control.bind(id.symbol, v) { evalStmt(rest) }
      }
    case DefStmt(d, rest) =>
      val ctx = Context.bindAll(eval(List(d)))
      evalStmt(rest)(given ctx)
    case ExprStmt(d, rest) =>
      evalExpr(d) andThen evalStmt(rest)
    case Return(d) =>
      evalExpr(d)
  }

  def traverse[T](c: List[Control[T]]): Control[List[T]] = c match {
    case Nil => pure(Nil)
    case t :: ts => for { v <- t; vs <- traverse(ts) } yield v :: vs
  }

  def loop(cond: Expr, block: Stmt)(given Context): Control[Value] = for {
    c <- evalExpr(cond)
    r <- if (c.asBoolean) evalStmt(block).flatMap { _ => loop(cond, block) } else pure(UnitValue)
  } yield r

  def evalExprs(args: List[Expr])(given Context): Control[List[Value]] =
    traverse { args.map(e => evalExpr(e)) }

  def evalArgSections(ps: List[List[ValueType] | BlockType], args: List[ArgSection])(given Context): Control[List[Value]] =
    args match {
      case Nil => pure(Nil)
      case arg :: args => for {
        v <- evalArgSection(ps.head, arg);
        vs <- evalArgSections(ps.tail, args)
      } yield v ++ vs
    }

  def evalArgSection(sec: List[ValueType] | BlockType, args: ArgSection)(given Context): Control[List[Value]] =
    (sec, args) match {
    case (_, ValueArgs(exprs)) => evalExprs(exprs)
    case (BlockType(_, _, tpe), BlockArg(ps, stmt)) =>
      val params = ps.map(_.id.symbol.asValueParam)
      pure(List(bindCapabilities(params, tpe.effects, stmt)))
  }

  def collectBinders(ps: Params)(given Context): List[Symbol] = ps match {
    case Nil => Nil
    case (b : BlockParam) :: ps => b :: collectBinders(ps)
    case (l : List[ValueParam]) :: ps => l ++ collectBinders(ps)
  }

  // convention: we bind capabilities AFTER normal params
  def bindCapabilities(params: List[Symbol], effs: Effects, s: Stmt)(given Context) = Closure { args =>
    Context.extendedWith((params ++ effs.effs.filterNot(_.builtin)) zip args) { evalStmt(s) }
  }

  // looks up capabilities and provides them as additional arguments to f
  def supplyCapabilities(fun: Closure, args: List[Value], effs: Effects)(given Context) = {
    val caps = effs.effs.filterNot(_.builtin).map(s => Context.get(s))
    fun.f(args ++ caps)
  }

  def builtins(out: Emitter): Map[Symbol, Thunk[Value]] = Map(
    infixAdd -> arithmetic(_ + _),
    infixSub -> arithmetic(_ - _),
    infixMul -> arithmetic(_ * _),
    infixDiv -> arithmetic(_ / _),
    mod      -> arithmetic(_ % _),
    rand     -> Thunk { Closure { _ => pure(IntValue((math.random() * 100).toInt)) }},
    infixEq  -> Thunk { Closure { case x :: y :: _ => pure(BooleanValue(x == y)) }},
    infixLte -> compare(_ <= _),
    infixGte -> compare(_ >= _),
    infixLt  -> compare(_ < _),
    infixGt  -> compare(_ > _),
    infixOr  -> logical(_ || _),
    infixAnd -> logical(_ && _),
    not      -> Thunk { Closure { case BooleanValue(b) :: _ => pure(BooleanValue(!b)) } },
    printInt -> Thunk { Closure { case x :: _ => out.emitln(x); pure(UnitValue) } },
    show     -> Thunk { Closure { case x :: _ => pure(StringValue(x.toString)) } },
    infixConcat -> Thunk { Closure { case StringValue(l) :: StringValue(r) :: _ => pure(StringValue(l + r)) } },

    addDouble -> Thunk { Closure { case DoubleValue(l) :: DoubleValue(r) :: _ => pure(DoubleValue(l + r)) } },
    mulDouble -> Thunk { Closure { case DoubleValue(l) :: DoubleValue(r) :: _ => pure(DoubleValue(l * r)) } },
    subDouble -> Thunk { Closure { case DoubleValue(l) :: DoubleValue(r) :: _ => pure(DoubleValue(l - r)) } }
  )

  private def arithmetic(f: (Int, Int) => Int): Thunk[Value] = Thunk { Closure {
    case IntValue(x) :: IntValue(y) :: Nil => pure(IntValue(f(x, y)))
  } }

  private def compare(f: (Int, Int) => Boolean): Thunk[Value] = Thunk { Closure {
    case IntValue(x) :: IntValue(y) :: Nil => pure(BooleanValue(f(x, y)))
  } }

  private def logical(f: (Boolean, Boolean) => Boolean): Thunk[Value] = Thunk { Closure {
    case BooleanValue(x) :: BooleanValue(y) :: Nil => pure(BooleanValue(f(x, y)))
  } }


  /**
   * The evaluation context of first AND second class values
   * For simplicity, we do not separate them -- this is not a problem since symbols are unique.
   */
  case class Context(
    focus: Tree,
    buffer: MessageBuffer,
    unit: CompilationUnit,
    env: Map[Symbol, Thunk[Value]]
  ) extends ErrorReporter {
    def get(sym: Symbol): Value =
      env.getOrElse(sym, sys.error("No value found for " + sym)).value

    def bindAll(bindings: Map[Symbol, Thunk[Value]]): Context =
      copy(env = env ++ bindings)

    def closure(name: Symbol) = get(name).asInstanceOf[Closure]

    def handler(name: Symbol) = get(name).asInstanceOf[Handler]

    def extendedWith[T](sym: Symbol, value: Thunk[Value])(f: (given Context) => T): T =
      f(given copy(env = env + (sym -> value)))

    def extendedWith[T](sym: Symbol, value: Value)(f: (given Context) => T): T =
      extendedWith(List(sym -> value))(f)

    def extendedWith[T](bindings: List[(Symbol, Value)])(f: (given Context) => T): T =
      f(given copy(env = env ++ bindings.map { case (s, v) => (s, Thunk(v)) }))

    def (id: Id) symbol: Symbol = unit.symbols(id)

    def (v: Value) asBoolean: Boolean = v.asInstanceOf[BooleanValue].value

    def (sym: Symbol) blockType: BlockType = types.blockType(sym)(given this)

    def (f: Fun) effects: Effects = f.returnType.effects

    def (f: Fun) returnType: Effectful = f.ret match {
      case Some(t) => t
      case None => types.blockType(f)(given this).ret
    }
  }
  def Context(given ctx: Context): Context = ctx
}
