package effekt
package evaluator

import effekt.context.Context
import effekt.context.assertions.{ SymbolAssertions }
import effekt.symbols._
import effekt.symbols.builtins._
import effekt.source.{ ArgSection, Assign, BlockArg, BooleanLit, Call, DataDef, Def, DefStmt, DoubleLit, Expr, ExprStmt, FunDef, Id, If, IntLit, MatchExpr, OpClause, Return, Stmt, StringLit, Tree, TryHandle, UnitLit, ValDef, ValueArgs, Var, VarDef, While }

import effekt.util.Thunk
import effekt.util.control
import effekt.util.control._

import org.bitbucket.inkytonik.kiama.util.Emitter
import org.bitbucket.inkytonik.kiama.util.Memoiser


class Evaluator {

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

    case class Handler(prompt: Prompt, clauses: Map[EffectOp, List[Value] => control.Control[Value]]) extends Value {
      override def toString = s"Handler(${clauses.keys}, $prompt)"
    }

    case class DataValue(tag: Constructor, args: List[Value]) extends Value {
        override def toString = s"${tag.name}(${ args.map { _.toString }.mkString(", ") })"
    }
  }

  import values._

  // Cache for evaluated modules. This avoids evaluating transitive dependencies multiple times
  val evaluatedModules: Memoiser[Module, Map[Symbol, Thunk[Value]]] = Memoiser.makeIdMemoiser()

  def run(mod: symbols.Module)(implicit compiler: Context): Value = {
    val mainSym = mod.terms.getOrElse(mainName, compiler.abort("Cannot find main function"))
    val mainFun = mainSym.asUserFunction

    // TODO refactor and convert into checked error
    val userEffects = compiler.blockTypeOf(mainSym).ret.effects.effs.filterNot { _.builtin }
    if (userEffects.nonEmpty) {
      compiler.abort(s"Main has unhandled user effects: ${userEffects}!")
    }

    eval(mod, compiler)(mainSym).value.asInstanceOf[Closure].f(Nil).run()
  }

  def eval(mod: Module, compiler: Context): Map[Symbol, Thunk[Value]] =
    evaluatedModules.getOrDefault(mod, {
      val env = mod.decl.imports.foldLeft(builtins(compiler.config.output())) {
        case (env, source.Import(path)) =>
          val mod = compiler.moduleOf(path)
          val res = eval(mod, compiler)
          env ++ res
      }
      val result = eval(mod.decl.defs)(EvalContext(env, compiler))
      evaluatedModules.put(mod, result)
      result
    })

  class Prompt extends Capability { type Res = Value }

  def evalExpr(expr: Expr)(implicit C: EvalContext): Control[Value] = expr match {
    case IntLit(n)     => pure(IntValue(n))
    case DoubleLit(n)  => pure(DoubleValue(n))
    case BooleanLit(b) => pure(BooleanValue(b))
    case UnitLit()     => pure(UnitValue)
    case StringLit(s)  => pure(StringValue(s))

    case v : Var => v.definition match {
      // use dynamic lookup on the stack for mutable variables
      case b: VarBinder => control.lookup(b)

      // otherwise fetch the value from the context
      case b => pure { C.get(b) }
    }

    case a @ Assign(b, expr) => for {
      v <- evalExpr(expr)
      _ <- control.update(a.definition, v)
    } yield UnitValue

    case If(cond, thn, els) => for {
      c <- evalExpr(cond)
      r <- if (C.asBoolean(c)) evalStmt(thn) else evalStmt(els)
    } yield r

    case While(cond, block) => loop(cond, block)

    case c @ Call(fun, _, args) => c.definition match {

      case op: EffectOp =>
        val effect = op.effect
        val handler = C.handler(effect)
        val BlockType(_, params, ret / effs) = Compiler.blockTypeOf(op)

        evalArgSections(params, args).flatMap { argv =>
          use(handler.prompt) { k =>
            // package the continuation as a block --
            // it does not take additional arguments, since it does not have effects
            val resume = Closure { args => k(args.head) }
            handler.clauses(op)(argv :+ resume)
          }
        }

      case sym =>
        val BlockType(_, params, ret / effs) = Compiler.blockTypeOf(sym)
        evalArgSections(params, args).flatMap { argv =>
          supplyCapabilities(C.closure(sym), argv, effs)
        }
    }

    case TryHandle(prog, clauses) =>

      val prompt = new Prompt

      val cs = clauses.map {
        case op @ OpClause(id, params, body, resume) =>
          val ps = params.flatMap { _.params.map(_.symbol) } :+ Compiler.symbolOf(resume)
          val impl: List[Value] => control.Control[Value] = args =>
            C.extendedWith(ps zip args) { implicit C => evalStmt(body) }
          (op.definition, impl)
      }.toMap

      val h = Handler(prompt, cs)

      // bind handler to every single effect it handles:
      val bindings = clauses.map { c => (c.definition.effect, h) }

      handle(prompt) {
        C.extendedWith(bindings) { implicit C =>
          evalStmt(prog)
        }
      }

    case MatchExpr(sc, clauses) =>
      evalExpr(sc) flatMap {
        case DataValue(tag, args) =>
          val cl = clauses.find { cl =>
            cl.definition == tag
          }.getOrElse(sys error s"Unmatched ${tag}")
          val ps = cl.params.flatMap { _.params.map(_.symbol) }
          C.extendedWith(ps zip args) {implicit C => evalStmt(cl.body) }
      }
  }

  def eval(funs: List[Def])(implicit C: EvalContext): Map[Symbol, Thunk[Value]] = {
    lazy val ctx: EvalContext = C bindAll bindings
    lazy val bindings: Map[Symbol, Thunk[Value]] = funs.flatMap {
      case (f: FunDef) =>
        List(f.symbol -> Thunk { evalFunDef(f)(ctx) })
      case (d: DataDef) => d.ctors.map { ctor =>
        ctor.symbol -> Thunk { evalConstructor(ctor) }
      }
      // top level values -- they do not have effects
      case (v: ValDef) =>
        List(v.symbol -> Thunk { evalStmt(v.binding).run() })
      case _ =>
        Nil
    }.toMap
    bindings
  }

  def evalConstructor(c: source.Constructor)(implicit C: EvalContext): Value =
    Closure { args => pure(DataValue(c.symbol, args)) }

  def evalFunDef(f: FunDef)(implicit C: EvalContext): Value = f match {
    case FunDef(name, _, params, ret, body) =>
      val sym = f.symbol
      val params = collectBinders(sym.params)
      bindCapabilities(params, Compiler.blockTypeOf(sym).ret.effects, body)
  }

  def evalStmt(stmt: Stmt)(implicit C: EvalContext): Control[Value] = stmt match {
    case DefStmt(d @ ValDef(id, _, binding), rest) => for {
      v <- evalStmt(binding)
      r <- C.extendedWith(d.symbol, v) { implicit C => evalStmt(rest) }
    } yield r
    case DefStmt(d @ VarDef(id, _, binding), rest) =>
      evalStmt(binding).flatMap { v =>
        control.bind(d.symbol, v) { evalStmt(rest) }
      }
    case DefStmt(d, rest) =>
      val ctx = C.bindAll(eval(List(d)))
      evalStmt(rest)(ctx)
    case ExprStmt(d, rest) =>
      evalExpr(d) andThen evalStmt(rest)
    case Return(d) =>
      evalExpr(d)
  }

  def traverse[T](c: List[Control[T]]): Control[List[T]] = c match {
    case Nil => pure(Nil)
    case t :: ts => for { v <- t; vs <- traverse(ts) } yield v :: vs
  }

  def loop(cond: Expr, block: Stmt)(implicit C: EvalContext): Control[Value] = for {
    c <- evalExpr(cond)
    r <- if (C.asBoolean(c)) evalStmt(block).flatMap { _ => loop(cond, block) } else pure(UnitValue)
  } yield r

  def evalExprs(args: List[Expr])(implicit C: EvalContext): Control[List[Value]] =
    traverse { args.map(e => evalExpr(e)) }

  def evalArgSections(ps: List[List[Type]], args: List[ArgSection])(implicit C: EvalContext): Control[List[Value]] =
    args match {
      case Nil => pure(Nil)
      case arg :: args => for {
        v <- evalArgSection(ps.head, arg);
        vs <- evalArgSections(ps.tail, args)
      } yield v ++ vs
    }

  def evalArgSection(sec: List[Type], args: ArgSection)(implicit C: EvalContext): Control[List[Value]] =
    (sec, args) match {
    case (_, ValueArgs(exprs)) => evalExprs(exprs)
    case (List(BlockType(_, _, tpe)), BlockArg(ps, stmt)) =>
      pure(List(bindCapabilities(ps.params.map(_.symbol), tpe.effects, stmt)))
  }

  def collectBinders(ps: Params)(implicit C: EvalContext): List[Symbol] = ps.flatten

  // convention: we bind capabilities AFTER normal params
  def bindCapabilities(params: List[Symbol], effs: Effects, s: Stmt)(implicit C: EvalContext) = Closure { args =>
    C.extendedWith((params ++ effs.effs.filterNot(_.builtin)) zip args) { implicit C =>
      evalStmt(s)
    }
  }

  // looks up capabilities and provides them as additional arguments to f
  def supplyCapabilities(fun: Closure, args: List[Value], effs: Effects)(implicit C: EvalContext) = {
    val caps = effs.effs.filterNot(_.builtin).map(s => C.get(s))
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
    printInt -> Thunk { Closure { case x :: _ => out.emitln(x.toString); pure(UnitValue) } },
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
  case class EvalContext(
    env: Map[Symbol, Thunk[Value]],
    compiler: Context
  ) {
    def get(sym: Symbol): Value =
      env.getOrElse(sym, compiler.abort("No value found for " + sym)).value

    def bindAll(bindings: Map[Symbol, Thunk[Value]]): EvalContext =
      copy(env = env ++ bindings)

    def extendedWith[T](sym: Symbol, value: Value)(f: EvalContext => T): T =
      extendedWith(List(sym -> value))(f)

    def extendedWith[T](bindings: List[(Symbol, Value)])(f: EvalContext => T): T =
      f(copy(env = env ++ bindings.map { case (s, v) => (s, Thunk(v)) }))

    def closure(name: Symbol) = get(name).asInstanceOf[Closure]

    def handler(name: Symbol) = get(name).asInstanceOf[Handler]

    def asBoolean(v: Value): Boolean = v.asInstanceOf[BooleanValue].value
  }
  def Context(implicit C: EvalContext): EvalContext = C
  def Compiler(implicit ctx: EvalContext): Context = ctx.compiler
  implicit def currentContext(implicit ctx: EvalContext): Context = ctx.compiler
}
