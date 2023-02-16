package effekt
package generator
package chez

import effekt.core.Id
import effekt.context.Context
import effekt.lifted.*
import effekt.symbols.{ Module, Symbol, Wildcard, TermSymbol }

import scala.language.implicitConversions
import effekt.util.paths.*
import kiama.output.PrettyPrinterTypes.Document

/**
 * Lifted variant of Chez Scheme. Mostly copy-and-paste from [[ChezScheme]].
 *
 * Difficult to share the code, since core and lifted are different IRs.
 */
object ChezSchemeLift extends Backend {

  def runMain(main: ChezName): chez.Expr =
    val monomorphized = true
    if (monomorphized) chez.Call(chez.Call(main), List(CPS.id))
    else chez.Call(chez.Call(main, CPS.id), List(CPS.id))

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, mainSymbol: TermSymbol)(using Context) =
    LiftInference(main).map { lifted =>
      val chezModule = chez.Let(Nil, compilationUnit(mainSymbol, lifted.mod, lifted.core))
      val result = chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(chezModule), 100)
      val mainFile = path(main.mod)
      Compiled(main.source, mainFile, Map(mainFile -> result))
    }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: AllTransformed)(using C: Context) =
    C.using(module = input.main.mod) { Some(chez.PrettyPrinter.format(compile(input.main))) }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(using Context): List[chez.Def] =
    LiftInference(in).toList.flatMap { lifted => toChez(lifted.core) }

  def compilationUnit(mainSymbol: Symbol, mod: Module, core: ModuleDecl): chez.Block = {
    val definitions = toChez(core)
    chez.Block(generateStateAccessors ++ definitions, Nil, runMain(nameRef(mainSymbol)))
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"

  def toChez(p: Param): ChezName = nameDef(p.id)

  def toChez(e: Argument): chez.Expr = e match {
    case e: lifted.Expr  => toChez(e)
    case b: lifted.Block => toChez(b)
    case e: lifted.Evidence => toChez(e)
  }

  def toChez(module: ModuleDecl): List[chez.Def] = {
    val decls = module.decls.flatMap(toChez)
    val externs = module.externs.map(toChez)
    // TODO FIXME, once there is a let _ = ... in there, we are doomed!
    val defns = module.definitions.map(toChez).flatMap {
      case Left(d) => Some(d)
      case Right(None) => None
      case Right(e) => ???
    }
    decls ++ externs ++ defns
  }

  def toChezExpr(stmt: Stmt): CPS = stmt match {
    case Return(e) => CPS.pure(toChez(e))
    case App(b, targs, args) => CPS.inline { k => chez.Call(chez.Call(toChez(b), args map toChez), List(k.reify)) }

    case If(cond, thn, els) =>
      CPS.join { k =>
        chez.If(toChez(cond), toChezExpr(thn)(k), toChezExpr(els)(k))
      }
    case Val(id, binding, body) =>
      toChezExpr(binding).flatMap { value =>
        CPS.inline { k =>
          chez.Let(List(Binding(nameDef(id), value)), toChez(body, k))
        }
      }
    case Match(scrutinee, clauses, default) => CPS.join { k =>
      val sc = toChez(scrutinee)
      val cls = clauses.map { case (constr, branch) =>
        val names = RecordNames(constr)
        val pred = chez.Call(chez.Variable(names.predicate), List(sc))
        val matcher = chez.Call(chez.Call(chez.Variable(names.matcher), List(sc, toChez(branch))), List(k.reify))
        (pred, matcher)
      }
      chez.Cond(cls, default.map { d => toChezExpr(d)(k) })
    }

    case Hole() => CPS.inline { k => chez.Builtin("hole") }

    case State(id, init, region, ev, body) if region == symbols.builtins.globalRegion =>
      CPS.inline { k =>
        chez.Let(List(Binding(nameDef(id), chez.Builtin("box", toChez(init)))), toChez(body, k))
      }

    case State(id, init, region, ev, body) =>
      CPS.inline { k =>
       chez.Let(List(Binding(nameDef(id), chez.Builtin("fresh", Variable(nameRef(region)), toChez(init)))), toChez(body, k))
      }

    case Try(body, handler) =>
      val handlers = handler.map { h =>
        val names = RecordNames(h.interface.name)
        val cap = freshName(names.name)
        Binding(cap, toChez(h))
      }
      CPS.inline { k =>
        chez.Let(handlers,
          chez.Call(CPS.reset(chez.Call(toChez(body), CPS.lift :: handlers.map(h => Variable(h.name)))), List(k.reify)))
      }

    // TODO continue
    // [[ reset { body } ]] = k => [[ body ]] (pure) k
    case Reset(body) => CPS.join { k => chez.Expr.Call(toChezExpr(body).apply(CPS.pure), List(k.reify)) }

    // TODO deal with evidence passed to the continuation.
    // [[ shift(ev, {k} => body) ]] = ev(k => k2 => [[ body ]] k2 )
    case Shift(ev, Block.BlockLit(tparams, List(kparam), body)) =>
      CPS.lift(ev.lifts, CPS.inline { k1 =>
        chez.Let(List(chez.Binding(toChez(kparam), k1.reify)),
          toChezExpr(body).reify())
      })

    // TODO properly CPS translate
    case Region(body) =>
     CPS.inline { k => chez.Call(chez.Builtin("with-region", toChez(body)), List(k.reify)) }

    case other => CPS.inline { k => chez.Let(Nil, toChez(other, k)) }
  }

  def toChez(decl: Declaration): List[chez.Def] = decl match {
    case Declaration.Data(did, _, ctors) =>
      ctors.flatMap { ctor => generateConstructor(ctor.id, ctor.fields.map(_.id)) }

    // We use chez scheme records to also represent capabilities.
    case Declaration.Interface(id, _, operations) =>
      generateConstructor(id, operations.map(_.id))
  }

  def toChez(decl: lifted.Extern): chez.Def = decl match {
    case Extern.Def(id, tparams, params, ret, body) =>
      chez.Constant(nameDef(id),
        chez.Lambda( params.map { p => ChezName(p.id.name.name) },
          chez.RawExpr(body)))

    case Extern.Include(contents) =>
      RawDef(contents)
  }

  def toChez(defn: Definition): Either[chez.Def, Option[chez.Expr]] = defn match {
    case Definition.Def(id, block) =>
      Left(chez.Constant(nameDef(id), toChez(block)))

    case Definition.Let(Wildcard(), binding) =>
      toChez(binding) match {
        // drop the binding altogether, if it is of the form:
        //   let _ = myVariable; BODY
        // since this might lead to invalid scheme code.
        case _: chez.Variable => Right(None)
        case other => Right(Some(other))
      }

    // we could also generate a let here...
    case Definition.Let(id, binding) =>
      Left(chez.Constant(nameDef(id), toChez(binding)))
  }


  // Scheme Blocks in CPS
  // [[ { def foo; ...; IOEXPR ...; CPSEXPR } ]]_k = { [[def foo; ...]]; [[IOEXPR ...]]; [[CPSEXPR]]_k }
  def toChez(stmt: Stmt, k: Continuation): chez.Block = stmt match {

    case Scope(definitions, body) =>
      definitions.map(toChez).foldRight(toChez(body, k)) {
        case (Left(defn), chez.Block(defns, exprs, result)) => chez.Block(defn :: defns, exprs, result)
        case (Right(Some(expr)), chez.Block(Nil, exprs, result)) => chez.Block(Nil, expr :: exprs, result)
        case (Right(Some(expr)), rest) => chez.Block(Nil, expr :: Nil, chez.Let(Nil, rest))
        case (Right(None), rest) => rest
      }

    case other => chez.Block(Nil, Nil, toChezExpr(other)(k))
  }

  def toChez(block: BlockLit): chez.Lambda = block match {
    case BlockLit(tparams, params, body) =>
      val k = freshName("k")
      chez.Lambda((params map toChez),
        chez.Lambda(List(k),
          toChez(body, Continuation.Dynamic(chez.Variable(k)))))
  }

  def toChez(block: Block): chez.Expr = block match {
    case BlockVar(id, tpe) =>
      Variable(nameRef(id))

    case b @ BlockLit(tparams, params, body) => toChez(b)

    case Member(b, field, annotatedTpe) =>
      chez.Call(Variable(nameRef(field)), List(toChez(b)))

    case Unbox(e) =>
      toChez(e)

    case New(impl) => toChez(impl)
  }

  def toChez(impl: Implementation): chez.Expr =
    val ChezName(name) = nameRef(impl.interface.name)
    chez.Call(Variable(ChezName(name)), impl.operations.map { case Operation(_, block) => toChez(block) })

  def toChez(scope: Evidence): chez.Expr = toChez(scope.lifts)

  def toChez(lifts: List[Lift]): chez.Expr = lifts match {
    case el :: Nil => toChez(el)
    case el :: rest => chez.Builtin("nested", toChez(el), toChez(rest))
    case Nil => Variable(ChezName("here"))
  }

  def toChez(l: Lift): chez.Expr = l match {
    case Lift.Var(x) => chez.Variable(nameRef(x))
    case Lift.Try() => Variable(ChezName("lift"))
    case Lift.Reg() => Variable(ChezName("lift"))
  }

  def toChez(expr: Expr): chez.Expr = expr match {
    case Literal((), _) => chez.RawValue("#f")
    case Literal(s: String, _) => ChezString(s)
    case Literal(b: Boolean, _) => if (b) chez.RawValue("#t") else chez.RawValue("#f")
    case l: Literal => chez.RawValue(l.value.toString)
    case ValueVar(id, _)  => chez.Variable(nameRef(id))

    case PureApp(b, targs, args) => chez.Call(toChez(b), args map {
      case e: Expr  => toChez(e)
      case b: Block => toChez(b)
      case e: Evidence => toChez(e)
    })

    case Select(b, field, _) =>
      chez.Call(nameRef(field), toChez(b))

    case Box(b) => toChez(b)

    case Run(s) => toChezExpr(s).run
  }

  /**
   * This is mostly copy and pasted from the ML backend [[ml.ML.CPS]]. At some point, we should
   * add yet another intermediate representation for CPS to share this (and optimizations on it).
   */
  enum Continuation {
    case Dynamic(cont: chez.Expr)
    case Static(cont: chez.Expr => chez.Expr)

    def apply(e: chez.Expr): chez.Expr = this match {
      case Continuation.Dynamic(k) => chez.Call(k, List(e))
      case Continuation.Static(k) => k(e)
    }

    def reify: chez.Expr = this match {
      case Continuation.Dynamic(k) => k
      case Continuation.Static(k) =>
        val a = freshName("a")
        chez.Lambda(List(a), k(chez.Variable(a)))
    }

    def reflect: chez.Expr => chez.Expr = this match {
      case Continuation.Static(k) => k
      case Continuation.Dynamic(k) => a => chez.Call(k, List(a))
    }
  }
  class CPS(prog: Continuation => chez.Expr) {
    def apply(k: Continuation): chez.Expr = prog(k)
    def apply(k: chez.Expr): chez.Expr = prog(Continuation.Dynamic(k))
    def apply(k: chez.Expr => chez.Expr): chez.Expr = prog(Continuation.Static(k))

    def flatMap(f: chez.Expr => CPS): CPS = CPS.inline(k => prog(Continuation.Static(a => f(a)(k))))
    def map(f: chez.Expr => chez.Expr): CPS = flatMap(a => CPS.pure(f(a)))
    def run: chez.Expr = prog(Continuation.Static(a => a))

    def reify(): chez.Expr =
      val k = freshName("k")
      chez.Lambda(List(k), this.apply(Continuation.Dynamic(chez.Expr.Variable(k))))
  }

  object CPS {

    def inline(prog: Continuation => chez.Expr): CPS = CPS(prog)
    def join(prog: Continuation => chez.Expr): CPS = CPS {
      case k: Continuation.Dynamic => prog(k)
      case k: Continuation.Static =>
        val kName = freshName("k")
        chez.Let(List(Binding(kName, k.reify)),
          prog(Continuation.Dynamic(chez.Variable(kName))))
    }

    def reflect(e: chez.Expr): CPS =
      CPS.join { k => chez.Expr.Call(e, List(k.reify)) }

    def lift(lifts: List[Lift], m: CPS): CPS = lifts match {

      // TODO implement lift for reg properly
      case (Lift.Try() | Lift.Reg()) :: lifts =>
        val k2 = freshName("k2")
        lift(lifts, CPS.inline { k1 => chez.Lambda(List(k2),
          m.apply(a => chez.Expr.Call(chez.Expr.Call(k1.reify, List(a)), List(chez.Expr.Variable(k2))))
        )})

      // [[ [ev :: evs](m) ]] = ev([[ [evs](m) ]])
      case Lift.Var(x) :: lifts =>
        CPS.reflect(chez.Call(nameRef(x), lift(lifts, m).reify()))

      case Nil => m
    }

    def reset(prog: chez.Expr): chez.Expr =
      chez.Call(prog, List(pure))


    // fn a => fn k2 => k2(a)
    def pure: chez.Expr =
      val a = freshName("a")
      val k2 = freshName("k2")
      chez.Lambda(List(a), chez.Lambda(List(k2), chez.Call(k2, chez.Variable(a))))

    // TODO generate
    def lift: chez.Expr = chez.Variable(ChezName("lift"))

    def pure(expr: chez.Expr): CPS = CPS.inline(k => k(expr))

    def runMain(main: ChezName): chez.Expr = chez.Call(chez.Call(main, id), List(id))

    def id =
      val a = ChezName("a")
      chez.Lambda(List(a), chez.Variable(a))
  }

  def freshName(s: String): ChezName =
    ChezName(s + Symbol.fresh.next())

  def generateStateAccessors: List[chez.Function] = {
    val ref = ChezName("ref")
    val value = ChezName("value")
    val ev = ChezName("ev")

    val getter = chez.Function(nameDef(symbols.builtins.TState.get), List(ref),
      chez.Lambda(List(ev), CPS.pure(chez.Builtin("unbox", Variable(ref))).reify()))

    val setter = chez.Function(nameDef(symbols.builtins.TState.put), List(ref),
      chez.Lambda(List(ev, value), CPS.pure(chez.Builtin("set-box!", Variable(ref), Variable(value))).reify()))

    List(getter, setter)
  }
}
