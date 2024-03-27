package effekt
package generator
package ml

import effekt.context.Context
import effekt.lifted.*
import effekt.core.Id
import effekt.symbols.{ Module, Symbol, TermSymbol, Wildcard }
import effekt.util.messages.INTERNAL_ERROR
import effekt.util.paths.*
import kiama.output.PrettyPrinterTypes.Document

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.collection.mutable

object Transformer {

  val mlFeatureFlags: List[String] = List("ml")

  def runMain(main: MLName): ml.Expr = CPS.runMain(main)

  def compilationUnit(mainSymbol: Symbol, core: ModuleDecl)(using C: Context): ml.Toplevel = {
    ml.Toplevel(toML(core)(using TransformerContext(C)), runMain(name(mainSymbol)))
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".sml"


  // interfaces are typed structurally: we only generate one datatype for
  // each arity.
  case class CoData(name: MLName, accessors: List[MLName])

  case class TransformerContext(
    // for error reporting
    compilerContext: Context,

    recordCache: mutable.Map[Int, CoData] = mutable.Map.empty,
    accessorCache: mutable.Map[Id, MLName] = mutable.Map.empty
  )
  implicit def useAsContext(C: TransformerContext): Context = C.compilerContext



  def toML(p: Param): MLName = name(p.id)

  def toML(e: Argument)(using TransformerContext): ml.Expr = e match {
    case e: lifted.Expr => toML(e)
    case b: lifted.Block => toML(b)
    case e: lifted.Evidence => toML(e)
  }

  def toML(module: ModuleDecl)(using TransformerContext): List[ml.Binding] = {
    val decls = sortDeclarations(module.decls).flatMap(toML)
    val externs = module.externs.map(toML)
    val rest = sortDefinitions(module.definitions).map(toML)
    decls ++ externs ++ rest
  }

  /**
   * Sorts the definitions topologically. Fails if functions are mutually recursive, since this
   * is not supported by the ml backend, yet.
   *
   * Keep let-definitions in the order they are, since they might have observable side-effects
   */
  def sortDefinitions(defs: List[Definition])(using C: TransformerContext): List[Definition] = {
    def sort(defs: List[Definition], toSort: List[Definition]): List[Definition] = defs match {
      case (d: Definition.Let) :: rest  =>
        val sorted = sortTopologically(toSort, d => freeVariables(d).vars.keySet, d => d.id)
         sorted ++ (d :: sort(rest, Nil))
      case (d: Definition.Def) :: rest => sort(rest, d :: toSort)
      case Nil => sortTopologically(toSort, d => freeVariables(d).vars.keySet, d => d.id)
    }

    sort(defs, Nil)
  }

  def sortDeclarations(defs: List[Declaration])(using C: TransformerContext): List[Declaration] =
    sortTopologically(defs, d => freeTypeVariables(d), d => d.id)

  def sortTopologically[T](defs: List[T], dependencies: T => Set[Id], id: T => Id)(using C: TransformerContext): List[T] = {
    val ids = defs.map(id).toSet
    val fvs = defs.map{ d => id(d) -> dependencies(d).intersect(ids) }.toMap

    @tailrec
    def go(todo: List[T], out: List[T], emitted: Set[Id]): List[T] =
      if (todo.isEmpty) {
        out
      } else {
        val (noDependencies, rest) = todo.partition { d => (fvs(id(d)) -- emitted).isEmpty }
        if (noDependencies.isEmpty) {
          val mutuals = rest.map(id).mkString(", ")
          C.abort(s"Mutual definitions are currently not supported by this backend.\nThe following definitions could be mutually recursive: ${mutuals} ")
        } else go(rest, noDependencies ++ out, emitted ++ noDependencies.map(id).toSet)
    }

    go(defs, Nil, Set.empty).reverse
  }

  def tpeToML(tpe: BlockType)(using C: TransformerContext): ml.Type = tpe match {
    case BlockType.Function(tparams, eparams, vparams, bparams, ret) if tparams.nonEmpty =>
      C.abort("polymorphic functions not supported")
    case BlockType.Function(Nil, Nil, Nil, Nil, resType) =>
      ml.Type.Fun(List(ml.Type.Unit), tpeToML(resType))
    case BlockType.Function(Nil, Nil, vtpes, Nil, resType) =>
      ml.Type.Fun(vtpes.map(tpeToML), tpeToML(resType))
    case BlockType.Function(tparams, eparams, vparams, bparams, result) =>
      C.abort("higher order functions currently not supported")
    case BlockType.Interface(typeConstructor, args) =>
      ml.Type.TApp(ml.Type.Data(interfaceNameFor(args.size)), args.map(tpeToML))
  }

  def tpeToML(tpe: ValueType)(using TransformerContext): ml.Type = tpe match {
    case lifted.Type.TUnit => ml.Type.Unit
    case lifted.Type.TInt => ml.Type.Integer
    case lifted.Type.TDouble => ml.Type.Real
    case lifted.Type.TBoolean => ml.Type.Bool
    case lifted.Type.TString => ml.Type.String
    case ValueType.Var(id) => ml.Type.Var(name(id))
    case ValueType.Data(id, Nil) => ml.Type.Data(name(id))
    case ValueType.Data(id, args) => ml.Type.TApp(ml.Type.Data(name(id)), args.map(tpeToML))
    case ValueType.Boxed(tpe) => tpeToML(tpe)
  }

  def toML(decl: Declaration)(using C: TransformerContext): List[ml.Binding] = decl match {

    case Declaration.Data(id: symbols.TypeConstructor.Record, tparams, List(ctor)) =>
      defineRecord(name(id), name(id.constructor), ctor.fields.map { f => name(f.id) })

    case Declaration.Data(id, tparams, ctors) =>
      def constructorToML(c: Constructor): (MLName, Option[ml.Type]) = c match {
        case Constructor(id, fields) =>
          val tpeList = fields.map { f => tpeToML(f.tpe) }
          val tpe = typesToTupleIsh(tpeList)
          (name(id), tpe)
      }

      val tvars: List[ml.Type.Var] = tparams.map(p => ml.Type.Var(name(p)))
      List(ml.Binding.DataBind(name(id), tvars, ctors map constructorToML))

    case Declaration.Interface(id, tparams, operations) =>
      defineInterface(id, operations.map { op => op.id })
  }

  def interfaceNameFor(arity: Int): MLName = MLName(s"Object${arity}")

  def defineInterface(typeName: Id, props: List[Id])(using C: TransformerContext): List[ml.Binding] = {
    val arity = props.size

    val interfaceName = interfaceNameFor(arity)
    val accessorNames = props.zipWithIndex.map { case (id, i) =>
      val name = MLName(s"member${i + 1}of${arity}")
      C.accessorCache.update(id, name)
      name
    }

    if C.recordCache.isDefinedAt(arity) then return Nil

    C.recordCache.update(arity, CoData(interfaceName, accessorNames))

    defineRecord(interfaceName, interfaceName, accessorNames)
  }

  def defineRecord(typeName: MLName, constructorName: MLName, fields: List[MLName])(using TransformerContext): List[ml.Binding] = {
    // we introduce one type var for each property, in order to avoid having to translate types
    val tvars: List[ml.Type.Var] = fields.map(_ => ml.Type.Var(freshName("a")))
    val dataDecl = ml.Binding.DataBind(typeName, tvars, List((constructorName, typesToTupleIsh(tvars))))

    val accessors = fields.zipWithIndex.map {
      case (fieldName, i) =>
        val arg = MLName("arg")
        // _, _, _, arg, _
        val patterns = fields.indices.map {
          j => if j == i then ml.Pattern.Named(arg) else ml.Pattern.Wild()
        }.toList

        ml.Binding.FunBind(fieldName,
          List(ml.Param.Patterned(ml.Pattern.Datatype(constructorName, patterns))),
          ml.Expr.Variable(arg))
    }
    dataDecl :: accessors
  }

  def toML(ext: Extern)(using TransformerContext): ml.Binding = ext match {
    case Extern.Def(id, tparams, params, ret, bodies) =>
      bodies.forFeatureFlags(mlFeatureFlags).getOrElse{ ??? /* TODO insert hole*/ } match {
        case ExternBody.StringExternBody(_, body) =>
          ml.FunBind(name(id), params map { p => ml.Param.Named(name(p.id)) }, toML(body))
        case ExternBody.EffektExternBody(_, body) => sys error "Effekt extern body should have been removed"
      }
    case Extern.Include(ff, contents) if ff.matches(mlFeatureFlags) =>
      RawBind(contents)
    case Extern.Include(ff, contents) =>
      RawBind("") // ignore, not meant for us
  }

  def toML(t: Template[lifted.Expr])(using TransformerContext): ml.Expr =
    ml.RawExpr(t.strings, t.args.map(e => toML(e)))

  def toMLExpr(stmt: Stmt)(using C: TransformerContext): CPS = stmt match {
    case lifted.Return(e) => CPS.pure(toML(e))

    case lifted.Get(id, ev, tpe) =>
      // ev (k => s => k s s)
      CPS.lift(ev.lifts,
        // TODO share the reified s
        CPS.reflected { k => CPS.reflected { s => k(s.reify)(s) }})


    case lifted.Put(id, ev, value) =>
      // ev (k => s2 => k () value)
      CPS.lift(ev.lifts,
        CPS.reflected { k => CPS.reflected { s2 => k(ml.Consts.unitVal)(toML(value)) }})

    case lifted.App(b, targs, args) => CPS.inline { k =>
      ml.Expr.Call(toML(b), (args map toML) ++ List(k.reify))
    }

    // As long as one of the two branches is known to be in CPS we "pull out" the
    // static continuation.
    // Then, the continuation is reified in order to generate a join-point.
    case lifted.If(cond, thn, els) =>
      CPS.reified(CPS.zip(toMLExpr(thn), toMLExpr(els)) { case (e1, e2) =>
        CPS.reified {
          ml.If(toML(cond), e1, e2)
        }
      }.reify())

    case lifted.Val(id, binding, body) =>
      toMLExpr(binding).flatMap { value =>
        toMLExpr(body).mapComputation { b => ml.mkLet(List(ml.ValBind(name(id), value)), b) }
      }

    case lifted.Match(scrutinee, clauses, default) =>
      def clauseToML(c: (Id, BlockLit)): (ml.Pattern, CPS) = {
        val (id, b) = c
        val binders = b.params.map(p => ml.Pattern.Named(name(p.id)))
        val pattern = ml.Pattern.Datatype(name(id), binders)
        (pattern, toMLExpr(b.body))
      }

      val (patterns, bodies) = clauses.map(clauseToML).unzip

      // We eta-expand all computations (including the default clause).
      val prog = CPS.zipAll(default.toList.map(toMLExpr) ++ bodies) {
        // no default clause
        case mlBodies if default.isEmpty =>
          val mlClauses = (patterns zip mlBodies).map { ml.MatchClause.apply }
          CPS.reified(ml.Match(toML(scrutinee), mlClauses, None))

        case default :: mlBodies =>
          val mlClauses = (patterns zip mlBodies).map { ml.MatchClause.apply }
          CPS.reified(ml.Match(toML(scrutinee), mlClauses, Some(default)))

        case _ => INTERNAL_ERROR("Cannot happen since default is non empty")
      }

      // create join points
      CPS.reified(prog.reify())

    // TODO maybe don't drop the continuation here? Although, it is dead code.
    case lifted.Hole() => CPS.inline { k => ml.RawExpr("raise Hole") }

    case lifted.Scope(definitions, body) => CPS.reflected { k =>
      // TODO couldn't it be that the definitions require the continuation?
      //  Right now, the continuation is only passed to the body.
      toMLExpr(body)(k).mapComputation { b => ml.mkLet(sortDefinitions(definitions).map(toML), b) }
    }

    case lifted.Alloc(id, init, region, ev, body) if region == symbols.builtins.globalRegion =>
      CPS.inline { k =>
        val bind = ml.Binding.ValBind(name(id), ml.Expr.Ref(toML(init)))
        ml.mkLet(List(bind), toMLExpr(body)(k).reify())
      }

    case lifted.Alloc(id, init, region, ev, body) =>
      CPS.inline { k =>
        val bind = ml.Binding.ValBind(name(id), ml.Call(ml.Consts.fresh)(ml.Variable(name(region)), toML(init)))
        ml.mkLet(List(bind), toMLExpr(body)(k).reify())
      }

    // Only used before monomorphization
    // [[ state(init) { (ev, x) => stmt } ]]_k = [[ { ev => stmt } ]] LIFT_STATE (a => s => k a)
    case Var(init, Block.BlockLit(_, List(ev, x), body)) =>
      CPS.inline { k =>
        // a => s => k a
        val returnCont = Continuation.Static { a => CPS.reflected { s => k(a) } }

        // m => k => s => m (a => k a s)
        def lift = {
          val m = freshName("m")
          val k = freshName("k")
          val a = freshName("a")
          val s = freshName("s")
          ml.Lambda(m)(ml.Lambda(k)(ml.Lambda(s)(
            ml.Call(m)(ml.Lambda(a)(ml.Call(ml.Call(k)(a))(s))))))
        }

        ml.mkLet(List(Binding.ValBind(name(ev.id), lift)),
          toMLExpr(body)(returnCont)(toML(init)).reify())
      }
    // after monomorphization
    case Var(init, Block.BlockLit(_, List(x), body)) =>
      CPS.reflected { k =>
        // a => s => k a
        val returnCont = Continuation.Static { a => CPS.reflected { s => k(a) } }
        toMLExpr(body)(returnCont)(toML(init))
      }
    case v: Var => C.panic("The body of var is always a block lit with two arguments (evidence and block)")

    // Only used before monomorphization
    case lifted.Try(body, handler) =>
      val args = ml.Consts.lift :: handler.map(toML)
      CPS.reflected { k =>
        CPS.reset(ml.Call(toML(body))(args: _*))(k)
      }

    case Reset(body) =>
      toMLExpr(body)(CPS.pure)

    // non-monomorphized version (without the scoped resumptions requirement)
    // [[ shift(ev, {k} => body) ]] = ev(k1 => k2 => let k ev a = ev (k1 a) in [[ body ]] k2)
    //    case Shift(ev, Block.BlockLit(tparams, List(kparam), body)) =>
    //      CPS.lift(ev.lifts, CPS.inline { k1 =>
    //        val a = freshName("a")
    //        val ev = freshName("ev")
    //        mkLet(List(
    //          ml.Binding.FunBind(toML(kparam), List(ml.Param.Named(ev), ml.Param.Named(a)),
    //            ml.Call(ev)(ml.Call(k1.reify)(ml.Expr.Variable(a))))),
    //          toMLExpr(body).reify())
    //      })

    // monomorphized version (with scoped resumptions requirement)
    case Shift(ev, body) =>
      CPS.lift(ev.lifts, CPS.reflected(CPS.reflect(toML(body))))

    //    case Shift(_, _) => INTERNAL_ERROR("Should not happen, body of shift is always a block lit with one parameter for the continuation.")

    // Only used before monomorphization
    case Region(body) =>
      CPS.inline { k => ml.Call(ml.Call(ml.Consts.withRegion)(toML(body)), List(k.reify)) }
  }

  def createBinder(id: Symbol, binding: Expr)(using TransformerContext): Binding = {
    ml.ValBind(name(id), toML(binding))
  }

  def createBinder(id: Symbol, binding: Block)(using TransformerContext): Binding = {
    binding match {
      case BlockLit(tparams, params, body) =>
        val k = freshName("k")
        ml.FunBind(name(id), params.map(p => ml.Param.Named(toML(p))) :+ ml.Param.Named(k), toMLExpr(body)(ml.Variable(k)).reify())
      case New(impl) =>
        toML(impl) match {
          case ml.Lambda(ps, body) =>
            ml.FunBind(name(id), ps, body)
          case other => ml.ValBind(name(id), other)
        }
      case _ =>
        ml.ValBind(name(id), toML(binding))
    }
  }

  def toML(defn: Definition)(using C: TransformerContext): ml.Binding = defn match {
    case Definition.Def(id, block) => createBinder(id, block)
    case Definition.Let(Wildcard(), binding) => ml.Binding.AnonBind(toML(binding))
    case Definition.Let(id, binding) => createBinder(id, binding)
  }

  def toML(block: BlockLit)(using TransformerContext): ml.Lambda = block match {
    case BlockLit(tparams, params, body) =>
      val k = freshName("k")
      ml.Lambda(params.map { p => ml.Param.Named(name(p.id)) } :+ ml.Param.Named(k), toMLExpr(body)(ml.Variable(k)).reify())
  }

  def toML(block: Block)(using C: TransformerContext): ml.Expr = block match {
    case lifted.BlockVar(id, _) =>
      Variable(name(id))

    case b @ lifted.BlockLit(_, _, _) =>
      toML(b)

    case lifted.Member(b, field, annotatedType) =>
      ml.Call(C.accessorCache(field))(ml.Call(toML(b))())

    case lifted.Unbox(e) => toML(e) // not sound

    case lifted.New(impl) => toML(impl)
  }

  def toML(impl: Implementation)(using TransformerContext): ml.Expr = impl match {
    case Implementation(interface, operations) =>
      ml.Lambda() {
        ml.Expr.Make(interfaceNameFor(operations.size), expsToTupleIsh(operations map toML))
      }
  }

  def toML(op: Operation)(using TransformerContext): ml.Expr = toML(op.implementation)

  def toML(scope: Evidence): ml.Expr = scope match {
    case Evidence(Nil) => Consts.here
    case Evidence(ev :: Nil) => toML(ev)
    case Evidence(scopes) =>
      scopes.map(toML).reduce(ml.Call(Consts.nested)(_, _))
  }

  def toML(l: Lift): ml.Expr = l match {
    case Lift.Try() => Consts.lift
    case Lift.Var(x) => Variable(name(x))
    case Lift.Reg() => Consts.lift
  }

  def toML(expr: Expr)(using C: TransformerContext): ml.Expr = expr match {
    case l: Literal =>
      def numberString(x: AnyVal): ml.Expr = {
        val s = x.toString
        if (s.startsWith("-")) {
          ml.RawExpr(s"~${s.substring(1)}")
        } else ml.RawValue(s)
      }

      l.value match {
        case v: Byte => numberString(v)
        case v: Short => numberString(v)
        case v: Int => numberString(v)
        case v: Long => numberString(v)
        case v: Float => numberString(v)
        case v: Double => numberString(v)
        case _: Unit => Consts.unitVal
        case v: String => MLString(v)
        case v: Boolean => if (v) Consts.trueVal else Consts.falseVal
        case _ => ml.RawValue(l.value.toString)
      }
    case ValueVar(id, _) => ml.Variable(name(id))

    case Make(data, tag, vargs) =>
      ml.Expr.Make(name(tag), expsToTupleIsh(vargs map toML))

    case PureApp(b, _, args) =>
      val mlArgs = args map {
        case e: Expr => toML(e)
        case b: Block => toML(b)
        case e: Evidence => toML(e)
      }
      ml.Call(toML(b), mlArgs)

    case Select(b, field, _) =>
      ml.Call(name(field))(toML(b))

    case Run(s) => toMLExpr(s).run

    case Box(b) => toML(b) // not sound
  }

  enum Continuation {
    case Dynamic(cont: ml.Expr)
    case Static(cont: ml.Expr => CPS)

    // Apply this continuation to its argument
    def apply(e: ml.Expr): CPS = this match {
      case Continuation.Dynamic(k) => CPS.reified(ml.Call(k)(e))
      case Continuation.Static(k) => k(e)
    }

    // Reify this continuation fully into an ML expression
    def reify: ml.Expr = this match {
      case Continuation.Dynamic(k) => k
      case Continuation.Static(k) =>
        val a = freshName("a")
        ml.Lambda(ml.Param.Named(a)) { k(ml.Variable(a)).reify() }
    }

    // Reflect one layer of this continuation
    def reflect: ml.Expr => CPS = this match {
      case Continuation.Static(k) => k
      // here reflection could also be improved
      case Continuation.Dynamic(k) => a => CPS.reified(ml.Call(k)(a))
    }
  }


  enum CPS {

    case Reflected(prog: Continuation => CPS)
    case Reified(prog: ml.Expr)

    // TODO maybe differentiate between reflected control and state?
    //    case class State(expr: ml.Expr)
    //    case ReflectState(prog: State => CPS)

    // "reset" this CPS term with the given continuation
    def apply(k: Continuation): CPS = this match {
      case CPS.Reflected(prog) => prog(k)
      case CPS.Reified(prog) => CPS.reflect(prog)(k)
    }
    def apply(k: ml.Expr): CPS = apply(Continuation.Dynamic(k))
    def apply(k: ml.Expr => CPS): CPS = apply(Continuation.Static(k))

    def flatMap(f: ml.Expr => CPS): CPS = this match {
      case Reflected(prog) => Reflected(k => prog(Continuation.Static(a => f(a)(k))))
      case Reified(prog) => Reflected(k => CPS.reflect(prog)(Continuation.Static(a => f(a)(k))) )
    }

    def map(f: ml.Expr => ml.Expr): CPS = flatMap(a => CPS.pure(f(a)))
    def run: ml.Expr = apply(Continuation.Static(a => CPS.reified(a))).reify()

    // Recursively reify this CPS computation into an ML expression
    def reify(): ml.Expr  = this match {
      case Reflected(prog) =>
        val k = freshName("k")
        ml.Lambda(ml.Param.Named(k))(prog(Continuation.Dynamic(ml.Expr.Variable(k))).reify())
      case Reified(prog) => prog
    }

    // f(LAM k => BODY[k])   =  LAM k => f(BODY[k])
    def mapComputation(f: ml.Expr => ml.Expr): CPS = flatMapComputation { e => CPS.Reified(f(e)) }

    def flatMapComputation(f: ml.Expr => CPS): CPS = this match {
      case CPS.Reflected(prog) => CPS.reflected { k => prog(k).flatMapComputation(f) }
      case CPS.Reified(prog) => f(prog)
    }

  }

  object CPS {
    def reified(prog: ml.Expr): CPS = Reified(prog)
    def reflected(prog: Continuation => CPS): CPS = Reflected(prog)
    def inline(prog: Continuation => ml.Expr): CPS = Reflected(k => CPS.reified(prog(k)))

    // used for join points to distribute continuations to the outside (see if)
    //  zip (LAM k => s) e f = LAM k => zip s[k] (e k.reify) f
    //  zip e1 e2 f          = f e1 e2
    def zip(c1: CPS, c2: CPS)(f: (ml.Expr, ml.Expr) => CPS): CPS = (c1, c2) match {
      case (CPS.Reified(prog1), CPS.Reified(prog2)) => f(prog1, prog2)
      case _ => CPS.reflected { k => zip(c1(k), c2(k))(f) }
    }

    // generalizing zip from two computations to many (used by match)
    def zipAll(cs: List[CPS])(f: List[ml.Expr] => CPS): CPS =
      if cs.forall {
          case _: Reified => true
          case _ => false
        }
      then f(cs.collect { case Reified(prog) => prog })
      else CPS.reflected { k => zipAll(cs.map(_.apply(k)))(f) }

    def reset(prog: ml.Expr): CPS =
      reflect(prog)(pure)

    // fn a => fn k2 => k2(a)
    def pure: Continuation =
      Continuation.Static { a => CPS.reflected { k2 => k2(a) }}

    // reflect one layer of CPS
    def reflect(e: ml.Expr): Continuation => CPS = e match {
      // (k => )
      case ml.Expr.Lambda(List(ml.Param.Named(k)), body) =>
        kValue => CPS.reified(utils.substituteTerms(body)(k -> kValue.reify))
      case ml.Expr.Lambda(ml.Param.Named(k) :: ps, body) =>
        kValue => CPS.reified(ml.Expr.Lambda(ps, utils.substituteTerms(body)(k -> kValue.reify)))
      case e =>
        k => CPS.reified(ml.Call(e)(k.reify))
    }

    // [[ Try() ]] = m k1 k2 => m (fn a => k1 a k2);
    def lift(lifts: List[Lift], m: CPS): CPS = lifts match {

      // [[ [Try :: evs](m) ]] = [[evs]](k1 => k2 => m(a => k1 a k2))
      case Lift.Try() :: lifts =>
        lift(lifts, CPS.reflected { k1 => CPS.reflected { k2 =>
          m.apply(a => k1(a)(k2))
        }})

      // [[ [Try :: evs](m) ]] = [[evs]](k => s => m(a => k a s))
      case Lift.Reg() :: lifts =>
        lift(lifts, CPS.reflected { k => CPS.reflected { s =>
          m.apply(a => k(a)(s))
        }})

      // [[ [ev :: evs](m) ]] = [[evs]]( [[ev]](m) )
      case Lift.Var(x) :: lifts =>
        CPS.reified(ml.Call(Variable(name(x)))(lift(lifts, m).reify()))

      case Nil => m
    }

    // TODO actually find main function and reflect body (passing static ID continuation)
    def runMain(main: MLName): ml.Expr =
      // when using monomorphization
      ml.Call(main)(id)
      // when not using monomorphization
      // ml.Call(main)(id, id)

    def id =
      val a = MLName("a")
      ml.Lambda(ml.Param.Named(a))(ml.Variable(a))
  }

  def typesToTupleIsh(types: List[ml.Type]): Option[ml.Type] = types match {
    case Nil => None
    case one :: Nil => Some(one)
    case fieldTypes => Some(ml.Type.Tuple(fieldTypes))
  }

  def expsToTupleIsh(exps: List[ml.Expr]): Option[ml.Expr] = exps match {
    case Nil => None
    case one :: Nil => Some(one)
    case exps => Some(ml.Expr.Tuple(exps))
  }
}
