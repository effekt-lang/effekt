package effekt
package generator
package ml

import effekt.context.Context
import effekt.lifted.*
import effekt.symbols.{Anon, Binder, BlockSymbol, BlockType, Module, Symbol, TermSymbol, TypeConstructor, TypeSymbol, ValueSymbol, ValueType, Wildcard}
import effekt.util.paths.*
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

/**
 * Lifted variant of Chez Scheme. Mostly copy-and-paste from [[ChezScheme]].
 *
 * Difficult to share the code, since core and lifted are different IRs.
 */
object ML extends Backend {

  def runMain(main: MLName): ml.Expr =
    ml.Call(Consts.run)(ml.Call(main)(Consts.here))

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, mainSymbol: TermSymbol)(using C: Context): Option[Compiled] = {

    assert(main.core.imports.isEmpty, "All dependencies should have been inlined by now.")

    val mainSymbol = C.checkMain(main.mod)

    LiftInference(main).map { lifted =>
      val mlModule = compilationUnit(mainSymbol, lifted.core)
      val result = ml.PrettyPrinter.pretty(ml.PrettyPrinter.toDoc(mlModule), 100)
      val mainFile = path(main.mod)
      Compiled(main.source, mainFile, Map(mainFile -> result))
    }
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: AllTransformed)(using C: Context): Option[Document] =
    C.using(module = input.main.mod) {
      Some(ml.PrettyPrinter.format(ml.PrettyPrinter.toDoc(compile(input.main))))
    }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(using Context): List[ml.Binding] =
    LiftInference(in).toList.flatMap { lifted => toML(lifted.core) }

  def compilationUnit(mainSymbol: Symbol, core: ModuleDecl)(implicit C: Context): ml.Toplevel = {
    ml.Toplevel(toML(core), runMain(name(mainSymbol)))
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".sml"


  def toML(p: Param): MLName = name(p.id)

  def toML(e: Argument)(using Context): ml.Expr = e match {
    case e: lifted.Expr => toML(e)
    case b: lifted.Block => toML(b)
    case e: lifted.Evidence => toML(e)
  }

  def toML(module: ModuleDecl)(using Context): List[ml.Binding] = {
    val decls = module.decls.flatMap(toML)
    val externs = module.externs.map(toML)
    val rest = module.definitions.map(toML)
    decls ++ externs ++ rest
  }

  def tpeToML(tpe: symbols.BlockType)(using C: Context): ml.Type = tpe match {
    case BlockType.FunctionType(tparams, _, _, _, _, _) if tparams.nonEmpty =>
      C.abort("polymorphic functions not supported")
    case BlockType.FunctionType(Nil, Nil, Nil, Nil, resType, _) =>
      ml.Type.Fun(List(ml.Type.Unit), tpeToML(resType))
    case BlockType.FunctionType(Nil, Nil, vtpes, Nil, resType, _) =>
      ml.Type.Fun(vtpes.map(tpeToML), tpeToML(resType))
    case BlockType.FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
      ???
    case BlockType.InterfaceType(typeConstructor, args) =>
        ml.Type.Tapp(ml.Type.Data(name(typeConstructor)), args.map(tpeToML))
  }

  def tpeToML(tpe: symbols.ValueType)(using Context): ml.Type = tpe match {
    case ValueType.BoxedType(blockType, _) =>
      tpeToML(blockType)
    case ValueType.ValueTypeRef(tvar) =>
      // Note that types with generic equality needs to be `''a` which is
      // ... annoying
      ml.Type.Var(name(tvar))
    case ValueType.ValueTypeApp(tc, Nil) =>
      tcToML(tc)
    case ValueType.ValueTypeApp(tc, args) =>
      ml.Type.Tapp(tcToML(tc), args.map(tpeToML))
  }

  def tcToML(tc: symbols.TypeConstructor): ml.Type = tc match {
    case symbols.builtins.UnitSymbol       => ml.Type.Unit
    case symbols.builtins.IntSymbol        => ml.Type.Integer
    case symbols.builtins.DoubleSymbol     => ml.Type.Real
    case symbols.builtins.BooleanSymbol    => ml.Type.Bool
    case symbols.builtins.StringSymbol     => ml.Type.String
    case TypeConstructor.DataType(_, _, _) => ml.Type.Data(name(tc))
    case TypeConstructor.Record(_, _, _)   => ml.Type.Data(name(tc))
    case TypeConstructor.ExternType(_, _)  => ml.Type.Builtin(name(tc))
  }

  def toML(decl: Decl)(using C: Context): List[ml.Binding] = decl match {
    case Data(did: TypeConstructor.DataType, ctors) =>
      def constructorToML(s: Symbol): (MLName, Option[ml.Type]) = s match {
        case symbols.Constructor(_, _, fields, _) =>
          val tpeList = fields.map(fieldSymbolType)
          val tpe = typesToTupleIsh(tpeList)
          (name(s), tpe)
        case _ => C.panic("Constructor's symbol is not a constructor symbol")
      }

      val tvars: List[ml.Type.Var] = did.tparams.map(p => ml.Type.Var(name(p)))
      List(ml.Binding.DataBind(name(did), tvars, ctors map constructorToML))

    case Decl.Data(id: TypeConstructor.Record, List(ctor: symbols.Constructor)) =>
      recordRep(id, id.constructor, ctor.fields)
    case Decl.Interface(id, operations) => recordRep(id, id, operations)
    case Data(_, _) => C.panic("Data symbol is not DataType or Record")
  }

  def recordRep(typeSym: Symbol, caseSym: Symbol, terms: List[Symbol])(using Context): List[ml.Binding] = {
    val caseName = name(caseSym)
    val tvars: List[ml.Type.Var] = terms.map(_ => ml.Type.Var(freshName("arg")))
    val dataDecl = ml.Binding.DataBind(name(typeSym), tvars, List((caseName, typesToTupleIsh(tvars))))
    val accessors = terms.zipWithIndex.map {
      case (sym, i) =>
        val fieldName = name(sym)
        val patterns = terms.indices.map(
          j => if (j == i) ml.Pattern.Named(fieldName) else ml.Pattern.Wild()
        ).toList
        val pattern = ml.Pattern.Datatype(caseName, patterns)
        val args = List(ml.Param.Patterned(pattern))
        val body = ml.Expr.Variable(fieldName)
        ml.Binding.FunBind(dataSelectorName(caseSym, sym), args, body)
    }
    dataDecl :: accessors
  }

  def toML(ext: Extern)(using Context): ml.Binding = ext match {
    case Extern.Def(id, _, params, body) =>
      ml.FunBind(name(id), params map (paramToML(_, false)), RawExpr(body))
    case Extern.Include(contents) =>
      RawBind(contents)
  }

  def paramToML(p: Param, unique: Boolean = true)(using Context): ml.Param = {
    val id = if (unique) name(p.id) else MLName(p.id.name.toString)
    ml.Param.Named(id)
  }

  def toMLExpr(stmt: Stmt)(using C: Context): CPS = stmt match {
    case Return(e) => CPS.pure(toML(e))

    case App(b, _, args) => CPS.inline { k => Expr.Call(Expr.Call(toML(b), args map toML), List(k.reify)) }

    case If(cond, thn, els) =>
      CPS.join { k =>
        ml.If(toML(cond), toMLExpr(thn)(k), toMLExpr(els)(k))
      }

    case Val(id, _, binding, body) =>
      toMLExpr(binding).flatMap { value =>
        CPS.inline { k =>
          ml.mkLet(List(ml.ValBind(name(id), value)), toMLExpr(body)(k))
        }
      }

    case Match(scrutinee, clauses, default) => CPS.join { k =>
      def clauseToML(c: (symbols.Constructor, BlockLit)): ml.MatchClause = {
        val (constructor, b) = c
        val binders = b.params.map(p => ml.Pattern.Named(name(p.id)))
        val pattern = constructor.tpe match {
          case symbols.Record(_, _, _) =>
            val tag = name(constructor)
            ml.Pattern.Datatype(tag, binders)
          case symbols.DataType(_, _, _) =>
            val tag = name(constructor)
            ml.Pattern.Datatype(tag, binders)
          case _: symbols.ExternType => C.abort("Match on ExternType is not supported")
        }
        val body = toMLExpr(b.body)(k)
        ml.MatchClause(pattern, body)
      }

      ml.Match(toML(scrutinee), clauses map clauseToML, default map { d => toMLExpr(d)(k) })
    }

    // TODO maybe don't drop the continuation here? Although, it is dead code.
    case Hole => CPS.inline { k => ml.Expr.RawExpr("raise Hole") }

    case Scope(definitions, body) => CPS.inline { k => ml.mkLet(definitions.map(toML), toMLExpr(body)(k)) }

    case State(id, init, region, body) if region == symbols.builtins.globalRegion =>
      CPS.inline { k =>
        val bind = ml.Binding.ValBind(name(id), ml.Expr.Ref(toML(init)))
        ml.mkLet(List(bind), toMLExpr(body)(k))
      }

    case State(id, init, region, body) =>
      CPS.inline { k =>
        val bind = ml.Binding.ValBind(name(id), ml.Call(ml.Consts.fresh)(ml.Variable(name(region)), toML(init)))
        ml.mkLet(List(bind), toMLExpr(body)(k))
      }

    case Try(body, _, handler) =>
      val handlers: List[ml.Expr.MakeDatatype] = handler.map { (impl: Implementation) =>
        val fields = impl.operations.map { case Operation(op, BlockLit(params, body)) =>
          val args = params.init.map(paramToML(_))
          val resumeName = name(params.last.id)
          val ev = freshName("ev")
          val evResume = freshName("ev_resume")
          val v = freshName("v")
          val k1 = freshName("k1")
          val k2 = freshName("k2")

          // ev (fn k1 => fn k2 => let fun resume ev_res v = ev_res k1(v); in body[[k2]] end)
          val newBody = ml.Call(
            ml.Expr.Variable(ev)
          )(
            ml.Lambda(
              ml.Param.Named(k1)
            )(ml.Lambda(
                ml.Param.Named(k2)
              )(ml.mkLet(
                List(ml.Binding.FunBind(
                  resumeName,
                  List(ml.Param.Named(evResume), ml.Param.Named(v)),
                  ml.Call(evResume)(ml.Call(k1)(ml.Expr.Variable(v)))
                )),
                toMLExpr(body)(ml.Variable(k2)))
              )
            )
          )
          ml.Expr.Lambda(ml.Param.Named(ev) :: args, newBody)
        }
        ml.Expr.MakeDatatype(name(impl.id), expsToTupleIsh(fields))
      }
      val args = ml.Consts.lift :: handlers

      CPS.inline { k =>
        ml.Call(CPS.reset(ml.Call(toML(body))(args: _*)), List(k.reify))
      }

    case Region(body, _) =>
      CPS.inline { k => ml.Call(ml.Call(ml.Consts.withRegion)(toML(body)), List(k.reify)) }
  }

  def createBinder(id: Symbol, binding: Expr)(using Context): Binding = {
    ml.ValBind(name(id), toML(binding))
  }

  def createBinder(id: Symbol, binding: Block)(using Context): Binding = {
    binding match {
      case BlockLit(params, body) =>
        val k = freshName("k")
        ml.FunBind(name(id), params.map(p => ml.Param.Named(toML(p))) :+ ml.Param.Named(k), toMLExpr(body)(ml.Variable(k)))
      case _ =>
        ml.ValBind(name(id), toML(binding))
    }
  }

  def toML(defn: Definition)(using C: Context): ml.Binding = defn match {
    case Definition.Def(id, _, block) => createBinder(id, block)
    case Definition.Let(Wildcard(), _, binding) => ml.Binding.AnonBind(toML(binding))
    case Definition.Let(id, _, binding) => createBinder(id, binding)
  }

  def toML(block: BlockLit)(using Context): ml.Lambda = block match {
    case BlockLit(params, body) =>
      val k = freshName("k")
      ml.Lambda(params.map(paramToML(_)) :+ ml.Param.Named(k), toMLExpr(body)(ml.Variable(k)))
  }

  def toML(block: Block)(using C: Context): ml.Expr = block match {
    case BlockVar(id, _) =>
      Variable(name(id))

    case b@BlockLit(_, _) =>
      toML(b)

    case Member(b, field) =>
      val selector = field match {
        case op: symbols.Operation =>
          dataSelectorName(op.interface, op)
        case f: symbols.Field => fieldSelectorName(f)
        case _: symbols.TermSymbol => C.panic("TermSymbol Member is not supported")
      }
      ml.Call(selector)(toML(b))

    case Unbox(e) => toML(e) // not sound

    case New(Implementation(id, operations)) =>
      ml.Expr.MakeDatatype(name(id), expsToTupleIsh(operations map toML))
  }

  def toML(op: Operation)(using Context): ml.Expr = {
    val Operation(_, implementation) = op
    toML(implementation)
  }

  def toML(scope: Evidence): ml.Expr = scope match {
    case Evidence(Nil) => Consts.here
    case Evidence(ev :: Nil) => Variable(name(ev))
    case Evidence(scopes) =>
      scopes.map(s => ml.Variable(name(s))).reduce(ml.Call(Consts.nested)(_, _))
  }

  def toML(expr: Expr)(using C: Context): ml.Expr = expr match {
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

    case lifted.PureApp(lifted.Member(lifted.BlockVar(x, _), symbols.builtins.TState.get), List(), List()) =>
      ml.Expr.Deref(ml.Variable(name(x)))

    case lifted.PureApp(lifted.Member(lifted.BlockVar(x, _), symbols.builtins.TState.put), List(), List(arg)) =>
      ml.Expr.Assign(ml.Variable(name(x)), toML(arg))

    case PureApp(b, _, args) =>
      val mlArgs = args map {
        case e: Expr => toML(e)
        case b: Block => toML(b)
        case e: Evidence => toML(e)
      }
      b match {
        case BlockVar(id@symbols.Constructor(_, _, _, symbols.TypeConstructor.DataType(_, _, _)), _) =>
          ml.Expr.MakeDatatype(name(id), expsToTupleIsh(mlArgs))
        case BlockVar(id@symbols.Constructor(_, _, _, symbols.TypeConstructor.Record(_, _, _)), _) =>
          ml.Expr.MakeDatatype(name(id), expsToTupleIsh(mlArgs))
        case _ => ml.Call(toML(b), mlArgs)
      }

    case Select(b, field, _) =>
      ml.Call(fieldSelectorName(field))(toML(b))

    case Run(s, _) => toMLExpr(s).run

    case Box(b) => toML(b) // not sound
  }

  enum Continuation {
    case Dynamic(cont: ml.Expr)
    case Static(cont: ml.Expr => ml.Expr)

    def apply(e: ml.Expr): ml.Expr = this match {
      case Continuation.Dynamic(k) => ml.Call(k)(e)
      case Continuation.Static(k) => k(e)
    }
    def reify: ml.Expr = this match {
      case Continuation.Dynamic(k) => k
      case Continuation.Static(k) =>
        val a = freshName("a")
        ml.Lambda(ml.Param.Named(a))(k(ml.Variable(a)))
    }
    def reflect: ml.Expr => ml.Expr = this match {
      case Continuation.Static(k) => k
      case Continuation.Dynamic(k) => a => ml.Call(k)(a)
    }
  }

  class CPS(prog: Continuation => ml.Expr) {
    def apply(k: Continuation): ml.Expr = prog(k)
    def apply(k: ml.Expr): ml.Expr = prog(Continuation.Dynamic(k))
    def apply(k: ml.Expr => ml.Expr): ml.Expr = prog(Continuation.Static(k))

    def flatMap(f: ml.Expr => CPS): CPS = CPS.inline(k => prog(Continuation.Static(a => f(a)(k))))
    def map(f: ml.Expr => ml.Expr): CPS = flatMap(a => CPS.pure(f(a)))
    def run: ml.Expr = prog(Continuation.Static(a => a))
  }

  object CPS {

    def inline(prog: Continuation => ml.Expr): CPS = CPS(prog)
    def join(prog: Continuation => ml.Expr): CPS = CPS {
      case k: Continuation.Dynamic => prog(k)
      case k: Continuation.Static =>
        val kName = freshName("k")
        mkLet(List(ValBind(kName, k.reify)), prog(Continuation.Dynamic(ml.Variable(kName))))
    }

    def reset(prog: ml.Expr): ml.Expr =
      val a = freshName("a")
      val k2 = freshName("k2")
      // fn a => fn k2 => k2(a)
      val pure = ml.Lambda(Param.Named(a)) { ml.Lambda(Param.Named(k2)) { ml.Call(ml.Variable(k2), List(ml.Variable(a))) }}
      ml.Call(prog, List(pure))

    def pure(expr: ml.Expr): CPS = CPS.inline(k => k(expr))

    def id =
      val a = MLName("a")
      ml.Lambda(ml.Param.Named(a))(ml.Variable(a))
  }

  def fieldSymbolType(f: Symbol)(using C: Context): ml.Type = f match {
    case f: symbols.Field => tpeToML(f.param.tpe.getOrElse(C.panic("No type on record field")))
    case _ => C.panic("Record fields are not actually a field")
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

  def fieldSelectorName(f: Symbol)(using C: Context): MLName = f match {
    case f: symbols.Field =>
      dataSelectorName(f.constructor, f)
    case _ => C.panic("Record fields are not actually a field")
  }

  def dataSelectorName(data: Symbol, selection: Symbol)(using C: Context): MLName = {
    val dataName = name(data)
    val selectionName = name(selection)
    MLName(dataName.name + selectionName.name)
  }

  def freshName(s: String): MLName =
    name(symbols.Name.local(s + Symbol.fresh.next()))

}
