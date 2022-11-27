package effekt
package generator
package ml

import effekt.context.Context
import effekt.lifted.*
import effekt.symbols.{Module, Symbol, TermSymbol, TypeConstructor, TypeSymbol, ValueType, Wildcard}
import effekt.util.paths.*
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

/**
 * Lifted variant of Chez Scheme. Mostly copy-and-paste from [[ChezScheme]].
 *
 * Difficult to share the code, since core and lifted are different IRs.
 */
object ML extends Backend {

  //  // TODO we use the $then variant, for now, since the `then` variant is a macro and would
  //  // require adding it to the syntax ml.Tree
  //  def bind(binding: ml.Expr, param: MLName, body: ml.Block): ml.Expr =
  //    Builtin("$then", binding, ml.Lambda(List(param), body))

  def runMain(main: MLName): ml.Expr =
    ml.Call(Consts.run)(ml.Call(main)(Consts.here))

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(using C: Context): Option[Compiled] = {
    val mainSymbol = C.checkMain(main.mod)
    val deps = dependencies.flatMap { dep => compile(dep) }

    LiftInference(main).map { lifted =>
      val mlModule = compilationUnit(mainSymbol, lifted.mod, lifted.core, deps)
      val result = ml.PrettyPrinter.pretty(ml.PrettyPrinter.toDoc(mlModule), 100)
      val mainFile = path(main.mod)
      Compiled(mainFile, Map(mainFile -> result))
    }
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(using C: Context): Option[Document] =
    C.using(module = input.mod) {
      Some(ml.PrettyPrinter.format(ml.PrettyPrinter.toDoc(compile(input))))
    }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(using Context): List[ml.Binding] =
    LiftInference(in).toList.flatMap { lifted => toML(lifted.core) }

  def compilationUnit(mainSymbol: Symbol, mod: Module, core: ModuleDecl, dependencies: List[ml.Binding])(implicit C: Context): ml.Toplevel = {
    val defs = toML(core)
    ml.Toplevel(dependencies ++ defs, runMain(name(mainSymbol)))
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

  def tpeToML(tpe: symbols.ValueType, inst: ml.Type => ml.Type = x => x)(using C: Context): ml.Type = tpe match {
    case ValueType.BoxedType(_, _) => C.abort("Boxed type is not supported")
    case ValueType.ValueTypeRef(tvar) =>
      val tv = ml.Type.Var(name(tvar))
      inst(tv)
    case ValueType.ValueTypeApp(tc, Nil) => tcToML(tc)
    case ValueType.ValueTypeApp(TypeConstructor.Record(_, tparams, constructor), args) =>
      val argTypes = args.map(tpeToML(_, inst))
      val instMap = tparams.map(t => ml.Type.Var(name(t))).zip(argTypes).toMap
      val instF = (tpe: ml.Type) => inst(if (instMap.contains(tpe)) instMap(tpe) else tpe)
      val fieldTypes = constructor.fields.map { f =>
        (name(f), tpeToML(f.param.tpe.getOrElse(C.panic(s"Record constructor is missing types")), instF))
      }
      ml.Type.Record(fieldTypes)
    case ValueType.ValueTypeApp(tc, args) =>
      ml.Type.Tapp(tcToML(tc), args.map(tpeToML(_, inst)))
  }

  def tcToML(tc: symbols.TypeConstructor)(using C: Context): ml.Type = tc match {
    case symbols.builtins.IntSymbol => ml.Type.Integer
    case symbols.builtins.DoubleSymbol => ml.Type.Real
    case symbols.builtins.BooleanSymbol => ml.Type.Bool
    case symbols.builtins.StringSymbol => ml.Type.String
    case TypeConstructor.DataType(_, _, _) =>
      ml.Type.Data(name(tc))
    case TypeConstructor.Record(_, _, constructor) =>
      val fieldTypes = constructor.fields.map { f =>
        (name(f), tpeToML(f.param.tpe.getOrElse(C.panic("Record constructor is missing types"))))
      }
      ml.Type.Record(fieldTypes)
    case TypeConstructor.ExternType(_, _) =>
      ml.Type.Builtin(name(tc))
  }

  def toML(decl: Decl)(using C: Context): List[ml.Binding] = decl match {
    case Data(did: TypeConstructor.DataType, ctors) =>
      def constructorToML(s: Symbol): (MLName, Option[ml.Type]) = s match {
        case symbols.Constructor(_, _, fields, _) =>
          val tpeList = fields.map(f =>
            tpeToML(f.param.tpe.getOrElse(C.panic("Data constructor is missing types")))
          )
          val tpe = tpeList match {
            case Nil => None
            case one :: Nil => Some(one)
            case _ => Some(ml.Type.Tuple(tpeList))
          }
          (name(s), tpe)
        case _ => C.panic("Constructor's symbol is not a constructor symbol")
      }

      val tvars: List[ml.Type.Var] = did.tparams.map(p => ml.Type.Var(name(p)))
      List(ml.Binding.DataBind(name(did), tvars, ctors map constructorToML))

    case Decl.Record(id: TypeConstructor.Record, fields) => // use the native constructor
      val tparams: List[ml.Type.Var] = id.tparams.map(p => ml.Type.Var(name(p)))
      val fieldTypes = fields.map {
        case f: symbols.Field => (name(f), tpeToML(f.param.tpe.getOrElse(C.panic("No type on record field"))))
        case _ => C.panic("Record fields are not actually a field")
      }
      val tpe = ml.Type.Record(fieldTypes)
      val binding = ml.Binding.TypeBind(name(id), tparams, tpe)
      List(binding)
    case Interface(_, _) => Nil // use native record type
    case Data(_, _) => C.panic("Data symbol is not TypeConstructor.DataType")
    case Decl.Record(_, _) => C.panic("Record symbol is not TypeConstructor.Record")
  }

  def toML(ext: Extern): ml.Binding = ext match {
    case Extern.Def(id, _, params, body) =>
      ml.FunBind(name(id), params map (p => MLName(p.id.name.name)), RawExpr(body))
    case Extern.Include(contents) =>
      RawBind(contents)
  }

  def toMLExpr(stmt: Stmt)(using C: Context): ml.Expr = stmt match {
    case Return(e) => Call(Consts.pure)(toML(e))
    case App(b, _, args) => Expr.Call(toML(b), args map toML)
    case If(cond, thn, els) => ml.If(toML(cond), toMLExpr(thn), toMLExpr(els))
    case Val(id, tpe, binding, body) =>
      Call(Consts.bind)(toMLExpr(binding), ml.Lambda(name(id))(toMLExpr(body)))
    case Match(scrutinee, clauses, default) =>
      def clauseToML(c: (symbols.Constructor, BlockLit)): ml.MatchClause = {
        val (constructor, b) = c
        val binders = b.params.map(p => name(p.id))
        val pattern = constructor.tpe match {
          case symbols.Record(_, _, _) =>
            val fieldNames = constructor.fields map name
            assert(fieldNames.length == binders.length)
            ml.Pattern.Record(fieldNames zip binders)
          case symbols.DataType(_, _, _) =>
            val tag = name(constructor)
            ml.Pattern.Datatype(tag, binders)
          case _: symbols.ExternType => C.abort("Match on ExternType is not supported")
        }
        val body = toMLExpr(b.body)
        ml.MatchClause(pattern, body)
      }

      ml.Match(toML(scrutinee), clauses map clauseToML, default map toMLExpr)

    case Hole => ml.Expr.RawExpr("raise Hole")

    case Scope(definitions, body) => ml.Expr.Let(definitions.map(toML), toMLExpr(body))

    case State(id, init, region, body) if region == symbols.builtins.globalRegion => C.abort("State is not supported")
    //      ml.Let(List(Binding(nameDef(id), ml.Builtin("box", toML(init)))), toML(body))

    case State(id, init, region, body) => C.abort("State is not supported")
    //      ml.Let(List(Binding(nameDef(id), ml.Builtin("fresh", Variable(nameRef(region)), toML(init)))), toML(body))

    case Try(body, _, handler) =>
      val handlers: List[ml.Expr.MakeRecord] = handler.map { (h: Implementation) =>
        val fields = h.operations.map { case Operation(op, BlockLit(params, body)) =>
          val args = params.init.map(p => name(p.id))
          val resumeName = name(params.last.id)
          val evName = freshName("ev_tmp")
          val ev1Name = freshName("ev1_tmp")
          val vName = freshName("vName")
          val kName = freshName("k_tmp")
          val newBody = ml.Call(
            ml.Expr.Variable(evName)
          )(
            ml.Lambda(
              kName
            )(
              ml.Expr.Let(
                List(ml.Binding.FunBind(
                  resumeName,
                  List(ev1Name, vName),
                  ml.Call(ev1Name)(ml.Call(kName)(ml.Expr.Variable(vName)))
                )),
                toMLExpr(body)
              )
            )
          )
          (name(op), ml.Expr.Lambda(evName :: args, newBody))
        }
        ml.Expr.MakeRecord(fields)
      }
      val args = ml.Consts.lift :: handlers
      val tr = ml.Call(toML(body))(args: _*)
      ml.Call(ml.Consts.reset)(tr)

    case Region(body, answerType) => C.abort("Region is not supported")
    //      ml.Builtin("with-region")(toML(body))

//    case Val(Wildcard(), _, binding, body) =>
//      val mlBinding = toMLExpr(binding)
//      toMLExpr(body) match {
//        case ml.Sequence(exps, rest) => ml.Sequence(mlBinding :: exps, rest)
//        case mlbody => ml.Sequence(List(mlBinding), mlbody)
//      }
//
//    case Val(id, _, binding, body) =>
//      val mlBinding = createBinder(id, binding)
//      toMLExpr(body) match {
//        case ml.Let(bindings, body) => ml.Let(mlBinding :: bindings, body)
//        case mlbody => ml.Let(List(mlBinding), mlbody)
//      }

    //    case Def(id, _, block, rest) =>
    //      val constDef = createBinder(id, block)
    //      toMLExpr(rest) match {
    //        case ml.Let(bindings, body) => ml.Let(constDef :: bindings, body)
    //        case mlbody => ml.Let(List(constDef), mlbody)
    //      }
  }

  def createBinder(id: Symbol, binding: Expr)(using Context): Binding = {
    ml.ValBind(name(id), toML(binding))
  }

  def createBinder(id: Symbol, binding: Block)(using Context): Binding = {
    binding match {
      case BlockLit(params, body) =>
        ml.FunBind(name(id), params map toML, toMLExpr(body))
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
      ml.Lambda(params map toML: _*)(toMLExpr(body))
  }

  def toML(block: Block)(using C: Context): ml.Expr = block match {
    case BlockVar(id) =>
      Variable(name(id))

    case b@BlockLit(_, _) =>
      toML(b)

    case Member(b, field) =>
      ml.FieldLookup(toML(b), name(field))

    //    case Extern(params, body) =>
    //      ml.Lambda(params map { p => MLName(p.id.name.name) }: _*)(ml.RawExpr(body))

    case Unbox(e) =>
      toML(e)

    case New(Implementation(id, operations)) => C.abort("New is not supported")
    //      val MLName(name) = nameRef(id)
    //      ml.Call(Variable(MLName(s"make-${name}")), clauses.map { case (_, block) => toML(block) })
  }

  def toML(scope: Evidence): ml.Expr = scope match {
    case Evidence(Nil) => Consts.here
    case Evidence(ev :: Nil) => Variable(name(ev))
    case Evidence(scopes) => ml.Call(Consts.nested)(scopes map { s => ml.Variable(name(s)) }: _*)
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
    case ValueVar(id) => ml.Variable(name(id))

    case PureApp(b, _, args) =>
      val mlArgs = args map {
        case e: Expr => toML(e)
        case b: Block => toML(b)
        case e: Evidence => toML(e)
      }
      b match {
        case BlockVar(id@symbols.Constructor(_, _, _, symbols.TypeConstructor.DataType(_, _, _))) =>
          if (mlArgs.isEmpty) ml.MakeDatatype(name(id), None)
          else ml.MakeDatatype(name(id), Some(ml.Expr.Tuple(mlArgs)))
        case BlockVar(symbols.Constructor(_, _, _, symbols.TypeConstructor.Record(_, _, constr))) =>
          val fieldNames = constr.fields map name
          assert(fieldNames.length == mlArgs.length)
          ml.Expr.MakeRecord(fieldNames zip mlArgs)
        case _ => ml.Call(toML(b), mlArgs)
      }

    case Select(b, field) =>
      ml.FieldLookup(toML(b), name(field))

    case Run(s, _) => Call(Consts.run)(toMLExpr(s))

    case Box(b) => C.abort("Box not supported")
  }

  def freshName(s: String): MLName =
    name(symbols.Name.local("ev_tmp" + Symbol.fresh.next()))

}
