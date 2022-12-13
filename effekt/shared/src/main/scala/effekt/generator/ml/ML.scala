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

  def toMLExpr(stmt: Stmt)(using C: Context): ml.Expr = stmt match {
    case Return(e) => Call(Consts.pure)(toML(e))
    case App(b, _, args) => Expr.Call(toML(b), args map toML)
    case If(cond, thn, els) => ml.If(toML(cond), toMLExpr(thn), toMLExpr(els))
    case Val(id, _, binding, body) =>
      Call(Consts.bind)(toMLExpr(binding), ml.Lambda(ml.Param.Named(name(id)))(toMLExpr(body)))
    case Match(scrutinee, clauses, default) =>
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
        val body = toMLExpr(b.body)
        ml.MatchClause(pattern, body)
      }

      ml.Match(toML(scrutinee), clauses map clauseToML, default map toMLExpr)

    case Hole => ml.Expr.RawExpr("raise Hole")

    case Scope(definitions, body) => ml.mkLet(definitions.map(toML), toMLExpr(body))

    case State(id, init, region, body) if region == symbols.builtins.globalRegion =>
      val bind = ml.Binding.ValBind(name(id), ml.Expr.Ref(toML(init)))
      ml.mkLet(List(bind), toMLExpr(body))

    case State(id, init, region, body) =>
      val bind = ml.Binding.ValBind(name(id), ml.Call(ml.Consts.fresh)(ml.Variable(name(region)), toML(init)))
      ml.mkLet(List(bind), toMLExpr(body))

    case Try(body, _, handler) =>
      val handlers: List[ml.Expr.MakeDatatype] = handler.map { (impl: Implementation) =>
        val fields = impl.operations.map { case Operation(op, BlockLit(params, body)) =>
          val args = params.init.map(paramToML(_))
          val resumeName = name(params.last.id)
          val evName = freshName("ev_tmp")
          val ev1Name = freshName("ev1_tmp")
          val vName = freshName("vName")
          val kName = freshName("k_tmp")
          val newBody = ml.Call(
            ml.Expr.Variable(evName)
          )(
            ml.Lambda(
              ml.Param.Named(kName)
            )(
              ml.mkLet(
                List(ml.Binding.FunBind(
                  resumeName,
                  List(ml.Param.Named(ev1Name), ml.Param.Named(vName)),
                  ml.Call(ev1Name)(ml.Call(kName)(ml.Expr.Variable(vName)))
                )),
                toMLExpr(body)
              )
            )
          )
          ml.Expr.Lambda(ml.Param.Named(evName) :: args, newBody)
        }
        ml.Expr.MakeDatatype(name(impl.id), expsToTupleIsh(fields))
      }
      val args = ml.Consts.lift :: handlers
      val tr = ml.Call(toML(body))(args: _*)
      ml.Call(ml.Consts.reset)(tr)

    case Region(body, _) =>
      ml.Call(ml.Consts.withRegion)(toML(body))

  }

  def createBinder(id: Symbol, binding: Expr)(using Context): Binding = {
    ml.ValBind(name(id), toML(binding))
  }

  def createBinder(id: Symbol, binding: Block)(using Context): Binding = {
    binding match {
      case BlockLit(params, body) =>
        ml.FunBind(name(id), params.map(p => ml.Param.Named(toML(p))), toMLExpr(body))
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
      ml.Lambda(params map (paramToML(_)), toMLExpr(body))
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

    case Run(s, _) => Call(Consts.run)(toMLExpr(s))

    case Box(b) => toML(b) // not sound
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
