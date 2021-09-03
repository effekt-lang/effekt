package effekt
package namer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Context, ContextOps }
import effekt.context.assertions.SymbolAssertions
import effekt.source.{ Def, Id, IdDef, IdRef, ModuleDecl, Named, Tree }
import effekt.symbols._
import scopes._

/**
 * The output of this phase: a mapping from source identifier to symbol
 *
 * It contains both, TermSymbols and TypeSymbols
 *
 * There is an important distinction between:
 *   - resolving: that is looking up symbols (might include storing the result into the symbolTable)
 *   - binding: that is adding a binding to the environment (lexical.Scope)
 *
 */
class Namer extends Phase[ModuleDecl, ModuleDecl] {

  val phaseName = "namer"

  def run(mod: ModuleDecl)(implicit C: Context): Option[ModuleDecl] = {
    Some(resolve(mod))
  }

  def resolve(decl: ModuleDecl)(implicit C: Context): ModuleDecl = {
    var scope: Scope = toplevel(builtins.rootTypes)

    // process all imports, updating the terms and types in scope
    val imports = decl.imports map {
      case im @ source.Import(path) => Context.at(im) {
        val modImport = Context.moduleOf(path)
        scope.defineAll(modImport.terms, modImport.types)
        modImport
      }
    }

    // create new scope for the current module
    scope = scope.enterGlobal

    Context.initNamerstate(scope)

    resolveGeneric(decl)

    Context.module.export(imports, scope.terms.toMap, scope.types.toMap)
    decl
  }

  /**
   * To allow mutually recursive definitions, here we only resolve the declarations,
   * not the bodies of functions.
   */
  def preresolve(d: Def)(implicit C: Context): Unit = Context.focusing(d) {

    case d @ source.ValDef(id, annot, binding) =>
      ()

    case d @ source.VarDef(id, annot, binding) =>
      ()

    case f @ source.FunDef(id, tparams, params, annot, body) =>
      val uniqueId = Context.freshNameFor(id)
      // we create a new scope, since resolving type params introduces them in this scope
      val sym = Context scoped {
        val tps = tparams map resolve
        val ps = params map resolve
        val ret = Context scoped {
          Context.bind(ps)
          annot map resolve
        }
        UserFunction(uniqueId, tps, ps, ret, f)
      }
      Context.define(id, sym)

    case source.InterfaceDef(id, tparams, ops) =>
      val effectName = Name.qualified(id)
      val effectSym = Context scoped {
        val tps = tparams map resolve
        // we do not resolve the effect operations here to allow them to refer to types that are defined
        // later in the file
        Interface(effectName, tps)
      }
      Context.define(id, effectSym)

    case source.DataDef(id, tparams, ctors) =>
      val typ = Context scoped {
        val tps = tparams map resolve
        // we do not resolve the constructors here to allow them to refer to types that are defined
        // later in the file
        DataType(C.nameFor(id), tps)
      }
      Context.define(id, typ)

    case source.ExternType(id, tparams) =>
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        BuiltinType(C.nameFor(id), tps)
      })

    //    case source.ExternEffect(id, tparams) =>
    //      Context.define(id, Context scoped {
    //        val tps = tparams map resolve
    //        BuiltinEffect(Name(id), tps)
    //      })

    case source.ExternFun(pure, id, tparams, params, ret, body) => {
      val name = Context.freshNameFor(id)
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        val ps: Params = params map resolve
        val tpe = resolve(ret)
        BuiltinFunction(name, tps, ps, Some(tpe), pure, body)
      })
    }

    case d @ source.ExternInclude(path) =>
      d.contents = Context.contentsOf(path).getOrElse {
        C.abort(s"Missing include: ${path}")
      }
      ()
  }

  /**
   * An effectful traversal with two side effects:
   * 1) the passed environment is enriched with definitions
   * 2) names are resolved using the environment and written to the table
   */
  def resolveGeneric(tree: Tree)(implicit C: Context): Unit = Context.focusing(tree) {

    // (1) === Binding Occurrences ===
    case source.ModuleDecl(path, imports, decls) =>
      decls foreach { preresolve }
      resolveAll(decls)

    case source.DefStmt(d, rest) =>
      // resolve declarations but do not resolve bodies
      preresolve(d)
      // resolve bodies
      resolveGeneric(d)
      resolveGeneric(rest)

    case source.ValueParam(id, tpe) =>
      Context.define(id, ValueParam(Name.local(id), tpe.map(resolve)))

    case source.BlockParam(id, tpe) =>
      val p = BlockParam(Name.local(id), resolve(tpe))
      Context.define(id, p)

    case d @ source.ValDef(id, annot, binding) =>
      val tpe = annot.map(resolve)
      resolveGeneric(binding)
      Context.define(id, ValBinder(Name.local(id), tpe, d))

    case d @ source.VarDef(id, annot, binding) =>
      val tpe = annot.map(resolve)
      resolveGeneric(binding)
      Context.define(id, VarBinder(Name.local(id), tpe, d))

    // FunDef, EffDef, and DataDef have already been resolved as part of the module declaration
    case f @ source.FunDef(id, tparams, params, ret, body) =>
      val sym = Context.symbolOf(f)
      Context scoped {
        sym.tparams.foreach { p =>
          Context.bind(p)
        }
        Context.bind(sym.params)
        resolveGeneric(body)
      }

    case source.InterfaceDef(id, tparams, ops) =>
      val effectSym = Context.resolveType(id).asUserEffect
      effectSym.ops = ops.map {
        case source.Operation(id, tparams, params, ret) =>
          // effect operations are always selected, no functions are generated, so we use a local name here
          val name = Name.local(id)
          Context scoped {
            // the parameters of the effect are in scope
            effectSym.tparams.foreach { p => Context.bind(p) }

            val tps = tparams map resolve

            // The type parameters of an effect op are:
            //   1) all type parameters on the effect, followed by
            //   2) the annotated type parameters on the concrete operation
            val op = Operation(name, effectSym.tparams ++ tps, params map resolve, resolve(ret), effectSym)
            Context.define(id, op)
            op
          }
      }

    // The type itself has already been resolved, now resolve constructors
    case d @ source.DataDef(id, tparams, ctors) =>
      val typ = d.symbol
      typ.variants = ctors map {
        case source.Constructor(id, ps) =>
          val name = Context.freshNameFor(id)
          val ctorRet = if (typ.tparams.isEmpty) typ else TypeApp(typ, typ.tparams)
          val record = Record(name, typ.tparams, ctorRet)
          // define constructor
          Context.define(id, record: TermSymbol)
          // define record type
          Context.define(id, record: TypeSymbol)

          // now also resolve fields
          record.fields = resolveFields(ps, record)
          record
      }

    case source.ExternType(id, tparams) => ()
    // case source.ExternEffect(id, tparams) => ()
    case source.ExternFun(pure, id, tparams, params, ret, body) => ()
    case source.ExternInclude(path) => ()

    case source.If(cond, thn, els) =>
      resolveGeneric(cond);
      Context scoped { resolveGeneric(thn) }
      Context scoped { resolveGeneric(els) }

    case source.While(cond, block) =>
      resolveGeneric(cond);
      Context scoped { resolveGeneric(block) }

    case source.BlockStmt(block) =>
      Context scoped { resolveGeneric(block) }

    case source.TryHandle(body, handlers) =>

      val caps = handlers map {
        case source.Handler(cap, clauses) => Context scoped {
          val param = resolve(cap)
          Context.bind(param)
          param
        }
      }
      Context scoped {
        caps foreach { Context.bind }
        resolveGeneric(body)
      }
    //
    //      def extractUserEffect(e: Effect): UserEffect = e match {
    //        case EffectApp(e, args) => extractUserEffect(e)
    //        case e: UserEffect      => e
    //        case b: BuiltinEffect =>
    //          Context.abort(s"Cannot handle built in effects like ${b}")
    //      }
    //
    //      val eff: UserEffect = Context.at(effect) { extractUserEffect(resolve(effect)) }
    //
    //      clauses.foreach {
    //        case source.OpClause(op, params, body, resumeId) =>
    //
    //          // try to find the operation in the handled effect:
    //          eff.ops.find { o => o.name.toString == op.name } map { opSym =>
    //            Context.assignSymbol(op, opSym)
    //          } getOrElse {
    //            Context.abort(s"Effect operation ${op.name} is not part of effect ${eff}.")
    //          }
    //
    //          val ps = params.map(resolve)
    //          Context scoped {
    //            Context.bind(ps)
    //            Context.define(resumeId, ResumeParam(C.module))
    //            resolveGeneric(body)
    //          }
    //      }

    case source.MatchClause(pattern, body) =>
      val ps = resolve(pattern)
      Context scoped {
        ps.foreach { Context.bind }
        resolveGeneric(body)
      }

    case source.BlockArg(params, stmt) =>
      val ps = params.map(resolve)
      Context scoped {
        Context.bind(ps)
        resolveGeneric(stmt)
      }

    // (2) === Bound Occurrences ===

    case source.Call(target, targs, args) =>
      resolveGeneric(target)
      targs foreach resolve
      resolveAll(args)

    // do not try to attempt resolving the selector!
    case source.Select(target, selector) =>
      resolveGeneric(target)

    case source.Var(id)           => Context.resolveVar(id)

    case tpe: source.ValueType    => resolve(tpe)
    case tpe: source.FunctionType => resolve(tpe)

    // THIS COULD ALSO BE A TYPE!
    case id: Id                   => Context.resolveTerm(id)

    case other                    => resolveAll(other)
  }

  def resolveAll(obj: Any)(implicit C: Context): Unit = obj match {
    case p: Product => p.productIterator.foreach {
      case t: Tree => resolveGeneric(t)
      case other   => resolveAll(other)
    }
    case t: Iterable[t] => t.foreach { t => resolveAll(t) }
    case leaf           => ()
  }

  /**
   * Resolve Parameters as part of resolving function signatures
   *
   * Since we annotate functions and effect declarations with resolved types, we need to
   * resolve the parameters.
   *
   * Importantly, resolving them will *not* add the parameters as binding occurence in the current scope.
   * This is done separately by means of `bind`
   */
  def resolve(params: source.ParamSection)(implicit C: Context): List[Param] = Context.focusing(params) {
    case ps: source.ValueParams => resolve(ps)
    case b: source.BlockParam   => List(resolve(b))
  }
  def resolve(p: source.BlockParam)(implicit C: Context): BlockParam = Context.focusing(p) {
    case source.BlockParam(id, tpe) =>
      val sym = BlockParam(Name.local(id), resolve(tpe))
      Context.assignSymbol(id, sym)
      sym
  }

  def resolve(ps: source.ValueParams)(implicit C: Context): List[ValueParam] =
    ps.params map { p =>
      val sym = ValueParam(Name.local(p.id), p.tpe.map(resolve))
      Context.assignSymbol(p.id, sym)
      sym
    }

  def resolveFields(params: source.ValueParams, record: Record)(implicit C: Context): List[Field] = Context.focusing(params) {
    case ps @ source.ValueParams(params) =>

      val paramSyms = Context scoped {
        // Bind the type parameters
        record.tparams.foreach { t => Context.bind(t) }
        resolve(ps)
      }

      (paramSyms zip params) map {
        case (paramSym, paramTree) =>
          val fieldId = paramTree.id.clone
          val name = Context.freshLocalName(fieldId)
          val fieldSym = Field(name, paramSym, record)
          Context.define(fieldId, fieldSym)
          fieldSym
      }
  }

  /**
   * Resolve pattern matching
   *
   * Returns the value params it binds
   */
  def resolve(p: source.MatchPattern)(implicit C: Context): List[ValueParam] = p match {
    case source.IgnorePattern()     => Nil
    case source.LiteralPattern(lit) => Nil
    case source.AnyPattern(id) =>
      val p = ValueParam(Name.local(id), None)
      Context.assignSymbol(id, p)
      List(p)
    case source.TagPattern(id, patterns) =>
      Context.resolveTerm(id)
      patterns.flatMap { resolve }
  }

  /**
   * Resolve Types
   *
   * resolving a type means reconstructing the composite type (e.g. Effectful, ...) from
   * symbols, instead of trees.
   */
  def resolve(tpe: source.ValueType)(implicit C: Context): ValueType = Context.at(tpe) {
    val res = tpe match {
      case source.TypeApp(id, args) =>
        val data = Context.resolveType(id).asValueType
        TypeApp(data, args.map(resolve))
      case source.TypeVar(id) =>
        Context.resolveType(id).asValueType
      case source.ValueTypeTree(tpe) =>
        tpe
      case source.BoxedType(tpe) =>
        BoxedType(resolve(tpe))
    }
    C.annotateResolvedType(tpe)(res.asInstanceOf[tpe.resolved])
    // check that we resolved to a well-kinded type
    kinds.wellformed(res)
    res
  }

  def resolve(tpe: source.BlockType)(implicit C: Context): BlockType = tpe match {
    case b: source.FunctionType  => resolve(b)
    case c: source.InterfaceType => resolve(c)
  }

  def resolve(tpe: source.FunctionType)(implicit C: Context): FunctionType = {
    val res = FunctionType(Nil, List(tpe.params.map(resolve)), resolve(tpe.ret))
    C.annotateResolvedType(tpe)(res)
    res
  }

  def resolve(tpe: source.InterfaceType)(implicit C: Context): InterfaceType = {
    val res = Context.resolveType(tpe.id).asInterfaceType
    C.annotateResolvedType(tpe)(res)
    res
  }

  //  def resolve(eff: source.Effect)(implicit C: Context): Effect = Context.at(eff) {
  //    val res = eff match {
  //      case source.Effect(e, Nil)  => Context.resolveType(e).asInterfaceType
  //      case source.Effect(e, args) => EffectApp(Context.resolveType(e).asInterfaceType, args.map(resolve))
  //    }
  //    kinds.wellformed(res)
  //    res
  //  }

  //  def resolve(tpe: source.Effects)(implicit C: Context): Effects =
  //    Effects(tpe.effs.map(resolve).toSeq: _*) // TODO this otherwise is calling the wrong apply

  /**
   * Resolves type variables, term vars are resolved as part of resolve(tree: Tree)
   */
  def resolve(id: Id)(implicit C: Context): TypeVar = {
    val sym = TypeVar(Name.local(id))
    Context.define(id, sym)
    sym
  }
}

/**
 * Environment Utils -- we use a mutable cell to express adding definitions more easily
 */
trait NamerOps extends ContextOps { Context: Context =>

  /**
   * The state of the namer phase
   */
  private var scope: Scope = scopes.EmptyScope()

  private[namer] def initNamerstate(s: Scope): Unit = scope = s

  /**
   * Override the dynamically scoped `in` to also reset namer state
   */
  override def in[T](block: => T): T = {
    val before = scope
    val result = super.in(block)
    scope = before
    result
  }

  private[namer] def nameFor(id: Id): Name = nameFor(id.name)

  private[namer] def nameFor(id: String): Name = {
    if (scope.isGlobal) Name.qualified(id, module)
    else LocalName(id)
  }

  // TODO we only want to add a seed to a name under the following conditions:
  // - there is already another instance of that name in the same
  //   namespace.
  // - if it is not already fully qualified
  private[namer] def freshLocalName(id: Id): LocalName = LocalName(freshTermName(id))

  private[namer] def freshNameFor(id: Id): Name = nameFor(freshTermName(id))

  private[namer] def freshTermName(id: Id): String = {
    val alreadyBound = scope.currentTermsFor(id.name).size
    val seed = if (alreadyBound > 0) "$" + alreadyBound else ""
    id.name + seed
  }

  // Name Binding and Resolution
  // ===========================
  private[namer] def define(id: Id, s: TermSymbol): Unit = {
    assignSymbol(id, s)
    scope.define(id.name, s)
  }

  private[namer] def define(id: Id, s: TypeSymbol): Unit = {
    assignSymbol(id, s)
    scope.define(id.name, s)
  }

  private[namer] def bind(s: TermSymbol): Unit = scope.define(s.name.name, s)

  private[namer] def bind(s: TypeSymbol): Unit = scope.define(s.name.name, s)

  private[namer] def bind(params: List[List[Param]]): Context = {
    params.flatten.foreach { p => bind(p) }
    this
  }

  /**
   * Tries to find a _unique_ term symbol in the current scope under name id.
   * Stores a binding in the symbol table
   */
  private[namer] def resolveTerm(id: Id): TermSymbol = at(id) {
    val sym = scope.lookupFirstTerm(id.name)
    assignSymbol(id, sym)
    sym
  }

  private[namer] def resolveAny(id: Id): Symbol = at(id) {
    val sym = scope.lookupFirst(id.name)
    assignSymbol(id, sym)
    sym
  }

  /**
   * Variables have to be resolved uniquely
   */
  private[namer] def resolveVar(id: Id): TermSymbol = resolveTerm(id)

  private[namer] def resolveType(id: Id): TypeSymbol = at(id) {
    val sym = scope.lookupType(id.name)
    assignSymbol(id, sym)
    sym
  }

  private[namer] def scopedGlobally[R](block: => R): R = Context in {
    scope = scope.enterGlobal
    block
  }
  private[namer] def scoped[R](block: => R): R = Context in {
    scope = scope.enterLocal
    block
  }
}
