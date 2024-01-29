package effekt
package namer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.context.assertions.*
import effekt.typer.Substitutions
import effekt.source.{ Def, Id, IdDef, IdRef, MatchGuard, ModuleDecl, Tree }
import effekt.symbols.*
import effekt.util.messages.ErrorMessageReifier
import scopes.*

/**
 * The output of this phase: a mapping from source identifier to symbol
 *
 * It contains both, TermSymbols and TypeSymbols
 *
 * There is an important distinction between:
 *   - resolving: that is looking up symbols (might include storing the result into the symbolTable)
 *   - binding: that is adding a binding to the environment (lexical.Scope)
 *
 *
 * TODO we could split resolution in three phases
 * 1. only look at the declaration head of definitions in one scope
 * 2. look at the bodies of effect declarations and type definitions
 * 3. look into the bodies of functions
 */
object Namer extends Phase[Parsed, NameResolved] {

  val phaseName = "namer"

  def run(input: Parsed)(using Context): Option[NameResolved] = {
    val Parsed(source, tree) = input
    val mod = Module(tree, source)
    Context.using(module = mod, focus = tree) { resolve(tree) }
    Some(NameResolved(source, tree, mod))
  }

  def resolve(decl: ModuleDecl)(using Context): ModuleDecl = {
    var scope: Scope = toplevel(builtins.rootTerms, builtins.rootTypes, builtins.rootCaptures)

    def processDependency(path: String) =
      val modImport = Context.moduleOf(path)
      scope.defineAll(modImport.terms, modImport.types, modImport.captures)
      modImport

    // process the prelude (but don't if we are processing the prelude already)
    val preludes = Context.config.prelude()
    val isPrelude = preludes.contains(decl.path)

    val processedPreludes = if (!isPrelude) {
      preludes.map(processDependency)
    } else { Nil }

    // process all imports, updating the terms and types in scope
    val imports = decl.imports collect {
      case im @ source.Import(path) =>
        Context.at(im) { processDependency(path) }
    }

    // create new scope for the current module
    scope = scope.enterGlobal

    Context.initNamerstate(scope)

    resolveGeneric(decl)

    // We only want to import each dependency once.
    val allImports = (processedPreludes ++ imports).distinct
    Context.module.exports(allImports, scope.terms.toMap, scope.types.toMap, scope.captures.toMap)
    decl
  }

  /**
   * To allow mutually recursive definitions, here we only resolve the declarations,
   * not the bodies of functions.
   */
  def preresolve(d: Def)(using Context): Unit = Context.focusing(d) {

    case d @ source.ValDef(id, annot, binding) =>
      ()

    case d @ source.VarDef(id, annot, binding) =>
      ()

    case d @ source.RegDef(id, annot, region, binding) =>
      ()

    // allow recursive definitions of objects
    case d @ source.DefDef(id, annot, source.New(source.Implementation(interface, clauses))) =>
      val tpe = Context.at(interface) { resolve(interface) }
      val sym = Binder.DefBinder(Context.nameFor(id), Some(tpe), d)
      Context.define(id, sym)

    case d @ source.DefDef(id, annot, block) =>
      ()

    case f @ source.FunDef(id, tparams, vparams, bparams, annot, body) =>
      val uniqueId = Context.freshNameFor(id)

      // we create a new scope, since resolving type params introduces them in this scope
      val sym = Context scoped {
        val tps = tparams map resolve
        // TODO resolve(ParamSection) loses structural information about value params and block params.
        //   we should refactor!
        val vps = vparams map resolve
        val bps = bparams map resolve
        val ret = Context scoped {
          Context.bindValues(vps)
          Context.bindBlocks(bps)
          annot map resolve
        }
        UserFunction(uniqueId, tps, vps, bps, ret.map { _._1 }, ret.map { _._2 }, f)
      }
      Context.define(id, sym)

    case source.InterfaceDef(id, tparams, ops, isEffect) =>
      val effectName = Context.nameFor(id)
      // we use the localName for effects, since they will be bound as capabilities
      val effectSym = Context scoped {
        val tps = tparams map resolve
        // we do not resolve the effect operations here to allow them to refer to types that are defined
        // later in the file
        Interface(effectName, tps)
      }
      Context.define(id, effectSym)

    case source.TypeDef(id, tparams, tpe) =>
      val tps = Context scoped { tparams map resolve }
      val alias = Context scoped {
        tps.foreach { t => Context.bind(t) }
        TypeAlias(Context.nameFor(id), tps, resolve(tpe))
      }
      Context.define(id, alias)

    case source.EffectDef(id, tparams, effs) =>
      val tps = Context scoped { tparams map resolve }
      val alias = Context scoped {
        tps.foreach { t => Context.bind(t) }
        EffectAlias(Context.nameFor(id), tps, resolve(effs))
      }
      Context.define(id, alias)

    case source.DataDef(id, tparams, ctors) =>
      val typ = Context scoped {
        val tps = tparams map resolve
        // we do not resolve the constructors here to allow them to refer to types that are defined
        // later in the file
        DataType(Context.nameFor(id), tps)
      }
      Context.define(id, typ)

    case source.RecordDef(id, tparams, fields) =>
      lazy val sym: Record = {
        val tps = Context scoped { tparams map resolve }
        // we do not resolve the fields here to allow them to refer to types that are defined
        // later in the file
        Record(Context.nameFor(id), tps, null)
      }
      Context.define(id, sym)

    case source.ExternType(id, tparams) =>
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        ExternType(Context.nameFor(id), tps)
      })

    case source.ExternInterface(id, tparams) =>
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        ExternInterface(Context.nameFor(id), tps)
      })

    case source.ExternDef(capture, id, tparams, vparams, bparams, ret, body) => {
      val name = Context.freshNameFor(id)
      val capt = resolve(capture)
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        val vps = vparams map resolve
        val bps = bparams map resolve

        val (tpe, eff) = Context scoped {
          Context.bindBlocks(bps)
          resolve(ret)
        }
        ExternFunction(name, tps, vps, bps, tpe, eff, capt, body)
      })
    }

    case source.ExternResource(id, tpe) =>
      val name = Context.freshNameFor(id)
      val btpe = resolve(tpe)
      val sym = ExternResource(name, btpe)
      Context.define(id, sym)
      Context.bindBlock(sym)

    case d @ source.ExternInclude(path, Some(contents), _) =>
      ()

    case d @ source.ExternInclude(path, None, _) =>
      d.contents = Some(Context.contentsOf(path).getOrElse {
        Context.abort(s"Missing include: ${path}")
      })
      ()
  }

  /**
   * An effectful traversal with two side effects:
   * 1) the passed environment is enriched with definitions
   * 2) names are resolved using the environment and written to the table
   */
  def resolveGeneric(tree: Tree)(using Context): Unit = Context.focusing(tree) {

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
      Context.bind(p.capture)

    case d @ source.ValDef(id, annot, binding) =>
      val tpe = annot.map(resolve)
      resolveGeneric(binding)
      Context.define(id, ValBinder(Context.nameFor(id), tpe, d))


    // Local mutable state
    case d @ source.VarDef(id, annot, binding) =>
      val tpe = annot.map(resolve)

      resolveGeneric(binding)
      val sym = VarBinder(Context.nameFor(id), tpe, d)
      Context.define(id, sym)

    // allocation into a region
    case d @ source.RegDef(id, annot, region, binding) =>
      val tpe = annot.map(resolve)
      val reg = Context.resolveTerm(region) match {
        case t: BlockSymbol => t
        case _ => Context.abort("Region needs to be a block.")
      }

      resolveGeneric(binding)
      val sym = RegBinder(Context.nameFor(id), tpe, reg, d)

      Context.define(id, sym)

    // already has been preresolved (to enable recursive definitions)
    case d @ source.DefDef(id, annot, source.New(impl)) =>
      resolveGeneric(impl)

    case d @ source.DefDef(id, annot, binding) =>
      val tpe = annot.map(resolve)
      resolveGeneric(binding)
      Context.define(id, DefBinder(Context.nameFor(id), tpe, d))

    // FunDef and InterfaceDef have already been resolved as part of the module declaration
    case f @ source.FunDef(id, tparams, vparams, bparams, ret, body) =>
      val sym = f.symbol
      Context scoped {
        sym.tparams.foreach { p => Context.bind(p) }
        Context.bindValues(sym.vparams)
        Context.bindBlocks(sym.bparams)

        resolveGeneric(body)
      }

    case f @ source.ExternDef(capture, id, tparams, vparams, bparams, ret, body) =>
      val sym = f.symbol
      Context scoped {
        sym.tparams.foreach { p => Context.bind(p) }
        Context.bindValues(sym.vparams)
        Context.bindBlocks(sym.bparams)
        body.args.foreach(resolveGeneric)
      }

    case source.InterfaceDef(id, tparams, operations, isEffect) =>
      val effectSym = Context.resolveType(id).asControlEffect
      effectSym.operations = operations.map {
        case op @ source.Operation(id, tparams, params, ret) => Context.at(op) {
          val name = Context.nameFor(id)

          Context scoped {
            // the parameters of the effect are in scope
            effectSym.tparams.foreach { p => Context.bind(p) }

            val tps = tparams map resolve

            // The type parameters of an effect op are:
            //   1) all type parameters on the effect, followed by
            //   2) the annotated type parameters on the concrete operation
            val (result, effects) = resolve(ret)

            val op = Operation(name, effectSym.tparams ++ tps, params map { p => resolve(p) }, result, effects, effectSym)
            Context.define(id, op)
            op
          }
        }
      }
      if (isEffect) effectSym.operations.foreach { op => Context.bind(op) }

    case source.TypeDef(id, tparams, tpe) => ()
    case source.EffectDef(id, tparams, effs)       => ()

    // The type itself has already been resolved, now resolve constructors
    case d @ source.DataDef(id, tparams, ctors) =>
      val data = d.symbol
      data.constructors = ctors map {
        case source.Constructor(id, ps) =>
          val name = Context.freshNameFor(id)
          val constructor = Constructor(name, data.tparams, null, data)
          Context.define(id, constructor)
          constructor.fields = resolveFields(ps, constructor)
          constructor
      }

    // The record has been resolved as part of the preresolution step
    case d @ source.RecordDef(id, tparams, fs) =>
      val record = d.symbol
      val name = Context.freshNameFor(id)
      val constructor = Constructor(name, record.tparams, null, record)
      // we define the constructor on a copy to avoid confusion with symbols
      Context.define(id.clone, constructor)
      record.constructor = constructor
      constructor.fields = resolveFields(fs, constructor)

    case source.ExternType(id, tparams) => ()
    case source.ExternInterface(id, tparams) => ()
    case source.ExternResource(id, tpe) => ()
    case source.ExternInclude(path, _, _) => ()

    case source.If(guards, thn, els) =>
      Context scoped { guards.foreach(resolve); resolveGeneric(thn) }
      Context scoped { resolveGeneric(els) }

    case source.While(guards, block, default) =>
      Context scoped { Context scoped { guards.foreach(resolve) }; resolveGeneric(block) }
      Context scoped { default foreach resolveGeneric }

    case source.BlockStmt(block) =>
      Context scoped { resolveGeneric(block) }

    case tree @ source.TryHandle(body, handlers) =>
      resolveAll(handlers)

      Context scoped {

        // bind all annotated capabilities
        handlers.foreach { handler =>
          handler.capability.foreach { p =>
            Context.bindBlock(resolve(p))
          }
        }

        resolveGeneric(body)
      }

    case tree @ source.Region(name, body) =>
      val reg = BlockParam(Name.local(name.name), builtins.TRegion)
      Context.define(name, reg)
      Context scoped {
        Context.bindBlock(reg)
        resolveGeneric(body)
      }

    case source.Implementation(interface, clauses) =>
      val eff: Interface = Context.at(interface) { resolve(interface).typeConstructor.asInterface }

      clauses.foreach {
        case clause @ source.OpClause(op, tparams, params, ret, body, resumeId) => Context.at(clause) {

          // try to find the operation in the handled effect:
          eff.operations.find { o => o.name.toString == op.name } map { opSym =>
            Context.assignSymbol(op, opSym)
          } getOrElse {
            Context.abort(pretty"Operation ${op} is not part of interface ${eff}.")
          }
          Context scoped {
            val tps = tparams.map(resolve)
            val vps = params.map(resolve)
            Context.bindValues(vps)
            Context.define(resumeId, ResumeParam(Context.module))
            resolveGeneric(body)
          }
        }
      }

    case source.MatchClause(pattern, guards, body) =>
      val ps = resolve(pattern)
      Context scoped {
        // variables bound by patterns are available in the guards.
        ps.foreach { Context.bind }
        guards.foreach { resolve }

        // wellformedness: only linear patterns
        var names: Set[Name] = Set.empty
        ps foreach { p =>
          if (names contains p.name)
            Context.error(pp"Patterns have to be linear: names can only occur once, but ${p.name} shows up multiple times.")

          val cs = Context.allConstructorsFor(p.name.name)
          if (cs.nonEmpty) {
            Context.warning(pp"Pattern binds variable ${p.name}. Maybe you meant to match on equally named constructor of type ${cs.head.tpe}?")
          }
          names = names + p.name
        }

        Context scoped {
          resolveGeneric(body)
        }
      }

    case f @ source.BlockLiteral(tparams, vparams, bparams, stmt) =>
      Context scoped {
        val tps = tparams map resolve
        val vps = vparams map resolve
        val bps = bparams map resolve

        Context.bindValues(vps)
        Context.bindBlocks(bps)

        resolveGeneric(stmt)
      }

    case source.Box(capt, block) =>
      capt foreach resolve
      resolveGeneric(block)

    // (2) === Bound Occurrences ===

    case source.Select(receiver, target) =>
      resolveGeneric(receiver)
      Context.resolveSelect(target)

    case source.MethodCall(receiver, target, targs, vargs, bargs) =>
      resolveGeneric(receiver)
      Context.resolveMethodCalltarget(target)
      targs foreach resolve
      resolveAll(vargs)
      resolveAll(bargs)

    case source.Do(effect, target, targs, vargs) =>
      Context.resolveEffectCall(effect map resolve, target)
      targs foreach resolve
      resolveAll(vargs)

    case source.Call(target, targs, vargs, bargs) =>
      Context.focusing(target) {
        case source.IdTarget(id)     => Context.resolveFunctionCalltarget(id)
        case source.ExprTarget(expr) => resolveGeneric(expr)
      }
      targs foreach resolve
      resolveAll(vargs)
      resolveAll(bargs)

    case source.Var(id) => Context.resolveVar(id)

    case source.Assign(id, expr) => Context.resolveVar(id) match {
      case _: VarBinder | _: RegBinder => resolveGeneric(expr)
      case _: ValBinder | _: ValueParam => Context.abort(pretty"Can only assign to mutable variables, but ${id.name} is a constant.")
      case y: Wildcard => Context.abort(s"Trying to assign to a wildcard, which is not allowed.")
      case _ => Context.abort(s"Can only assign to mutable variables.")
    }

    case tpe: source.ValueType    => resolve(tpe)
    case tpe: source.FunctionType => resolve(tpe)

    // THIS COULD ALSO BE A TYPE!
    case id: Id                   => Context.resolveTerm(id)

    case other                    => resolveAll(other)
  }

  // TODO move away
  def resolveFields(params: List[source.ValueParam], constructor: Constructor)(using Context): List[Field] = {
    val paramSyms = Context scoped {
      // Bind the type parameters
      constructor.tparams.foreach { t => Context.bind(t) }
      params map resolve
    }

    (paramSyms zip params) map {
      case (paramSym, paramTree) =>
        val fieldId = paramTree.id.clone
        val name = Context.freshNameFor(fieldId)
        val fieldSym = Field(name, paramSym, constructor)
        Context.define(fieldId, fieldSym)
        fieldSym
    }
  }

  def resolveAll(obj: Any)(using Context): Unit = obj match {
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
  def resolve(params: List[source.Param])(using Context): List[Param] =
    params map resolve

  def resolve(param: source.Param)(using Context): Param = param match {
    case p: source.ValueParam => resolve(p)
    case p: source.BlockParam => resolve(p)
  }
  def resolve(p: source.ValueParam)(using Context): ValueParam = {
    val sym = ValueParam(Name.local(p.id), p.tpe.map(resolve))
    Context.assignSymbol(p.id, sym)
    sym
  }
  def resolve(p: source.BlockParam)(using Context): BlockParam = {
    val sym: BlockParam = BlockParam(Name.local(p.id), resolve(p.tpe))
    Context.assignSymbol(p.id, sym)
    sym
  }

  /**
   * Resolve pattern matching
   *
   * Returns the value params it binds
   */
  def resolve(p: source.MatchPattern)(using Context): List[ValueParam] = p match {
    case source.IgnorePattern()     => Nil
    case source.LiteralPattern(lit) => Nil
    case source.AnyPattern(id) =>
      val p = ValueParam(Name.local(id), None)
      Context.assignSymbol(id, p)
      List(p)
    case source.TagPattern(id, patterns) =>
      Context.resolveTerm(id) match {
        case symbol: Constructor => ()
        case _ => Context.at(id) {
          Context.error("Can only pattern match on constructors of data types.")
        }
      }
      patterns.flatMap { resolve }
  }

  def resolve(p: source.MatchGuard)(using Context): Unit = p match {
    case MatchGuard.BooleanGuard(condition) => resolveGeneric(condition)
    case MatchGuard.PatternGuard(scrutinee, pattern) =>
      resolveGeneric(scrutinee)
      val ps = resolve(pattern)
      ps.foreach { Context.bind }
  }

  /**
   * Resolve Types
   *
   * resolving a type means reconstructing the composite type (e.g. Effectful, ...) from
   * symbols, instead of trees.
   *
   * We de-alias on-the-fly in Namer so that aliases can never show up again in the remaining compiler.
   * This way error messages might suffer; however it simplifies the compiler a lot.
   */
  def resolve(tpe: source.ValueType)(using Context): ValueType = resolvingType(tpe) {
    case source.ValueTypeRef(id, args) => Context.resolveType(id) match {
      case constructor: TypeConstructor => ValueTypeApp(constructor, args.map(resolve))
      case id: TypeVar =>
        if (args.nonEmpty) {
          Context.abort(pretty"Type variables cannot be applied, but receieved ${args.size} arguments.")
        }
        ValueTypeRef(id)
      case TypeAlias(name, tparams, tpe) =>
        val targs = args.map(resolve)
        if (tparams.size != targs.size) {
          Context.abort(pretty"Type alias ${name} expects ${tparams.size} type arguments, but got ${targs.size}.")
        }
        Substitutions.types(tparams, targs).substitute(tpe)
      case other => Context.abort(pretty"Expected a value type, but got ${other}")
    }
    case source.ValueTypeTree(tpe) =>
      tpe
    // TODO reconsider reusing the same set for terms and types...
    case source.BoxedType(tpe, capt) =>
      BoxedType(resolve(tpe), resolve(capt))
  }

  def resolve(tpe: source.BlockType)(using Context): BlockType = resolvingType(tpe) {
    case t: source.FunctionType  => resolve(t)
    case t: source.BlockTypeTree => t.eff
    case t: source.BlockTypeRef => resolve(t)
  }

  def resolve(funTpe: source.FunctionType)(using Context): FunctionType = resolvingType(funTpe) {
    /**
     * TODO share code with [[typer.Typer.makeFunctionType]]
     */
    case source.FunctionType(tparams, vparams, bparams, ret, effects) => Context scoped {
      val tps = tparams.map(resolve)
      val vps = vparams.map(resolve)

      var cps: List[Capture] = Nil
      val bps = bparams.map {
        case (id, tpe) =>
          val name = id.map(Name.local).getOrElse(NoName)
          val cap = CaptureParam(name)
          cps = cps :+ cap
          resolve(tpe)
      }

      val effs = resolve(effects).distinct
      effs.canonical.foreach { eff =>
        val cap = CaptureParam(eff.name)
        cps = cps :+ cap
      }

      cps foreach Context.bind

      val res = resolve(ret)

      FunctionType(tps, cps, vps, bps, res, effs)
    }
  }

  def resolve(tpe: source.BlockTypeRef)(using Context): InterfaceType = resolvingType(tpe) { tpe =>
    resolveWithAliases(tpe) match {
      case Nil => Context.abort("Expected a single interface type, not an empty effect set.")
      case resolved :: Nil => resolved
      case _ => Context.abort("Expected a single interface type, arbitrary effect aliases are not allowed.")
    }
  }

  /**
   * Resolves an interface type, potentially with effect aliases on the top level
   */
  def resolveWithAliases(tpe: source.BlockTypeRef)(using Context): List[InterfaceType] = Context.at(tpe) {
    val resolved: List[InterfaceType] = tpe match {
      case source.BlockTypeRef(id, args) => Context.resolveType(id) match {
        case EffectAlias(name, tparams, effs) =>
          if (tparams.size != args.size) {
            Context.abort(pretty"Effect alias ${name} expects ${tparams.size} type arguments, but got ${args.size}.")
          }
          val targs = args.map(resolve)
          val subst = Substitutions.types(tparams, targs)
          effs.toList.map(subst.substitute)
        case i: BlockTypeConstructor => List(InterfaceType(i, args.map(resolve)))
        case _ => Context.abort("Expected an interface type.")
      }
    }
    resolved.foreach(kinds.wellformed)
    resolved
  }

  def resolve(tpe: source.Effects)(using Context): Effects =
    Effects(tpe.effs.flatMap(resolveWithAliases).toSeq: _*) // TODO this otherwise is calling the wrong apply

  def resolve(e: source.Effectful)(using Context): (ValueType, Effects) =
    (resolve(e.tpe), resolve(e.eff))

  def resolve(capt: source.CaptureSet)(using Context): CaptureSet = Context.at(capt) {
    val captResolved = CaptureSet(capt.captures.map { Context.resolveCapture }.toSet)
    Context.annotateResolvedCapture(capt)(captResolved)
    captResolved
  }

  /**
   * Resolves type variables, term vars are resolved as part of resolve(tree: Tree)
   */
  def resolve(id: Id)(using Context): TypeParam = Context.at(id) {
    val sym: TypeParam = TypeParam(Name.local(id))
    Context.define(id, sym)
    sym
  }

  def resolvingType[T <: source.Type, R <: symbols.Type](tpe: T)(f: T => R)(using Context): R = Context.at(tpe) {
    val res = f(tpe)
    Context.annotateResolvedType(tpe)(res)
    kinds.wellformed(res)
    res
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
    else Name.local(id)
  }

  // TODO we only want to add a seed to a name under the following conditions:
  // - there is already another instance of that name in the same
  //   namespace.
  // - if it is not already fully qualified
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

  private[namer] def bind(s: Capture): Unit = bind(s.name.name, s)

  private[namer] def bind(name: String, s: Capture): Unit = scope.define(name, s)

  private[namer] def bind(s: TermSymbol): Unit = scope.define(s.name.name, s)

  private[namer] def bind(s: TypeSymbol): Unit = scope.define(s.name.name, s)

  private[namer] def bindParams(params: List[Param]) =
    params.foreach { p => bind(p) }

  private[namer] def bindValues(params: List[ValueParam]) =
    params.foreach { p => bind(p) }

  private[namer] def bindBlocks(params: List[BlockParam]) =
    // bind the block parameter as a term
    params.foreach { bindBlock }

  private[namer] def bindBlock(p: TrackedParam) = {
    // bind the block parameter as a term
    bind(p)
    bind(p.capture)
  }
  private[namer] def bindBlock(name: String, p: TrackedParam) = {
    scope.define(name, p)
    scope.define(name, p.capture)
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

  private[namer] def allConstructorsFor(name: String): Set[Constructor] =
    scope.allTermsFor(name).collect {
      case c: Constructor => c
    }

  private[namer] def resolveAny(id: Id): Symbol = at(id) {
    val sym = scope.lookupFirst(id.name)
    assignSymbol(id, sym)
    sym
  }

  /**
   * Resolves a potentially overloaded call target
   */
  private[namer] def resolveMethodCalltarget(id: Id): Unit = at(id) {

    val syms = scope.lookupOverloaded(id.name, term => term.isInstanceOf[BlockSymbol])

    if (syms.isEmpty) {
      abort(pretty"Cannot resolve function ${id.name}")
    }

    // TODO does local name make sense here?
    assignSymbol(id, CallTarget(Name.local(id), syms.asInstanceOf))
  }

  /**
   * Resolves a potentially overloaded field access
   */
  private[namer] def resolveFunctionCalltarget(id: Id): Unit = at(id) {
    val candidates = scope.lookupOverloaded(id.name, term => !term.isInstanceOf[Operation])

    resolveFunctionCalltarget(id, candidates) match {
      case Left(value) =>
        assignSymbol(id, value)
      case Right(blocks) =>
        if (blocks.isEmpty) {
          val allSyms = scope.lookupOverloaded(id.name, term => true).flatten

          if (allSyms.exists { case o: Operation => true; case _ => false })
            info(pretty"There is an equally named effect operation. Use syntax `do ${id}() to call it.`")

          if (allSyms.exists { case o: Field => true; case _ => false })
            info(pretty"There is an equally named field. Use syntax `obj.${id} to access it.`")

          abort(pretty"Cannot find a function named `${id}`.")
        }
        assignSymbol(id, CallTarget(Name.local(id), blocks))
    }
  }

  /**
   * This function implements the scoping rules for blocks and values.
   *
   * 1) A single value in the tightest scope shadows blocks.
   *    i.e. { def foo() = ...; { val foo = ...; >>>foo<<< }}
   *    refers to the value foo
   *
   * 2) If the tighest scope contains blocks, then we will ignore all values
   *    and resolve to an overloaded target.
   */
  private def resolveFunctionCalltarget(id: Id, candidates: List[Set[TermSymbol]]): Either[TermSymbol, List[Set[BlockSymbol]]] =

    // Mutable variables are treated as values, not as blocks. Maybe we should change the representation.
    def isValue(t: TermSymbol): Boolean = t.isInstanceOf[ValueSymbol] || t.isInstanceOf[RefBinder]
    def isBlock(t: TermSymbol): Boolean = t.isInstanceOf[BlockSymbol] && !t.isInstanceOf[RefBinder]

    candidates match {
      case Nil => Right(Nil)

      // should not occur by construction
      case terms :: rest if terms.isEmpty => resolveFunctionCalltarget(id, rest)

      case terms :: rest if terms.forall(isBlock) =>
        Right(candidates.map { scope => scope.collect { case b: BlockSymbol => b }}.filterNot(_.isEmpty))

      case terms :: rest if terms.exists(isValue) =>
        if (terms.exists(isBlock)) {
          panic("Should not happen by construction.")
        }
        // it is only a SINGLE value in the current scope => take it. It shadows blocks.
        if (terms.size == 1) {
          Left(terms.head)
        } else {
          abort(pretty"Multiple values with the same name $id in one scope. Values cannot be overloaded.")
        }

      case _ => panic("Should not happen")
    }

  /**
   * Resolves a potentially overloaded field access
   */
  private[namer] def resolveSelect(id: Id): Unit = at(id) {
    val syms = scope.lookupOverloaded(id.name, term => term.isInstanceOf[Field])

    if (syms.isEmpty) {
      abort(pretty"Cannot resolve field access ${id}")
    }

    assignSymbol(id, CallTarget(Name.local(id), syms.asInstanceOf))
  }

  /**
   * Resolves a potentially overloaded call to an effect
   */
  private[namer] def resolveEffectCall(eff: Option[InterfaceType], id: Id): Unit = at(id) {

    val syms = eff match {
      case Some(tpe) =>
        val interface = tpe.typeConstructor.asInterface
        val operations = interface.operations.filter { op => op.name.name == id.name }
        if (operations.isEmpty) Nil else List(operations.toSet)
      case None => scope.lookupEffectOp(id.name)
    }

    if (syms.isEmpty) {
      abort(pretty"Cannot resolve effect operation ${id}")
    }

    assignSymbol(id, CallTarget(Name.local(id), syms.asInstanceOf))
  }

  /**
   * Variables have to be resolved uniquely
   */
  private[namer] def resolveVar(id: Id): TermSymbol = resolveTerm(id) match {
    case b: BlockParam => b // abort("Blocks have to be fully applied and can't be used as values.")
    case other         => other
  }

  private[namer] def resolveType(id: Id): TypeSymbol = at(id) {
    val sym = scope.lookupType(id.name)
    assignSymbol(id, sym)
    sym
  }

  private[namer] def resolveCapture(id: Id): Capture = at(id) {
    val sym = scope.lookupCapture(id.name)
    assignSymbol(id, sym)
    sym
  }

  private[namer] def scoped[R](block: => R): R = Context in {
    scope = scope.enterLocal
    block
  }
}
