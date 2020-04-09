package effekt
package namer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Context }
import effekt.context.assertions.{ SymbolAssertions, TypeAssertions }
import effekt.source.{ Def, Id, Tree }
import effekt.symbols._
import scopes._

import org.bitbucket.inkytonik.kiama.util.Source

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
case class NamerState(
  scope: Scope
)

class Namer extends Phase[Module, Module] { namer =>

  val phaseName = "Namer"

  def run(mod: Module)(implicit C: Context): Option[Module] = {
    Some(resolve(mod))
  }

  def resolve(mod: Module)(implicit C: Context): Module = {

    var scope: Scope = toplevel(builtins.rootTypes)

    // process all imports, updating the terms and types in scope
    mod.decl.imports foreach {
      case im @ source.Import(path) => Context.at(im) {
        val modImport = Context.moduleOf(path)
        scope.defineAll(modImport.terms, modImport.types)
      }
    }

    // create new scope for the current module
    scope = scope.enter

    Context.namerState = NamerState(scope)

    resolveGeneric(mod.decl)

    mod.export(scope.terms.toMap, scope.types.toMap)
  }

  /**
   * An effectful traversal with two side effects:
   * 1) the passed environment is enriched with definitions
   * 2) names are resolved using the environment and written to the table
   */
  def resolveGeneric(tree: Tree)(implicit C: Context): Unit = Context.focusing(tree) {

    // (1) === Binding Occurrences ===
    case source.ModuleDecl(path, imports, decls) =>
      decls foreach { resolve }
      resolveAll(decls)

    case source.DefStmt(d, rest) =>
      // resolve declarations but do not resolve bodies
      resolve(d)
      // resolve bodies
      resolveGeneric(d)
      resolveGeneric(rest)

    case source.ValueParam(id, tpe) =>
      Context.define(id, ValueParam(Name(id), tpe.map(resolve)))

    case source.BlockParam(id, tpe) =>
      Context.define(id, BlockParam(Name(id), resolve(tpe)))

    // FunDef and EffDef have already been resolved as part of the module declaration
    case f @ source.FunDef(id, tparams, params, ret, body) =>
      val sym = Context.symbolOf(f)
      Context scoped {
        sym.tparams.foreach { p =>
          Context.bind(p)
        }
        Context.bind(sym.params)
        resolveGeneric(body)
      }

    case source.EffDef(id, ops) =>
      val effectSym = Context.resolveType(id).asUserEffect
      effectSym.ops = ops.map {
        case source.Operation(id, tparams, params, ret) =>
          val name = Context.freshTermName(id)
          Context scoped {
            val tps = tparams map resolve
            val tpe = Effectful(resolve(ret), Effects(List(effectSym)))
            EffectOp(Name(id), tps, params map resolve, Some(tpe), effectSym)
          }
      }
      effectSym.ops.foreach { op => Context.bind(op) }

    case source.TypeDef(id, tparams, tpe) => ()
    case source.EffectDef(id, effs)       => ()

    // The type itself has already been resolved, now resolve constructors
    case d @ source.DataDef(id, tparams, ctors) =>
      val typ = d.symbol
      typ.variants = ctors map {
        case source.Constructor(id, ps) =>
          val name = Context.freshTermName(id)
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

    // The record has been resolved as part of the preresolution step
    case d @ source.RecordDef(id, tparams, fields) =>
      val record = d.symbol
      record.fields = resolveFields(fields, record)

    case source.ExternType(id, tparams) => ()
    case source.ExternEffect(id, tparams) => ()
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
      Context scoped { resolveGeneric(body) }
      resolveAll(handlers)

    case source.Handler(name, clauses) =>
      val eff = Context.at(name) { Context.resolveType(name) }.asUserEffect

      clauses.foreach {
        case source.OpClause(op, params, body, resumeId) =>

          // try to find the operation in the handled effect:
          eff.ops.filter { o => o.name.toString == op.name }.headOption map { opSym =>
            Context.assignSymbol(op, opSym)
          } getOrElse {
            Context.abort(s"Effect operation ${op.name} is not part of effect ${name.name}.")
          }

          val ps = params.map(resolve)
          Context scoped {
            Context.bind(ps)
            Context.define(resumeId, ResumeParam(C.module))
            resolveGeneric(body)
          }
      }

    case source.MatchClause(pattern, body) =>
      val ps = resolve(pattern)
      Context scoped {
        ps.foreach { Context.bind }
        resolveGeneric(body)
      }

    case source.BlockArg(params, stmt) =>
      val ps = resolve(params)
      Context scoped {
        Context.bind(List(ps))
        resolveGeneric(stmt)
      }

    // (2) === Bound Occurrences ===

    case source.Call(id, targs, args) =>
      Context.resolveCalltarget(id)
      targs foreach resolve
      resolveAll(args)

    case source.Var(id)        => Context.resolveVar(id)

    case tpe: source.ValueType => resolve(tpe)
    case tpe: source.BlockType => resolve(tpe)

    // THIS COULD ALSO BE A TYPE!
    case id: Id                => Context.resolveTerm(id)

    case other                 => resolveAll(other)
  }

  // TODO move away
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
          val name = Context.freshTermName(fieldId)
          val fieldSym = Field(name, paramSym, record)
          Context.define(fieldId, fieldSym)
          fieldSym
      }
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
    case source.BlockParam(id, tpe) =>
      val sym = BlockParam(Name(id), resolve(tpe))
      Context.assignSymbol(id, sym)
      List(sym)
  }
  def resolve(ps: source.ValueParams)(implicit C: Context): List[ValueParam] =
    ps.params map { p =>
      val sym = ValueParam(Name(p.id), p.tpe.map(resolve))
      Context.assignSymbol(p.id, sym)
      sym
    }

  /**
   * To allow mutually recursive definitions, here we only resolve the declarations,
   * not the bodies of functions.
   */
  def resolve(d: Def)(implicit C: Context): Unit = Context.focusing(d) {

    case d @ source.ValDef(id, annot, binding) =>
      val tpe = annot.map(resolve)
      resolveGeneric(binding)
      Context.define(id, ValBinder(Name(id), tpe, d))

    case d @ source.VarDef(id, annot, binding) =>
      val tpe = annot.map(resolve)
      resolveGeneric(binding)
      Context.define(id, VarBinder(Name(id), tpe, d))

    case f @ source.FunDef(id, tparams, params, annot, body) =>
      val uniqueId = Context.freshTermName(id)
      // we create a new scope, since resolving type params introduces them in this scope
      val sym = Context scoped {
        UserFunction(
          uniqueId,
          tparams map resolve,
          params map resolve,
          annot map resolve,
          f
        )
      }
      Context.define(id, sym)

    case source.EffDef(id, ops) =>
      // we use the localName for effects, since they will be bound as capabilities
      val effectSym = UserEffect(Name(id), Nil)
      Context.define(id, effectSym)

    case source.TypeDef(id, tparams, tpe) =>
      val tps = Context scoped { tparams map resolve }
      val alias = Context scoped {
        tps.foreach { t => Context.bind(t) }
        TypeAlias(Name(id), tps, resolve(tpe))
      }
      Context.define(id, alias)

    case source.EffectDef(id, effs) =>
      val alias = Context scoped {
        EffectAlias(Name(id), resolve(effs))
      }
      Context.define(id, alias)

    case source.DataDef(id, tparams, ctors) =>
      val (typ, tps) = Context scoped {
        val tps = tparams map resolve
        // we do not resolve the constructors here to allow them to refer to types that are defined
        // later in the file
        (DataType(Name(id), tps), tps)
      }
      Context.define(id, typ)

    case source.RecordDef(id, tparams, fields) =>
      lazy val sym: Record = {
        val tps = Context scoped { tparams map resolve }
        // we do not resolve the fields here to allow them to refer to types that are defined
        // later in the file
        Record(Name(id), tps, null)
      }
      sym.tpe = if (sym.tparams.isEmpty) sym else TypeApp(sym, sym.tparams)

      // define constructor
      Context.define(id, sym: TermSymbol)
      // define record type
      Context.define(id, sym: TypeSymbol)

    case source.ExternType(id, tparams) =>
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        BuiltinType(Name(id), tps)
      })

    case source.ExternEffect(id, tparams) =>
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        BuiltinEffect(Name(id), tps)
      })

    case source.ExternFun(pure, id, tparams, params, ret, body) => {
      val name = Context.freshTermName(id)
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        val ps: Params = params map resolve
        val tpe = resolve(ret)
        BuiltinFunction(name, tps, ps, Some(tpe), pure, body)
      })
    }

    case d @ source.ExternInclude(path) =>
      d.contents = Context.contentsOf(path)
      ()
  }

  /**
   * Resolve pattern matching
   *
   * Returns the value params it binds
   */
  def resolve(p: source.MatchPattern)(implicit C: Context): List[ValueParam] = p match {
    case source.IgnorePattern() => Nil
    case source.AnyPattern(id) =>
      val p = ValueParam(Name(id), None)
      Context.assignSymbol(id, p)
      List(p)
    case source.TagPattern(id, patterns) =>
      Context.resolveTerm(id)
      patterns.flatMap(p => p.flatMap { resolve })
  }

  /**
   * Resolve Types
   *
   * resolving a type means reconstructing the composite type (e.g. Effectful, ...) from
   * symbols, instead of trees.
   */
  def resolve(tpe: source.ValueType)(implicit C: Context): ValueType = tpe match {
    case source.TypeApp(id, args) =>
      val data = Context.resolveType(id).asValueType
      TypeApp(data, args.map(resolve))
    case source.TypeVar(id) =>
      Context.resolveType(id).asValueType
  }

  def resolve(tpe: source.BlockType)(implicit C: Context): BlockType =
    BlockType(Nil, List(tpe.params.map(resolve)), resolve(tpe.ret))

  def resolve(tpe: source.Effect)(implicit C: Context): Effect =
    Context.resolveType(tpe.id).asEffect

  def resolve(tpe: source.Effects)(implicit C: Context): Effects =
    Effects(tpe.effs.map(resolve).toSeq: _*) // TODO this otherwise is calling the wrong apply

  def resolve(e: source.Effectful)(implicit C: Context): Effectful =
    Effectful(resolve(e.tpe), resolve(e.eff))

  /**
   * Resolves type variables, term vars are resolved as part of resolve(tree: Tree)
   */
  def resolve(id: Id)(implicit C: Context): TypeVar = {
    val sym = TypeVar(Name(id))
    Context.define(id, sym)
    sym
  }
}

/**
 * Environment Utils -- we use a mutable cell to express adding definitions more easily
 * The kiama environment uses immutable binding since they thread the environment through
 * their attributes.
 */
trait NamerOps { self: Context =>

  // State Access
  // ============
  private[namer] def scope: Scope = namerState.scope

  // TODO we only want to add a seed to a name under the following conditions:
  // - there is already another instance of that name in the same
  //   namespace.
  // - if it is not already fully qualified
  private[namer] def freshTermName(id: Id): Name = {
    val alreadyBound = scope.currentTermsFor(id.name).size
    val seed = if (alreadyBound > 0) "$" + alreadyBound else ""
    Name(id.name + seed, module)
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
  private[namer] def resolveTerm(id: Id): TermSymbol = {
    val sym = scope.lookupFirstTerm(id.name)
    assignSymbol(id, sym)
    sym
  }

  /**
   * Resolves a potentially overloaded call target
   */
  private[namer] def resolveCalltarget(id: Id): BlockSymbol = {
    val syms = scope.lookupOverloaded(id.name) map {
      _ collect {
        case b: BlockParam  => b
        case b: ResumeParam => b
        case b: Fun         => b
        case _              => abort("Expected callable")
      }
    }

    if (syms.isEmpty) {
      abort(s"Cannot resolve function ${id.name}")
    }

    val target = new CallTarget(Name(id), syms)
    assignSymbol(id, target)
    target
  }

  /**
   * Variables have to be resolved uniquely
   */
  private[namer] def resolveVar(id: Id): TermSymbol = resolveTerm(id) match {
    case b: BlockParam => abort("Blocks have to be fully applied and can't be used as values.")
    case other         => other
  }

  private[namer] def resolveType(id: Id): TypeSymbol = {
    val sym = scope.lookupType(id.name)
    assignSymbol(id, sym)
    sym
  }

  private[namer] def scoped[R](block: => R): R = {
    val before = namerState
    namerState = before.copy(scope = before.scope.enter)
    val result = block
    namerState = before
    result
  }
}
