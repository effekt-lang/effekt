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
 */
case class NamerState(
  terms: Scope[TermSymbol],
  types: Scope[TypeSymbol]
)

class Namer extends Phase[Module, Module] { namer =>

  val phaseName = "Namer"

  def run(mod: Module)(implicit C: Context): Option[Module] = {
    Some(resolve(mod))
  }

  def resolve(mod: Module)(implicit C: Context): Module = {

    var terms: Scope[TermSymbol] = toplevel(Map.empty)
    var types: Scope[TypeSymbol] = toplevel(builtins.rootTypes)

    // process all imports, updating the terms and types in scope
    mod.decl.imports foreach {
      case im @ source.Import(path) => Context.at(im) {
        val modImport = Context.moduleOf(path)
        terms = terms.enterWith(modImport.terms)
        types = types.enterWith(modImport.types)
      }
    }

    // create new scope for the current module
    terms = terms.enter
    types = types.enter

    Context.namerState = NamerState(terms, types)

    resolveGeneric(mod.decl)

    mod.export(terms.bindings.toMap, types.bindings.toMap)
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

    case source.EffDef(id, tparams, params, ret) => ()
    case source.TypeDef(id, tparams, tpe) => ()
    case source.EffectDef(id, effs) => ()
    case source.DataDef(id, tparams, ctors) => ()
    case source.ExternType(id, tparams) => ()
    case source.ExternEffect(id, tparams) => ()
    case source.ExternFun(pure, id, tparams, params, ret, body) => ()
    case source.ExternInclude(path) => ()

    case source.TryHandle(body, clauses) =>
      Context scoped { resolveGeneric(body) }
      resolveAll(clauses)

    case source.OpClause(op, params, body, resumeId) =>
      Context.at(op) { Context.resolveTerms(op) }
      val ps = params.map(resolve)
      Context scoped {
        Context.bind(ps)
        Context.define(resumeId, ResumeParam(C.module))
        resolveGeneric(body)
      }

    case source.Clause(op, params, body) =>
      Context.at(op) { Context.resolveTerms(op) }
      val ps = params.map(resolve)
      Context scoped {
        Context.bind(ps)
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
      Context.resolveFilter(id) {
        case b: BlockParam  => b
        case b: ResumeParam => b
        case b: Fun         => b
        case _              => Context.error("Expected callable")
      }
      targs foreach resolve
      resolveAll(args)

    case source.Var(id) => Context.resolveFilter(id) {
      case b: BlockParam => Context.error("Blocks have to be fully applied and can't be used as values.")
      case other         => other
    }

    case tpe: source.ValueType => resolve(tpe)
    case tpe: source.BlockType => resolve(tpe)

    // THIS COULD ALSO BE A TYPE!
    case id: Id                => Context.resolveTerms(id)

    case other                 => resolveAll(other)
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

    case source.EffDef(id, tparams, params, ret) =>
      // we use the localName for effects, since they will be bound as capabilities
      val effectSym = UserEffect(Name(id), Nil)
      val opSym = Context scoped {
        val tps = tparams map resolve
        val tpe = Effectful(resolve(ret), Effects(List(effectSym)))
        EffectOp(Name(id), tps, params map resolve, Some(tpe), effectSym)
      }
      effectSym.ops = List(opSym)
      // we would need a second id that is the definition of the operation
      Context.define(id, effectSym)
      Context.bind(opSym)

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
        (DataType(Name(id), tps), tps)
      }
      Context.define(id, typ)
      val cs = ctors map {
        case source.Constructor(id, ps) =>
          val sym = Context scoped {
            tps.foreach { t => Context.bind(t) }
            Constructor(Context.freshTermName(id), ps map resolve, typ)
          }
          Context.define(id, sym)
          sym
      }
      typ.ctors = cs

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

    case source.ExternFun(pure, id, tparams, params, ret, body) =>
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        val ps: Params = params map resolve
        val tpe = resolve(ret)
        BuiltinFunction(Context.freshTermName(id), tps, ps, Some(tpe), pure, body)
      })

    case d @ source.ExternInclude(path) =>
      d.contents = Context.contentsOf(path)
      ()
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
  private[namer] def terms: Scope[TermSymbol] = namerState.terms
  private[namer] def types: Scope[TypeSymbol] = namerState.types

  // TODO we only want to add a seed to a name under the following conditions:
  // - there is already another instance of that name in the same
  //   namespace.
  // - if it is not already fully qualified
  private[namer] def freshTermName(id: Id): Name = {
    val alreadyBound = terms.lookupHere(id.name).toList.size
    val seed = if (alreadyBound > 0) "$" + alreadyBound else ""
    Name(id.name + seed, module)
  }

  // Name Binding and Resolution
  // ===========================
  private[namer] def define(id: Id, s: TermSymbol): Unit = {
    assignSymbol(id, s)
    terms.define(id.name, s)
  }

  private[namer] def define(id: Id, s: TypeSymbol): Unit = {
    assignSymbol(id, s)
    types.define(id.name, s)
  }

  private[namer] def bind(s: TermSymbol): Unit = terms.define(s.name.name, s)

  private[namer] def bind(s: TypeSymbol): Unit = types.define(s.name.name, s)

  private[namer] def bind(params: List[List[Param]]): Context = {
    params.flatten.foreach { p => bind(p) }
    this
  }

  // lookup and resolve the given id from the environment and
  // store a binding in the symbol table
  private[namer] def resolveTerms(id: Id): List[TermSymbol] = {
    val sym = terms.lookup(id.name).getOrElse { abort(s"Could not resolve term ${id.name}") }
    assignSymbol(id, sym)
    List(sym)
  }

  // for positions that do not allow overloading (for now)
  private[namer] def resolveFilter[A](id: Id)(filter: PartialFunction[TermSymbol, A]): List[A] = {
    val sym = terms.lookup(id.name).getOrElse { abort(s"Could not resolve term ${id.name}") }
    assignSymbol(id, sym)

    List(sym).collect(filter)
  }

  private[namer] def resolveType(id: Id): TypeSymbol = {
    val sym = types.lookup(id.name).getOrElse { abort(s"Could not resolve type ${id.name}") }
    assignSymbol(id, sym)
    sym
  }

  private[namer] def scoped[R](block: => R): R = {
    val before = namerState
    namerState = before.copy(terms = before.terms.enter, types = before.types.enter)
    val result = block
    namerState = before
    result
  }
}
