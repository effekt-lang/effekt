package effekt
package namer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Context, Phase }
import effekt.context.assertions.{ SymbolAssertions, TypeAssertions }
import effekt.source.{ Def, Id, Tree }
import effekt.source.traversal._
import effekt.symbols._
import effekt.util.scopes._

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
  source: Source,
  module: effekt.source.ModuleDecl,
  terms: Scope[TermSymbol],
  types: Scope[TypeSymbol]
)

class Namer extends Phase { namer =>

  val name = "Namer"

  def run(src: Source, decl: source.ModuleDecl)(implicit compiler: Context): Module = {

    val topLevelTerms = toplevel[TermSymbol](builtins.rootTerms)
    val topLevelTypes = toplevel[TypeSymbol](builtins.rootTypes)

    val (terms, types) = decl.imports.foldLeft((topLevelTerms, topLevelTypes)) {
      case ((terms, types), source.Import(path)) =>
        val mod = compiler.resolve(path)
        (terms.enterWith(mod.terms), types.enterWith(mod.types))
    }

    val state = NamerState(src, decl, terms.enter, types.enter)
    compiler.namerState = state
    compiler.phases.init(this)


    resolve(decl)

    Module(
      decl,
      src,
      state.terms.bindings.toMap,
      state.types.bindings.toMap)
  }

  /**
   * An effectful traversal with two side effects:
   * 1) the passed environment is enriched with definitions
   * 2) names are resolved using the environment and written to the table
   */
  def resolve(tree: Tree)(implicit C: Context): Unit = Compiler.focusing(tree) {

    // (1) === Binding Occurrences ===
    case source.ModuleDecl(path, imports, decls) =>
      decls foreach { d => resolveDef(d, true) }
      Compiler scoped { resolveAll(decls) }

    case source.DefStmt(d, rest) =>
      Compiler scoped {
        resolveDef(d, false)
        resolve(d)
        resolve(rest)
      }

    case source.ValueParam(id, tpe) =>
      C.define(id, ValueParam(C.localName(id), tpe.map(resolveValueType)))

    case source.BlockParam(id, tpe) =>
      C.define(id, BlockParam(C.localName(id), resolveBlockType(tpe)))

    // FunDef and EffDef have already been resolved as part of the module declaration
    case f @ source.FunDef(id, tparams, params, ret, body) =>
      val funSym = Compiler.get(f)
      Compiler scoped {
        funSym.tparams.foreach { p =>
          Compiler.bind(p)
        }
        Compiler.bind(funSym.params)
        resolve(body)
      }

    case source.EffDef(id, tparams, params, ret) => ()
    case source.DataDef(id, tparams, ctors) => ()
    case source.ExternType(id, tparams) => ()
    case source.ExternEffect(id, tparams) => ()
    case source.ExternFun(pure, id, tparams, params, ret, body) => ()
    case source.ExternInclude(path) => ()

    case source.TryHandle(body, clauses) =>
      Compiler scoped { resolve(body) }
      resolveAll(clauses)

    case source.OpClause(op, params, body, resumeId) =>
      Compiler.at(op) { Compiler.resolveTerms(op) }
      val ps = params.map(resolveValueParams)
      Compiler scoped {
        Compiler.bind(ps)
        C.define(resumeId, ResumeParam())
        resolve(body)
      }

    case source.Clause(op, params, body) =>
      Compiler.at(op) { Compiler.resolveTerms(op) }
      val ps = params.map(resolveValueParams)
      Compiler scoped {
        Compiler.bind(ps)
        resolve(body)
      }

    case source.BlockArg(params, stmt) =>
      val ps = resolveValueParams(source.ValueParams(params)) // TODO drop wrapping after refactoring
      Compiler scoped {
        Compiler.bind(List(ps))
        resolve(stmt)
      }

    // (2) === Bound Occurrences ===

    case source.Call(id, targs, args) =>
      Compiler.resolveFilter(id) {
        case b : BlockParam => b
        case b : ResumeParam => b
        case b : Fun => b
        case _ => Compiler.error("Expected callable")
      }
      targs foreach resolveValueType
      resolveAll(args)

    case source.Var(id) => Compiler.resolveFilter(id) {
      case b : BlockParam => Compiler.error("Blocks have to be fully applied and can't be used as values.")
      case other => other
    }

    case tpe: source.ValueType => resolveValueType(tpe)
    case tpe: source.BlockType => resolveBlockType(tpe)

    // THIS COULD ALSO BE A TYPE!
    case id : Id => Compiler.resolveTerms(id)

    case other => resolveAll(other)
  }

  def resolveAll(obj: Any)(implicit C: Context): Unit =
    all[Context](c => t => resolve(t)(c))(obj)

  /**
   * Resolve Parameters as part of resolving function signatures
   *
   * Since we annotate functions and effect declarations with resolved types, we need to
   * resolve the parameters.
   *
   * Importantly, resolving them will *not* add the parameters as binding occurence in the current scope.
   * This is done separately by means of `bind`
   */
  def resolveParamSection(params: source.ParamSection)(implicit C: Context): List[Param] = params match {
    case ps : source.ValueParams => resolveValueParams(ps)
    case source.BlockParam(id, tpe) =>
      val sym = BlockParam(C.localName(id), resolveBlockType(tpe))
      Compiler.put(id, sym)
      List(sym)
  }
  def resolveValueParams(ps: source.ValueParams)(implicit C: Context): List[ValueParam] =
    ps.params map { p =>
      val sym = ValueParam(C.localName(p.id), p.tpe.map(resolveValueType))
      Compiler.put(p.id, sym)
      sym
    }

  // TODO consider setting owner, instead of this qualify hack
  def resolveDef(d: Def, qualify: Boolean)(implicit C: Context): Unit = {

    def name(id: Id) = if (qualify) {
      C.qualifiedName(id)
    } else {
      C.localName(id)
    }

    Compiler.focusing(d) {

      case d @ source.ValDef(id, annot, binding) =>
        val tpe = annot.map(resolveValueType)
        resolve(binding)
        C.define(id, ValBinder(C.localName(id), tpe, d))

      case d @ source.VarDef(id, annot, binding) =>
        val tpe = annot.map(resolveValueType)
        resolve(binding)
        C.define(id, VarBinder(C.localName(id), tpe, d))

      case f @ source.FunDef(id, tparams, params, annot, body) =>
        val sym = Compiler scoped {
          // we create a new scope, since resolving type params introduces them in this scope
          UserFunction(C.freshTermName(id, qualify), tparams map resolveTypeParam, params map resolveParamSection, annot map resolveEffectful, f)
        }
        C.define(id, sym)

      case e @ source.EffDef(id, tparams, params, ret) =>
        // we use the localName for effects, since they will be bound as capabilities
        val effectSym = UserEffect(C.localName(id), Nil)
        val opSym = Compiler scoped {
          val tps = tparams map resolveTypeParam
          val tpe = Effectful(resolveValueType(ret), Effects(List(effectSym)))
          EffectOp(C.localName(id), tps, params map resolveValueParams, Some(tpe), effectSym)
        }
        effectSym.ops = List(opSym)
        // we would need a second id that is the definition of the operation
        C.define(id, effectSym)
        Compiler.bind(opSym)

      case d @ source.DataDef(id, tparams, ctors) =>
        val (typ, tps) = Compiler scoped {
          val tps = tparams map resolveTypeParam
          (DataType(name(id), tps), tps)
        }
        C.define(id, typ)
        val cs = ctors map {
          case source.Constructor(id, ps) =>
            val sym = Compiler scoped {
              tps.foreach { t => Compiler.bind(t) }
              Constructor(name(id), ps map resolveValueParams, typ)
            }
            C.define(id, sym)
            sym
        }
        typ.ctors = cs

      case d @ source.ExternType(id, tparams) =>
        C.define(id, Compiler scoped {
          val tps = tparams map resolveTypeParam
          BuiltinType(name(id), tps)
        })

      case d @ source.ExternEffect(id, tparams) =>
        C.define(id, Compiler scoped {
          val tps = tparams map resolveTypeParam
          BuiltinEffect(name(id), tps)
        })

      case d @ source.ExternFun(pure, id, tparams, params, ret, body) =>
        C.define(id, Compiler scoped {
          val tps = tparams map resolveTypeParam
          val ps: Params = params map resolveParamSection
          val tpe = resolveEffectful(ret)
          BuiltinFunction(name(id), tps, ps, Some(tpe), pure, body)
        })

      case d @ source.ExternInclude(path) =>
        d.contents = Compiler.resolveInclude(Compiler.source, path)
        ()
    }
  }


  /**
   * Resolve Types
   *
   * resolving a type means reconstructing the composite type (e.g. Effectful, ...) from
   * symbols, instead of trees.
   */
  def resolveValueType(tpe: source.ValueType)(implicit C: Context): ValueType = tpe match {
    case source.TypeApp(id, args) =>
      val data = C.resolveType(id).asValueType
      TypeApp(data, args.map(resolveValueType))
    case source.TypeVar(id) => C.resolveType(id).asValueType
  }

  def resolveBlockType(tpe: source.BlockType)(implicit C: Context): BlockType =
    BlockType(Nil, List(tpe.params.map(resolveValueType)), resolveEffectful(tpe.ret))

  def resolveEffect(tpe: source.Effect)(implicit C: Context): Effect =
    C.resolveType(tpe.id).asEffect

  def resolveEffects(tpe: source.Effects)(implicit C: Context): Effects =
    Effects(tpe.effs.map(resolveEffect))

  def resolveEffectful(e: source.Effectful)(implicit C: Context): Effectful =
    Effectful(resolveValueType(e.tpe), resolveEffects(e.eff))

  def resolveTypeParam(id: Id)(implicit C: Context): TypeVar = {
    val sym = TypeVar(C.localName(id))
    C.define(id, sym)
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
  def source: Source = namerState.source
  def module: effekt.source.ModuleDecl = namerState.module
  def terms: Scope[TermSymbol] = namerState.terms
  def types: Scope[TypeSymbol] = namerState.types

  def qualifiedName(id: Id): Name = QualifiedName(module.path, id.name)
  def localName(id: Id): Name = LocalName(id.name)

  // TODO we only want to add a seed to a name under the following conditions:
  // - there is already another instance of that name in the same
  //   namespace.
  // - if it is not already fully qualified
  def freshTermName(id: Id, qualified: Boolean = false) = {
    // how many terms of the same name are already in scope?
    val alreadyBound = terms.lookup(id.name).toList.size
    val seed = "" // if (alreadyBound > 0) "$" + alreadyBound else ""

    if (qualified) {
      QualifiedName(module.path, id.name + seed)
    } else {
      LocalName(id.name + seed)
    }
  }

  // Name Binding and Resolution
  // ===========================
  def define(id: Id, s: TermSymbol): Unit = {
    put(id, s)
    terms.define(id.name, s)
  }

  def define(id: Id, s: TypeSymbol): Unit = {
    put(id, s)
    types.define(id.name, s)
  }

  def bind(s: TermSymbol): Unit = terms.define(s.name.name, s)

  def bind(s: TypeSymbol): Unit = types.define(s.name.name, s)

  def bind(params: List[List[Param]]): Context = {
    params.flatten.foreach { p => bind(p) }
    this
  }

  // lookup and resolve the given id from the environment and
  // store a binding in the symbol table
  def resolveTerms(id: Id): List[TermSymbol] = {
    val sym = terms.lookup(id.name).getOrElse { abort(s"Could not resolve term ${id.name}") }
    put(id, sym)
    List(sym)
  }

  // for positions that do not allow overloading (for now)
  def resolveFilter[A](id: Id)(filter: PartialFunction[TermSymbol, A]): List[A] = {
    val sym = terms.lookup(id.name).getOrElse { abort(s"Could not resolve term ${id.name}") }
    put(id, sym)

    List(sym).collect(filter)
  }

  def resolveType(id: Id): TypeSymbol = {
    val sym = types.lookup(id.name).getOrElse { abort(s"Could not resolve type ${id.name}") }
    put(id, sym)
    sym
  }

  def scoped[R](block: => R): R = {
    val before = namerState
    namerState = before.copy(terms = before.terms.enter, types = before.types.enter)
    val result = block
    namerState = before
    result
  }
}
