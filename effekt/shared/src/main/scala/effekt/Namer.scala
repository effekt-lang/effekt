package effekt
package namer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{Annotations, Context, ContextOps}
import effekt.context.assertions.*
import effekt.typer.Substitutions
import effekt.source.{Def, Id, IdDef, IdRef, Many, MatchGuard, ModuleDecl, Term, Tree, sourceOf}
import effekt.symbols.*
import effekt.util.messages.ErrorMessageReifier
import effekt.symbols.scopes.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

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
    Context.using(module = mod, focus = tree) { resolve(mod) }
    Some(NameResolved(source, tree, mod))
  }

  /** Shadow stack of modules currently named, for detection of cyclic imports */
  private val currentlyNaming: DynamicVariable[List[ModuleDecl]] = DynamicVariable(List())
  /** Current parent definition (if any) */
  private val parentDefinitions: DynamicVariable[List[Def]] = DynamicVariable(List())
  /** Counter to disambiguate hole identifiers */
  private val holeCount: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()

  /**
   * Run body in a context where we are currently naming `mod`.
   * Produces a cyclic import error when this is already the case
   */
  private def recursiveProtect[R](mod: ModuleDecl)(body: => R)(using Context): R = {
    if (currentlyNaming.value.exists { m => m.span.source == mod.span.source }) {
      val cycle = mod :: currentlyNaming.value.takeWhile{ m => m.span.source != mod.span.source }.reverse
      Context.abort(
        pretty"""Cyclic import: ${mod.path} depends on itself, via:\n\t${cycle.map(_.path).mkString(" -> ")} -> ${mod.path}""")
    } else {
      currentlyNaming.withValue(mod :: currentlyNaming.value) {
        body
      }
    }
  }

  def resolve(mod: Module)(using Context): ModuleDecl = {
    val Module(decl, src) = mod
    val scope = scopes.toplevel(Context.module.namespace, builtins.rootBindings)

    Context.initNamerstate(scope)

    def importDependency(filePath: String) =
      val included = Context.moduleOf(filePath)

      // Fully qualified:
      //   include "foo/bar/baz.effekt" as foo::bar::baz
      scope.importAs(included.exports, included.namespace)

      // Bind the module itself:
      //   include "foo/bar/baz.effekt" as baz
      scope.importAs(included.exports, List(included.name.name))

      // Open it:
      //   import baz::*
      scope.importAll(included.exports)
      included

    // process the prelude (but don't if we are processing the prelude already)
    val preludes = Context.config.prelude()
    val isPrelude = preludes.contains(decl.path)

    val processedPreludes = if (!isPrelude) {
      preludes.map(importDependency)
    } else { Nil }

    // process all includes, updating the terms and types in scope
    val includes = decl.includes collect {
      case im @ source.Include(path, span) =>
        // [[recursiveProtect]] is called here so the source position is the recursive import
        val mod = Context.at(im) { recursiveProtect(decl){ importDependency(path) } }
        Context.annotate(Annotations.IncludedSymbols, im, mod)
        mod
    }

    holeCount.clear()
    Context.timed(phaseName, src.name) { resolve(decl) }

    // We only want to import each dependency once.
    val allIncludes = (processedPreludes ++ includes).distinct

    Context.module.exports(allIncludes, scope.exports)
    decl
  }

  /**
   * To allow mutually recursive definitions, here we only resolve the declarations,
   * not the bodies of functions.
   */
  def preresolve(d: Def)(using Context): Unit = Context.focusing(d) {

    case d @ source.ValDef(id, annot, binding, doc, span) =>
      ()

    case d @ source.VarDef(id, annot, binding, doc, span) =>
      ()

    case d @ source.RegDef(id, annot, region, binding, doc, span) =>
      ()

    case source.NamespaceDef(id, definitions, doc, span) =>
      Context.namespace(id.name) {
        definitions.foreach(preresolve)
      }

    // allow recursive definitions of objects
    case d @ source.DefDef(id, captures, annot, source.New(source.Implementation(interface, clauses, _), _), doc, span) =>
      val tpe = Context.at(interface) { resolveBlockRef(interface) }
      val cpts = captures.unspan.map { resolve }
      val sym = Binder.DefBinder(Context.nameFor(id), cpts, Some(tpe), d)
      Context.define(id, sym)

    case d @ source.DefDef(id, captures, annot, block, doc, span) =>
      ()

    case f @ source.FunDef(id, tparams, vparams, bparams, captures, annot, body, doc, span) =>
      val uniqueId = Context.nameFor(id)
      val cpts = captures.map { resolve }.unspan
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
        UserFunction(uniqueId, tps.unspan, vps.unspan, bps.unspan, cpts, ret.unspan.map { _._1 }, ret.unspan.map { _._2 }, f)
      }
      Context.define(id, sym)

    case decl @ source.InterfaceDef(id, tparams, ops, doc, span) =>
      val effectName = Context.nameFor(id)
      // we use the localName for effects, since they will be bound as capabilities
      val effectSym = Context scoped {
        val tps = tparams map resolve
        // we do not resolve the effect operations here to allow them to refer to types that are defined
        // later in the file
        Interface(effectName, tps.unspan, List(), decl)
      }
      Context.define(id, effectSym)

    case d @ source.TypeDef(id, tparams, tpe, doc, span) =>
      val tps = Context scoped { tparams map resolve }
      val alias = Context scoped {
        tps.foreach { t => Context.bind(t) }
        TypeAlias(Context.nameFor(id), tps, resolveValueType(tpe), d)
      }
      Context.define(id, alias)

    case d @ source.EffectDef(id, tparams, effs, doc, span) =>
      val tps = Context scoped { tparams map resolve }
      val alias = Context scoped {
        tps.foreach { t => Context.bind(t) }
        EffectAlias(Context.nameFor(id), tps, resolve(effs), d)
      }
      Context.define(id, alias)

    case d @ source.DataDef(id, tparams, ctors, doc, span) =>
      val typ = Context scoped {
        val tps = tparams map resolve
        // we do not resolve the constructors here to allow them to refer to types that are defined
        // later in the file
        DataType(Context.nameFor(id), tps.unspan, List(), d)
      }
      Context.define(id, typ)

    case d @ source.RecordDef(id, tparams, fields, doc, span) =>
      lazy val sym: Record = {
        val tps = Context scoped { tparams map resolve }
        // we do not resolve the fields here to allow them to refer to types that are defined
        // later in the file
        Record(Context.nameFor(id), tps.unspan, null, d)
      }
      Context.define(id, sym)

    case d @source.ExternType(id, tparams, doc, span) =>
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        ExternType(Context.nameFor(id), tps.unspan, d)
      })

    case decl @ source.ExternInterface(id, tparams, doc, span) =>
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        ExternInterface(Context.nameFor(id), tps, decl)
      })

    case d @ source.ExternDef(id, tparams, vparams, bparams, captures, ret, bodies, doc, span) =>
      val name = Context.nameFor(id)
      val capt = resolve(captures)
      Context.define(id, Context scoped {
        val tps = tparams map resolve
        val vps = vparams map resolve
        val bps = bparams map resolve

        val (tpe, eff) = Context scoped {
          Context.bindBlocks(bps)
          resolve(ret)
        }

        ExternFunction(name, tps.unspan, vps.unspan, bps.unspan, tpe, eff, capt, bodies, d)
      })

    case d @ source.ExternResource(id, tpe, doc, span) =>
      val name = Context.nameFor(id)
      val btpe = resolveBlockType(tpe)
      val sym = ExternResource(name, btpe, d)
      Context.define(id, sym)
      Context.bindBlock(sym)

    case d @ source.ExternInclude(ff, path, Some(contents), _, doc, span) =>
      ()

    case d @ source.ExternInclude(ff, path, None, _, doc, span) =>
      // only load include if it is required by the backend.
      if (ff matches Context.compiler.supportedFeatureFlags) {
        d.contents = Some(Context.contentsOf(path).getOrElse {
          Context.abort(s"Missing include: ${path}")
        })
      } else {
        d.contents = None
      }
  }

  def resolve(m: source.ModuleDecl)(using Context): Unit = Context.focusing(m) {
    case source.ModuleDecl(path, includes, definitions, doc, span) =>
      definitions foreach { preresolve }
      definitions.foreach { resolve }
  }

  def resolve(s: source.Stmt)(using Context): Unit = Context.focusing(s) {

    case source.DefStmt(d, rest, span) =>
      // resolve declarations but do not resolve bodies
      preresolve(d)
      // resolve bodies
      resolve(d)
      resolve(rest)

    case source.ExprStmt(term, rest, span) =>
      resolve(term)
      resolve(rest)

    case source.Return(term, span) =>
      resolve(term)

    case source.BlockStmt(stmts, span) =>
      Context scoped { resolve(stmts) }
  }

  def resolve(d: source.Def)(using Context): Unit = withDefinition(d) {

    case d @ source.ValDef(id, annot, binding, doc, span) =>
      val tpe = annot.map(resolveValueType)
      resolve(binding)
      Context.define(id, ValBinder(Context.nameFor(id), tpe, d))


    // Local mutable state
    case d @ source.VarDef(id, annot, binding, doc, span) =>
      val tpe = annot.map(resolveValueType)

      resolve(binding)
      val sym = VarBinder(Context.nameFor(id), tpe, d)
      Context.define(id, sym)
      Context.bind(sym.capture)

    // allocation into a region
    case d @ source.RegDef(id, annot, region, binding, doc, span) =>
      val tpe = annot.map(resolveValueType)
      val reg = Context.resolveTerm(region) match {
        case t: BlockSymbol => t
        case _ => Context.abort("Region needs to be a block.")
      }

      resolve(binding)
      val sym = RegBinder(Context.nameFor(id), tpe, reg, d)

      Context.define(id, sym)

    // already has been preresolved (to enable recursive definitions)
    case d @ source.DefDef(id, captures, annot, source.New(impl, _), doc, span) =>
      resolve(impl)

    case d @ source.DefDef(id, captures, annot, binding, doc, span) =>
      val tpe = annot.map(resolveBlockType)
      resolve(binding)
      val cpts = captures.unspan.map { resolve }
      Context.define(id, DefBinder(Context.nameFor(id), cpts, tpe.unspan, d))

    // FunDef and InterfaceDef have already been resolved as part of the module declaration
    case f @ source.FunDef(id, tparams, vparams, bparams, captures, ret, body, doc, span) =>
      val sym = f.symbol
      Context.scopedWithName(id.name) {
        sym.tparams.foreach { p => Context.bind(p) }
        Context.bindValues(sym.vparams)
        Context.bindBlocks(sym.bparams)

        resolve(body)
      }

    case f @ source.ExternDef(id, tparams, vparams, bparams, captures, ret, bodies, doc, span) =>
      val sym = f.symbol
      Context.scopedWithName(id.name) {
        sym.tparams.foreach { p => Context.bind(p) }
        Context.bindValues(sym.vparams)
        Context.bindBlocks(sym.bparams)
        bodies.foreach {
          case source.ExternBody.StringExternBody(ff, body, span) => body.args.foreach(resolve)
          case source.ExternBody.EffektExternBody(ff, body, span) => resolve(body)
          case u: source.ExternBody.Unsupported => u
        }
      }

    case source.InterfaceDef(interfaceId, tparams, operations, doc, span) =>
      // symbol has already been introduced by the previous traversal
      val interface = Context.symbolOf(interfaceId).asInterface
      interface.operations = operations.map {
        case op @ source.Operation(id, tparams, vparams, bparams, ret, doc, span) => Context.at(op) {
          val name = Context.nameFor(id)

          val opSym = Context.scopedWithName(id.name) {
            // the parameters of the interface are in scope
            interface.tparams.foreach { p => Context.bind(p) }

            val tps = tparams map resolve

            val resVparams = vparams map resolve
            val resBparams = bparams map resolve

            // bring capture names in scope that are introduced by blockparameters
            resBparams.map { b => Context.bind(b.capture) }

            // The type parameters of an effect op are:
            //   1) all type parameters on the effect, followed by
            //   2) the annotated type parameters on the concrete operation
            val (result, effects) = resolve(ret)

            Operation(name, interface.tparams ++ tps.unspan, resVparams, resBparams, result, effects, interface, op)
          }

          // define in namespace ...
          Context.namespace(interfaceId.name) {
            Context.define(id, opSym)
          }
          // ... and bind outside
          Context.bind(opSym)
          opSym
        }
      }

    case source.NamespaceDef(id, definitions, doc, span) =>
      Context.namespace(id.name) {
        definitions.foreach(resolve)
      }

    // The type itself has already been resolved, now resolve constructors
    case d @ source.DataDef(typeId, tparams, ctors, doc, span) =>
      val data = d.symbol
      val constructors = ctors map {
        case c @ source.Constructor(id, tparams, ps, doc, span) =>
          val constructor = Context scoped {
            val name = Context.nameFor(id)
            val tps = tparams map resolve
            Constructor(name, data.tparams ++ tps.unspan, Nil, data, c)
          }
          // DataType::Constructor()
          Context.namespace(typeId.name) {
            Context.define(id, constructor)
          }
          constructor.fields = resolveFields(ps.unspan, constructor, false)
          constructor
      }
      // export DataType::{Constructor1, ...}
      constructors.foreach { c => Context.bind(c) }

      data.constructors = constructors

    // The record has been resolved as part of the preresolution step
    case d @ source.RecordDef(id, tparams, fs, doc, span) =>
      val record = d.symbol
      val name = Context.nameFor(id)
      val constructor = Constructor(name, record.tparams, Nil, record, d)
      // we define the constructor on a copy to avoid confusion with symbols
      Context.define(id.clone, constructor)
      record.constructor = constructor
      constructor.fields = resolveFields(fs.unspan, constructor, true)

    case source.TypeDef(id, tparams, tpe, doc, span)     => ()
    case source.EffectDef(id, tparams, effs, doc, span)  => ()
    case source.ExternType(id, tparams, doc, span)       => ()
    case source.ExternInterface(id, tparams, doc, span)  => ()
    case source.ExternResource(id, tpe, doc, span)       => ()
    case source.ExternInclude(ff, path, _, _, doc, span) => ()
  }

  def resolve(a: source.ValueArg)(using Context): Unit = Context.focusing(a) { _ =>
    resolve(a.value)
  }

  def resolve(t: source.Term)(using Context): Unit = Context.focusing(t) {

    case source.Literal(value, tpe, _) => ()

    case hole @ source.Hole(id, Template(strings, args), span) =>
      val h = Hole(Name.local(freshHoleId), hole)
      Context.addHole(h)
      Context.assignSymbol(id, h)
      Context scoped {
        args.foreach(resolve)
      }

    case source.Unbox(term, _) => resolve(term)

    case source.New(impl, _) => resolve(impl)

    case source.Match(scrutinees, clauses, default, _) =>
      scrutinees.foreach(resolve)
      clauses.foreach(resolve)
      Context.scoped { default.foreach(resolve) }

    case source.If(guards, thn, els, _) =>
      Context scoped { guards.foreach(resolve); resolve(thn) }
      Context scoped { resolve(els) }

    case source.While(guards, block, default, _) =>
      Context scoped { guards.foreach(resolve); resolve(block) }
      Context scoped { default foreach resolve }

    case tree @ source.TryHandle(body, handlers, _) =>
      handlers.foreach(resolve)

      Context scoped {

        // bind all annotated capabilities
        handlers.foreach { handler =>
          handler.capability.foreach { p =>
            Context.bindBlock(resolve(p))
          }
        }

        resolve(body)
      }

    case tree @ source.Region(name, body, _) =>
      val reg = BlockParam(Name.local(name.name), Some(builtins.TRegion), tree)
      Context.define(name, reg)
      Context scoped {
        Context.bindBlock(reg)
        resolve(body)
      }

    case f @ source.BlockLiteral(tparams, vparams, bparams, stmt, _) =>
      Context scoped {
        val tps = tparams map resolve
        val vps = vparams map resolve
        val bps = bparams map resolve

        Context.bindValues(vps)
        Context.bindBlocks(bps)

        resolve(stmt)
      }

    case source.Box(capt, block, _) =>
      capt foreach resolve
      resolve(block)

    // (2) === Bound Occurrences ===

    case source.Select(receiver, target, _) =>
      Context.panic("Cannot happen since Select is introduced later")

    case source.MethodCall(receiver, target, targs, vargs, bargs, _) =>
      resolve(receiver)

      // We are a bit context sensitive in resolving the method
      Context.focusing(target) { _ =>
        receiver match {
          case source.Var(id, _) => Context.resolveTerm(id) match {
            // (foo: ValueType).bar(args)  = Call(bar, foo :: args)
            case symbol: ValueSymbol =>
              if !Context.resolveOverloadedFunction(target)
              then Context.abort(pp"Cannot resolve function ${target}, called on a value receiver.")

            case symbol: RefBinder =>
              if !Context.resolveOverloadedFunction(target)
              then Context.abort(pp"Cannot resolve function ${target}, called on a receiver that is a reference.")

            // (foo: BlockType).bar(args)  = Invoke(foo, bar, args)
            case symbol: BlockSymbol =>
              if !Context.resolveOverloadedOperation(target)
              then Context.abort(pp"Cannot resolve operation ${target}, called on a receiver that is a computation.")
          }

          case source.Unbox(term, _) =>
            if !Context.resolveOverloadedFunction(target)
            then Context.abort(pp"Cannot resolve operation ${target}, called on an unboxed computation.")

          // expr.bar(args) = Call(bar, expr :: args)
          case term =>
            if !Context.resolveOverloadedFunction(target)
            then Context.abort(pp"Cannot resolve function ${target}, called on an expression.")
        }
      }
      targs foreach resolveValueType
      vargs foreach resolve
      bargs foreach resolve

    case source.Do(target, targs, vargs, bargs, _) =>
      Context.resolveEffectCall(target)
      targs foreach resolveValueType
      vargs foreach resolve
      bargs foreach resolve

    case source.Call(target, targs, vargs, bargs, _) =>
      Context.focusing(target) {
        case source.IdTarget(id)     => Context.resolveFunctionCalltarget(id)
        case source.ExprTarget(expr) => resolve(expr)
      }
      targs foreach resolveValueType
      vargs foreach resolve
      bargs foreach resolve

    case source.Var(id, _) => Context.resolveVar(id)

    case source.Assign(id, expr, _) => Context.resolveVar(id) match {
      case _: VarBinder | _: RegBinder => resolve(expr)
      case _: ValBinder | _: ValueParam => Context.abort(pretty"Can only assign to mutable variables, but ${id.name} is a constant.")
      case y: Wildcard => Context.abort(s"Trying to assign to a wildcard, which is not allowed.")
      case _ => Context.abort(s"Can only assign to mutable variables.")
    }
  }

  /**
   * Track the current top-level definition (if any)
   */
  def withDefinition[T](d: Def)(block: Def => T)(using Context): T =
    val defs = parentDefinitions.value
    parentDefinitions.withValue(d :: defs) {
      Context.focusing(d)(block)
    }

  // TODO move away
  def resolveFields(params: List[source.ValueParam], constructor: Constructor, defineAccessors: Boolean)(using Context): List[Field] = {
    val vps = Context scoped {
      // Bind the type parameters
      constructor.tparams.foreach { t => Context.bind(t) }
      params map resolveNonfunctionValueParam
    }

    (vps zip params) map {
      case (paramSym, paramTree) =>
        val fieldId = paramTree.id.clone
        val name = Context.nameFor(fieldId)
        val fieldSym = Field(name, paramSym, constructor, paramTree)
        if (defineAccessors) {
          Context.define(fieldId, fieldSym)
        } else {
          Context.assignSymbol(fieldId, fieldSym)
        }
        fieldSym
    }
  }

  def resolve(h: source.Handler)(using Context): Unit = h match {
    case source.Handler(capability, impl, _) =>
      resolve(impl)
      capability.foreach { param =>
        Context.bind(resolve(param))
      }
  }

  def resolve(i: source.Implementation)(using Context): Unit = Context.focusing(i) {
    case source.Implementation(interface, clauses, _) =>
      val eff: Interface = Context.at(interface) { resolveBlockRef(interface).typeConstructor.asInterface }

      clauses.foreach {
        case clause @ source.OpClause(op, tparams, vparams, bparams, ret, body, resumeId, _) => Context.at(clause) {

          // try to find the operation in the handled effect:
          eff.operations.find { o => o.name.toString == op.name } map { opSym =>
            Context.assignSymbol(op, opSym)
          } getOrElse {
            Context.abort(pretty"Operation ${op} is not part of interface ${eff}.")
          }
          Context scoped {
            val tps = tparams.map(resolve)
            val vps = vparams.map(resolve)
            val bps = bparams.map(resolve)
            Context.bindValues(vps)
            Context.bindBlocks(bps)
            Context.define(resumeId, ResumeParam(Context.module))
            resolve(body)
          }
        }
      }
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

  /**
   * Used for fields where "please wrap this in braces" is not good advice to be told by [[resolveValueType]].
   */
  def resolveNonfunctionValueParam(p: source.ValueParam)(using Context): ValueParam = {
    val sym = ValueParam(Name.local(p.id), p.tpe.map(tpe => resolveValueType(tpe, isParam = false)), decl = p)
    Context.assignSymbol(p.id, sym)
    sym
  }

  def resolve(p: source.ValueParam)(using Context): ValueParam = {
    val sym = ValueParam(Name.local(p.id), p.tpe.map(tpe => resolveValueType(tpe, isParam = true)), decl = p)
    Context.assignSymbol(p.id, sym)
    sym
  }
  def resolve(p: source.BlockParam)(using Context): BlockParam = {
    val sym: BlockParam = BlockParam(Name.local(p.id), p.tpe.map { tpe => resolveBlockType(tpe, isParam = true) }, p)
    Context.assignSymbol(p.id, sym)
    sym
  }

  def resolve(m: source.MatchClause)(using Context): Unit = Context.focusing(m) {
    case source.MatchClause(pattern, guards, body, _) =>
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

          val cs = Context.allConstructorsFor(p.name)
          if (cs.nonEmpty) {
            Context.warning(pp"Pattern binds variable ${p.name}. Maybe you meant to match on equally named constructor of type ${cs.head.tpe}?")
          }
          names = names + p.name
        }

        Context scoped {
          resolve(body)
        }
      }
  }

  /**
   * Resolve pattern matching
   *
   * Returns the value params it binds
   */
  def resolve(p: source.MatchPattern)(using Context): List[ValueParam] = p match {
    case source.IgnorePattern(_)     => Nil
    case source.LiteralPattern(lit, _) => Nil
    case source.AnyPattern(id, _) =>
      val p = ValueParam(Name.local(id), None, decl = id)
      Context.assignSymbol(id, p)
      List(p)
    case source.TagPattern(id, patterns, _) =>
      Context.resolveTerm(id) match {
        case symbol: Constructor => ()
        case _ => Context.at(id) {
          Context.error("Can only pattern match on constructors of data types.")
        }
      }
      patterns.flatMap { resolve }
    case source.MultiPattern(patterns, _) =>
      patterns.flatMap { resolve }
  }

  def resolve(p: source.MatchGuard)(using Context): Unit = p match {
    case MatchGuard.BooleanGuard(condition, _) => resolve(condition)
    case MatchGuard.PatternGuard(scrutinee, pattern, _) =>
      resolve(scrutinee)
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
  def resolveValueType(tpe: source.ValueType, isParam: Boolean = false)(using Context): ValueType = resolvingType(tpe) {
    case source.TypeRef(id, args, span) => Context.resolveType(id) match {
      case constructor: TypeConstructor => ValueTypeApp(constructor, args.unspan.map(resolveValueType))
      case id: TypeVar =>
        if (args.nonEmpty) {
          Context.abort(pretty"Type variables cannot be applied, but received ${args.size} arguments.")
        }
        ValueTypeRef(id)
      case TypeAlias(name, tparams, tpe, _) =>
        val targs = args.map(resolveValueType)
        if (tparams.size != targs.size) {
          Context.abort(pretty"Type alias ${name} expects ${tparams.size} type arguments, but got ${targs.size}.")
        }
        Substitutions.types(tparams, targs.unspan).substitute(tpe)
      case other =>
        Context.error(pretty"Expected value type, but got block type ${other}.")
        other match {
          case constructor: BlockTypeConstructor =>
            Context.info(pretty"Did you mean to box the interface ${constructor}, i.e. `${other} at {}`?")
          case _ => ()
        }

        // Dummy value type in order to aggregate more errors (see #947)
        ValueTypeApp(ErrorValueType(), Nil)
    }
    case source.ValueTypeTree(tpe, _) =>
      tpe
    // TODO reconsider reusing the same set for terms and types...
    case source.BoxedType(tpe, capt, _) =>
      BoxedType(resolveBlockType(tpe), resolve(capt))
    case other =>
      Context.error(pretty"Expected value type, but got ${describeType(other)}.")
      other match {
        case funTpe: source.FunctionType =>
          if isParam then Context.info(pretty"Did you mean to use braces in order to receive a block type `${funTpe.sourceOf}`?")
          Context.info(pretty"Did you mean to use a first-class, boxed function type `${funTpe.sourceOf} at {}`?")
        case source.Effectful(source.FunctionType(tparams, vparams, bparams, result, funEffects, _), effects, span ) =>
          val combinedEffects = prettySourceEffectSet(funEffects.effs.toSet ++ effects.effs.toSet)
          Context.info(pretty"A function type cannot have multiple effect sets, did you mean to use `/ ${combinedEffects}` instead of `/ ${funEffects.sourceOf} / ${effects.sourceOf}`?")
        case source.Effectful(source.BoxedType(tpe@source.FunctionType(tparams, vparams, bparams, result, funEffects, _), capt, _), effects, span) =>
          Context.info(pretty"Did you want to write a boxed type with effects, `${tpe.sourceOf} / ${effects.sourceOf} at ${capt.sourceOf}`?")
        case source.Effectful(innerTpe, eff, span) =>
          if isParam then Context.info(pretty"Did you mean to use braces and a function type `() => ${innerTpe.sourceOf} / ${eff.sourceOf}`?")
          Context.info(pretty"Did you mean to use a first-class, boxed type `() => ${innerTpe.sourceOf} at {} / ${eff.sourceOf}`?")
        case _ => ()
      }

      // Dummy value type in order to aggregate more errors (see #947)
      ValueTypeApp(ErrorValueType(), Nil)
  }
  def resolveValueType(tpe: source.ValueType)(using Context): ValueType = resolveValueType(tpe, isParam = false)

  def resolveBlockType(tpe: source.BlockType, isParam: Boolean = false)(using Context): BlockType = resolvingType(tpe) {
    case t: source.FunctionType  => resolve(t)
    case t: source.BlockTypeTree => t.eff
    case t: source.TypeRef => resolveBlockRef(t)
    case other =>
      Context.error(pretty"Expected block type, but got ${describeType(other)}.")
      other match
        case source.BoxedType(innerTpe, eff, _) =>
          if isParam then Context.info(pretty"Did you mean to use parentheses in order to receive a value type ${other.sourceOf}?")
          Context.info(pretty"Did you mean to use the block type ${innerTpe.sourceOf} without 'at ${eff.sourceOf}'?")
        case source.Effectful(source.FunctionType(tparams, vparams, bparams, result, funEffects, _), effects, span) =>
          val combinedEffects = prettySourceEffectSet(funEffects.effs.toSet ++ effects.effs.toSet)
          Context.info(pretty"A function type cannot have multiple effect sets, did you mean to use `/ ${combinedEffects}` instead of `/ ${funEffects.sourceOf} / ${effects.sourceOf}`?")
        case source.Effectful(source.BoxedType(tpe @ source.FunctionType(tparams, vparams, bparams, result, funEffects, _), capt, _), effects, span) =>
          Context.info(pretty"Did you want to write a boxed type with effects, `${tpe.sourceOf} / ${effects.sourceOf} at ${capt.sourceOf}`?")
        case source.Effectful(innerTpe, effs, span) =>
          // NOTE: We could use `isParam` to write a more precise message, but what exactly would it be?
          Context.info(pretty"Did you mean to use a function type () => ${innerTpe.sourceOf} / ${effs.sourceOf}?")
        case _ => ()

      // Dummy interface type in order to aggregate more errors (see #947)
      InterfaceType(ErrorBlockType(), Nil)
  }
  def resolveBlockType(tpe: source.BlockType)(using Context): BlockType = resolveBlockType(tpe, isParam = false)

  def resolve(funTpe: source.FunctionType)(using Context): FunctionType = resolvingType(funTpe) {
    /**
     * TODO share code with [[typer.Typer.makeFunctionType]]
     */
    case source.FunctionType(tparams, vparams, bparams, ret, effects, _) => Context scoped {
      val tps = tparams.map(resolve)
      val vps = vparams.map(resolveValueType)

      var cps: List[Capture] = Nil
      val bps = bparams.map {
        case (id, tpe) =>
          val name = id.map(Name.local).getOrElse(NoName)
          val cap = CaptureParam(name)
          cps = cps :+ cap
          resolveBlockType(tpe)
      }

      val effs = resolve(effects).distinct
      CanonicalOrdering(effs.toList) foreach { eff =>
        val cap = CaptureParam(eff.name)
        cps = cps :+ cap
      }

      cps foreach Context.bind

      val res = resolveValueType(ret)

      FunctionType(tps.unspan, cps, vps.unspan, bps.unspan, res, effs)
    }
  }

  def resolveBlockRef(tpe: source.TypeRef)(using Context): InterfaceType = resolvingType(tpe) { tpe =>
    resolveWithAliases(tpe) match {
      case Nil => Context.abort("Expected a single interface type, not an empty effect set.")
      case resolved :: Nil => resolved
      case _ => Context.abort("Expected a single interface type, arbitrary effect aliases are not allowed.")
    }
  }

  /**
   * Resolves a block reference into an interface type, potentially with effect aliases on the top level
   */
  def resolveWithAliases(tpe: source.TypeRef)(using Context): List[InterfaceType] = Context.at(tpe) {
    val resolved: List[InterfaceType] = tpe match {
      case source.TypeRef(id, args, span) => Context.resolveType(id) match {
        case EffectAlias(name, tparams, effs, _) =>
          if (tparams.size != args.size) {
            Context.abort(pretty"Effect alias ${name} expects ${tparams.size} type arguments, but got ${args.size}.")
          }
          val targs = args.map(resolveValueType)
          val subst = Substitutions.types(tparams, targs.unspan)
          effs.toList.map(subst.substitute)
        case i: BlockTypeConstructor => List(InterfaceType(i, args.unspan.map(resolveValueType)))
        case other =>
          Context.error(pretty"Expected an interface type, but got value type ${other}.")
          // Dummy interface type in order to aggregate more errors (see #947)
          List(InterfaceType(ErrorBlockType(), Nil))
      }
    }
    resolved.foreach(kinds.wellformed)
    resolved
  }

  def resolve(tpe: source.Effects)(using Context): Effects =
    Effects(tpe.effs.flatMap(resolveWithAliases).toSeq: _*) // TODO this otherwise is calling the wrong apply

  def resolve(e: source.Effectful)(using Context): (ValueType, Effects) =
    (resolveValueType(e.tpe), resolve(e.eff))

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

  // Used for nice error messages in Namer.resolve{Block,Value}Type.
  private def describeType(t: source.Type)(using Context): String = t match {
    case _: source.TypeRef => s"a type reference ${t.sourceOf}"
    case _: source.BoxedType => s"a boxed type ${t.sourceOf}"
    case _: source.FunctionType => s"a second-class function type ${t.sourceOf}"
    case _: source.Effectful => s"a type-and-effect annotation ${t.sourceOf}"

    // THESE TWO SHOULD NEVER BE USER-VISIBLE!
    case source.ValueTypeTree(tpe, _) => s"a value type tree ${tpe}"
    case source.BlockTypeTree(eff, _) => s"a block type tree ${eff}"
  }

  private def prettySourceEffectSet(effects: Set[source.TypeRef])(using Context) =
    effects.toList.map { _.sourceOf } match {
      case List(eff) => eff
      case Nil => "{}"
      case many => many.mkString("{", ", ", "}")
    }

  /**
   * Generate a fresh hole id that is unique in the current module.
   * Ideally, this id should be somewhat stable wrt. edits to the source code.
   * For this, we use the name of the current top-level definition, disambiguated by a counter that increments in
   * depth-first order as Namer traverses the syntax tree.
   */
  private def freshHoleId: String = {
    val prefix = parentDefinitions.value match {
      case Nil => "hole"
      case nonempty => nonempty.reverse.map(_.id.name).mkString("_")
    }
    val count = holeCount.getOrElseUpdate(prefix, 0)
    holeCount(prefix) = count + 1
    s"${prefix}${count}"
  }
}

/**
 * Environment Utils -- we use a mutable cell to express adding definitions more easily
 */
trait NamerOps extends ContextOps { Context: Context =>

  /**
   * The state of the namer phase
   */
  private var scope: Scoping = _

  private[namer] def initNamerstate(s: Scoping): Unit = {
    annotate(Annotations.HolesForFile, module.source, Nil)
    scope = s
  }

  /**
   * Override the dynamically scoped `in` to also reset namer state.
   * This is important since dependencies are resolved in a stack-like manner.
   */
  override def in[T](block: => T): T = {
    val before = scope
    val result = super.in(block)
    scope = before
    result
  }

  private[namer] def nameFor(id: IdDef): Name = scope.path match {
    case Some(path) => QualifiedName(path, id.name)
    case None => LocalName(id.name)
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

  private[namer] def bindValues(params: Many[ValueParam]): Unit =
    bindValues(params.unspan)

  private[namer] def bindBlocks(params: List[BlockParam]) =
    // bind the block parameter as a term
    params.foreach { bindBlock }

  private[namer] def bindBlocks(params: Many[BlockParam]): Unit =
    bindBlocks(params.unspan)

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
  private[namer] def resolveTerm(id: IdRef): TermSymbol = at(id) {
    val sym = scope.lookupFirstTerm(id)
    assignSymbol(id, sym)
    sym
  }

  private[namer] def addHole(h: Hole): Unit =
    val src = module.source
    val holesSoFar = annotationOption(Annotations.HolesForFile, src).getOrElse(Nil)
    annotate(Annotations.HolesForFile, src, holesSoFar :+ (h, scope.scope))

  private[namer] def allConstructorsFor(name: Name): Set[Constructor] = name match {
    case NoName => panic("Constructor needs to be named")
    case LocalName(name) => scope.allTermsFor(Nil, name).collect {
      case c: Constructor => c
    }
    case QualifiedName(prefix, name) => scope.allTermsFor(prefix, name).collect {
      case c: Constructor => c
    }
  }

  private[namer] def resolveAny(id: IdRef): Symbol = at(id) {
    val sym = scope.lookupFirst(id.path, id.name)
    assignSymbol(id, sym)
    sym
  }

  /**
   * Resolves a potentially overloaded method target
   */
  private[namer] def resolveOverloadedOperation(id: IdRef): Boolean = at(id) {
    val syms = scope.lookupOperation(id.path, id.name)

    val syms2 = if (syms.isEmpty) scope.lookupFunction(id.path, id.name) else syms

    if (syms2.nonEmpty) { assignSymbol(id, CallTarget(syms2.asInstanceOf)); true } else { false }
  }

  private[namer] def resolveOverloadedFunction(id: IdRef): Boolean = at(id) {
    val syms = scope.lookupFunction(id.path, id.name)

    val syms2 = if (syms.isEmpty) scope.lookupOperation(id.path, id.name) else syms

    // lookup first block param and do not collect multiple since we do not (yet?) permit overloading on block parameters
    val syms3 = if (syms2.isEmpty) List(scope.lookupFirstBlockParam(id.path, id.name)) else syms2

    if (syms3.nonEmpty) { assignSymbol(id, CallTarget(syms3.asInstanceOf)); true } else { false }
  }

  /**
   * Resolves a potentially overloaded function call
   */
  private[namer] def resolveFunctionCalltarget(id: IdRef): Unit = at(id) {
    val candidates = scope.lookupOverloaded(id, term => !term.isInstanceOf[Operation])

    resolveFunctionCalltarget(id, candidates) match {
      case Left(value) =>
        assignSymbol(id, value)
      case Right(blocks) =>
        if (blocks.isEmpty) {
          val ops = scope.lookupOperation(id.path, id.name).flatten

          // Provide specific info messages for operations
          ops.foreach { op =>
            info(pretty"There is an equally named effect operation ${op} of interface ${op.interface}. Use syntax `do ${id}()` to call it.")
          }

          // Always abort with the generic message
          abort(pretty"Cannot find a function named `${id}`.")
        }
        assignSymbol(id, CallTarget(blocks))
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
  @tailrec
  private def resolveFunctionCalltarget(id: IdRef, candidates: List[Set[TermSymbol]]): Either[TermSymbol, List[Set[BlockSymbol]]] =

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
  private[namer] def resolveSelect(id: IdRef): Unit = at(id) {
    val syms = scope.lookupOverloaded(id, term => term.isInstanceOf[Field])

    if (syms.isEmpty) {
      abort(pretty"Cannot resolve field access ${id}")
    }

    assignSymbol(id, CallTarget(syms.asInstanceOf))
  }

  /**
   * Resolves a potentially overloaded call to an effect
   */
  private[namer] def resolveEffectCall(id: IdRef): Unit = at(id) {

    val syms = scope.lookupOperation(id.path, id.name)

    if (syms.isEmpty) {
      abort(pretty"Cannot resolve effect operation ${id}")
    }

    assignSymbol(id, CallTarget(syms.asInstanceOf))
  }

  /**
   * Variables have to be resolved uniquely
   */
  private[namer] def resolveVar(id: IdRef): TermSymbol = resolveTerm(id) match {
    case b: BlockParam => b // abort("Blocks have to be fully applied and can't be used as values.")
    case other         => other
  }

  private[namer] def resolveType(id: IdRef): TypeSymbol = at(id) {
    val sym = scope.lookupType(id)
    assignSymbol(id, sym)
    sym
  }

  private[namer] def resolveCapture(id: IdRef): Capture = at(id) {
    val sym = scope.lookupCapture(id)
    assignSymbol(id, sym)
    sym
  }

  private[namer] def scoped[R](block: => R): R = Context in {
    scope.scoped { block }
  }

  private[namer] def scopedWithName[R](name: String)(block: => R): R = Context in {
    scope.scoped(name, block)
  }

  private[namer] def namespace[R](name: String)(block: => R): R = Context in {
    scope.namespace(name) { block }
  }
}
