package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.context.assertions._
import effekt.source.{ AnyPattern, Def, DefStmt, IgnorePattern, MatchPattern, ModuleDecl, Stmt, TagPattern, Term, Tree }
import effekt.substitutions._
import effekt.symbols._
import effekt.symbols.builtins._
import effekt.symbols.kinds._
import effekt.util.messages.FatalPhaseError
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages

/**
 * Typechecking
 * ============
 *
 * Preconditions:
 * --------------
 * Typer assumes that all dependencies already have been type checked.
 * In particular, it assumes that all definitions / symbols (functions, parameters etc.)
 * have been annotated with a type: this models a (global) typing context.
 *
 * Postconditions:
 * ---------------
 * All trees will be annotated with intermediate types (and effects). This is useful for
 * IDE support.
 * Also, after type checking, all definitions of the file will be annotated with their type.
 */
case class TyperResult[+T](tpe: T, capt: CaptureSet)
object / {
  def unapply[T](t: TyperResult[T]): Option[(T, CaptureSet)] = Some((t.tpe, t.capt))
}
object TyperResult {
  implicit class TypeOps[T](tpe: T) {
    def /(capt: CaptureSet): TyperResult[T] = TyperResult(tpe, capt)
  }
}
import TyperResult._

class Typer extends Phase[ModuleDecl, ModuleDecl] {

  val phaseName = "typer"

  def run(module: ModuleDecl)(implicit C: Context): Option[ModuleDecl] = try {
    val mod = Context.module

    Context.initTyperstate()

    Context in {
      Context.withUnificationScope {
        // We split the type-checking of definitions into "pre-check" and "check"
        // to allow mutually recursive defs
        module.defs.foreach { d => precheckDef(d) }
        module.defs.foreach { d => checkStmts(source.DefStmt(d, source.Return(source.UnitLit()))) }
        TUnit / Pure
      }
    }

    if (C.buffer.hasErrors) {
      None
    } else {
      Some(module)
    }
  } finally {
    // Store the backtrackable annotations into the global DB
    // This is done regardless of errors, since
    Context.commitTypeAnnotations()

  }

  // checks an expression in second-class position
  //<editor-fold desc="blocks">

  def insertBoxing(expr: Term)(implicit C: Context): TyperResult[ValueType] =
    Context.abort(s"Right now blocks cannot be used as expressions.")

  def insertUnboxing(expr: Term)(implicit C: Context): TyperResult[BlockType] =
    Context.abort(s"Currently expressions cannot be used as blocks.")

  /**
   * We defer checking whether something is first-class or second-class to Typer now.
   */
  def checkExprAsBlock(expr: Term)(implicit C: Context): TyperResult[BlockType] =
    checkBlock(expr) {
      case source.Unbox(expr) =>
        val vtpe / capt1 = checkExpr(expr)
        // TODO here we also need unification variables for block types!
        // C.unify(tpe, BoxedType())
        vtpe match {
          case BoxedType(btpe, capt2) => btpe / (capt1 ++ capt2)
          case _ => Context.abort(s"Unbox requires a boxed type, but got $vtpe")
        }

      case source.Var(id) => id.symbol match {
        case b: BlockSymbol =>
          val (tpe, capt) = Context.lookup(b)
          tpe / capt
        case e: ValueSymbol => insertUnboxing(expr)
      }

      case s @ source.Select(expr, selector) =>
        checkExprAsBlock(expr) match {
          case (i @ InterfaceType(interface, targs) / capt) =>
            // (1) find the operation
            // try to find an operation with name "selector"
            val op = interface.ops.collect {
              case op if op.name.name == selector.name => op
            } match {
              case Nil      => Context.at(s) { Context.abort(s"Cannot select ${selector} in type ${i}") }
              case List(op) => op
              case _        => Context.at(s) { Context.abort(s"Multiple operations match ${selector} in type ${i}") }
            }
            // assign the resolved operation to the identifier
            Context.assignSymbol(selector, op)

            // (2) substitute type arguments
            val tsubst = (interface.tparams zip targs).toMap
            tsubst.substitute(op.toType) / capt

          case _ => Context.abort(s"Selection requires an interface type.")
        }

      case _ => Context.abort(s"Expected something of a block type.")
    }

  //</editor-fold>

  //<editor-fold desc="expressions">

  def checkExpr(expr: Term)(implicit C: Context): TyperResult[ValueType] =
    check(expr) {
      case source.IntLit(n)     => TInt / Pure
      case source.BooleanLit(n) => TBoolean / Pure
      case source.UnitLit()     => TUnit / Pure
      case source.DoubleLit(n)  => TDouble / Pure
      case source.StringLit(s)  => TString / Pure

      case source.If(cond, thn, els) =>
        val cndTpe / cndCapt = cond checkAgainst TBoolean
        val thnTpe / thnCapt = checkStmt(thn)
        val elsTpe / elsCapt = checkStmt(els)

        Context.unify(thnTpe, elsTpe)

        thnTpe / (cndCapt ++ thnCapt ++ elsCapt)

      case source.While(cond, block) =>
        val _ / cndCapt = cond checkAgainst TBoolean
        val _ / blkCapt = block checkAgainst TUnit
        TUnit / (cndCapt ++ blkCapt)

      // the variable now can also be a block variable
      case source.Var(id) => id.symbol match {
        case b: VarBinder => Context.lookup(b) match {
          case (BlockTypeApp(TState, List(tpe)), capt) => tpe / capt
          case _ => Context.panic(s"Builtin state cannot be typed.")
        }
        case b: BlockSymbol => insertBoxing(expr)
        case x: ValueSymbol => Context.lookup(x) / Pure
      }

      case e @ source.Assign(id, expr) =>
        // assert that it is a mutable variable
        val sym = e.definition.asVarBinder
        val stTpe / stCapt = Context.lookup(sym) match {
          case (BlockTypeApp(TState, List(tpe)), capt) => tpe / capt
          case _ => Context.panic(s"Builtin state cannot be typed.")
        }
        val _ / exprCapt = expr checkAgainst stTpe

        TUnit / (stCapt ++ exprCapt)

      case source.Box(annotatedCapt, block) =>
        // by introducing a unification scope here, we know that `capt` cannot contain fresh unification variables.
        val inferredTpe / inferredCapt = Context.at(block) { Context withUnificationScope { checkBlockArgument(block) } }

        // box { ... }  ~>  box ?C { ... }
        val capt = annotatedCapt.map { c =>
          val expected = c.resolve
          C.sub(inferredCapt, expected)
          expected
        }.getOrElse(inferredCapt)

        // If there is no annotated capture set, we simply use the inferred one. This might lead to type errors
        // up higher.
        BoxedType(inferredTpe, capt) / Pure

      case source.Unbox(_) => insertBoxing(expr)

      case c @ source.Call(e, targsTree, vargs, bargs) =>
        val funTpe / funCapt = checkExprAsBlock(e) match {
          case TyperResult(b: FunctionType, capt) => b / capt
          case _ => Context.abort("Callee is required to have function type")
        }

        val targs = targsTree map { _.resolve }

        // (1) Instantiate blocktype
        // e.g. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
        // TODO: do something about capture params
        val (trigids, crigids, bt @ FunctionType(_, _, vparams, bparams, ret)) = Context.instantiate(funTpe)

        // (2) Wellformedness -- check arity
        if (targs.nonEmpty && targs.size != trigids.size)
          Context.abort(s"Wrong number of type arguments ${targs.size}")

        if (vparams.size != vargs.size)
          Context.error(s"Wrong number of value arguments, given ${vargs.size}, but function expects ${vparams.size}.")

        if (bparams.size != bargs.size)
          Context.error(s"Wrong number of block arguments, given ${bargs.size}, but function expects ${bparams.size}.")

        // (3) Unify with provided type arguments, if any.
        if (targs.nonEmpty) {
          (trigids zip targs) map { case (r, a) => Context.unify(r, a) }
        }

        var argCapt = Pure
        (vparams zip vargs) foreach {
          case (paramType, arg) =>
            val _ / capt = arg checkAgainst paramType
            argCapt ++= capt
        }
        (bparams zip bargs zip crigids) foreach {
          case ((paramType, arg), cvar) =>
            val got / capt = checkBlockArgument(arg)
            C.unify(paramType, got)

            // here we use unify, not sub since this really models substitution
            C.unify(capt, cvar)
            argCapt ++= capt
        }

        ret / (funCapt ++ argCapt)

      case source.TryHandle(prog, handlers) =>

        val capabilityParams = handlers.map { h => h.capability.symbol }

        // (1) create new unification scope and check handled program (`prog`) with capabilities in scope
        val ret / bodyCapt = Context.at(prog) {
          Context.withUnificationScope {
            // bind capability types in type environment
            capabilityParams foreach Context.bind
            checkStmt(prog)
          }
        }

        val boundCaptures: Set[Capture] = capabilityParams.map(CaptureOf).toSet

        // subtract capability from inferred capture Cp. Outer unification variables cannot possibly contain the fresh capability.
        val bodyCaptWithoutCapabilities = bodyCapt -- CaptureSet(boundCaptures)

        // check that none of the bound capabilities escapes through the return type
        val escape = freeCapture(ret) intersect boundCaptures
        if (escape.nonEmpty) {
          C.at(prog) { C.error(s"The return type is not allowed to refer to any of the bound capabilities, but mentions: ${CaptureSet(escape)}") }
        }

        // Create a new unification scope and introduce a fresh capture variable for the continuations ?Ck
        Context.withUnificationScope {

          var handlerCapt = Pure

          // the capture variable for the continuation ?Ck
          val resumeCapture = C.freshCaptVar(CaptureParam(LocalName("$resume")))

          // Check all handler bodies and collect all inferred capture sets Ch
          handlers foreach Context.withFocus { h =>
            // try { ... } with s: >>>State[Int]<<< { ... }
            val annotatedType @ InterfaceType(interface, targs) = h.capability.symbol.tpe.asInterfaceType

            val tparams = interface.tparams
            val tsubst = (tparams zip targs).toMap

            // (3) check all operations are covered
            val covered = h.clauses.map { _.definition }
            val notCovered = interface.ops.toSet -- covered.toSet

            if (notCovered.nonEmpty) {
              val explanation = notCovered.map { op => s"${op.name} of interface ${op.effect.name}" }.mkString(", ")
              Context.error(s"Missing definitions for operations: ${explanation}")
            }

            if (covered.size > covered.distinct.size)
              Context.error(s"Duplicate definitions of operations")

            // (4) actually check each clause
            h.clauses foreach Context.withFocus {
              // TODO what is with type parameters of operation clauses?
              case d @ source.OpClause(op, tparams, vparams, body, resume) =>

                val declaration = d.definition

                // (4a) the effect operation might refer to type parameters of the interface
                //   i.e. interface Foo[A] { def bar[B](a: A): B }
                //
                // at the handle site, we might have
                //   try { ... } with f: Foo[Int] { def bar[C](a: Int): C }
                //
                // So as a first step, we need to obtain the function type of the declaration bar:
                //   bar: [B](a: A) -> B
                // and substitute { A -> Int}
                //   barSubstituted: [B](a: Int) -> B
                // TODO: do something about capture set
                val FunctionType(tparams1, cparams1, vparams1, bparams1, ret1) = tsubst.substitute(declaration.toType)

                // (4b) check the body of the clause
                val tparamSyms = tparams.map { t => t.symbol.asTypeVar }
                val vparamSyms = vparams.map { p => p.symbol }

                vparamSyms foreach Context.bind

                // TODO add constraints on resume capture
                val resumeType = FunctionType(Nil, Nil, List(ret1), Nil, ret)
                val resumeSym = Context.symbolOf(resume).asBlockSymbol

                Context.bind(resumeSym, resumeType, CaptureSet(resumeCapture))

                // TODO does this need to be a fresh unification scope?
                val opRet / opCapt = Context in { body checkAgainst ret }
                handlerCapt ++= opCapt

                // (4c) Note that the expected type is NOT the declared type but has to take the answer type into account
                /** TODO: do something about capture set parameters */
                val inferredTpe = FunctionType(tparamSyms, Nil, vparamSyms.map { Context.lookup }, Nil, opRet)
                val expectedTpe = FunctionType(tparams1, cparams1, vparams1, bparams1, ret)

                Context.unify(inferredTpe, expectedTpe)
            }
          }

          // Ck = (Cp - cap) union (UNION Ch)
          val residualCapt = bodyCaptWithoutCapabilities ++ handlerCapt
          C.unify(CaptureSet(resumeCapture), residualCapt)

          ret / residualCapt
        }

      case source.Match(sc, clauses) =>

        // (1) Check scrutinee
        // for example. tpe = List[Int]
        val scTpe / scCapt = Context.withUnificationScope { checkExpr(sc) }

        var capt = scCapt

        // (2) check exhaustivity
        checkExhaustivity(scTpe, clauses.map { _.pattern })

        // (3) infer types for all clauses
        // TODO here we would need multi arity constraints!
        val (firstTpe / firstCapt, firstTree) :: clauseTpes = clauses.map {
          case c @ source.MatchClause(p, body) =>
            val res = Context.withUnificationScope {
              Context in {
                Context.bind(checkPattern(scTpe, p))
                checkStmt(body)
              }
            }
            (res, body)
        }

        capt ++= firstCapt

        // (4) unify clauses and collect effects
        clauseTpes foreach {
          case (clauseTpe / clauseCapt, tree) =>
            capt ++= clauseCapt
            Context.at(tree) { Context.unify(firstTpe, clauseTpe) }
        }

        firstTpe / capt

      case source.Select(_, _) =>
        insertBoxing(expr)

      case source.Hole(stmt) =>
        checkStmt(stmt)
        THole / Pure
    }

  //</editor-fold>

  //<editor-fold desc="pattern matching">

  /**
   * This is a quick and dirty implementation of coverage checking. Both performance, and error reporting
   * can be improved a lot.
   */
  def checkExhaustivity(sc: ValueType, cls: List[MatchPattern])(implicit C: Context): Unit = {
    val catchall = cls.exists { p => p.isInstanceOf[AnyPattern] || p.isInstanceOf[IgnorePattern] }

    if (catchall)
      return ;

    sc match {
      case TypeConstructor(t: DataType) =>
        t.variants.foreach { variant =>
          checkExhaustivity(variant, cls)
        }

      case TypeConstructor(t: Record) =>
        val (related, unrelated) = cls.collect { case p: TagPattern => p }.partitionMap {
          case p if p.definition == t => Left(p.patterns)
          case p => Right(p)
        }

        if (related.isEmpty) {
          Context.error(s"Non exhaustive pattern matching, missing case for ${sc}")
        }

        (t.fields.map { f => f.tpe } zip related.transpose) foreach {
          case (t, ps) => checkExhaustivity(t, ps)
        }
      case other =>
        ()
    }
  }

  def checkPattern(sc: ValueType, pattern: MatchPattern)(implicit C: Context): Map[Symbol, ValueType] = Context.focusing(pattern) {
    case source.IgnorePattern()    => Map.empty
    case p @ source.AnyPattern(id) => Map(p.symbol -> sc)
    case p @ source.LiteralPattern(lit) =>
      lit.checkAgainst(sc)
      Map.empty
    case p @ source.TagPattern(id, patterns) =>

      // symbol of the constructor we match against
      val sym: Record = Context.symbolOf(id) match {
        case c: Record => c
        case _         => Context.abort("Can only match on constructors")
      }

      // (4) Compute blocktype of this constructor with rigid type vars
      // i.e. Cons : `(?t1, List[?t1]) => List[?t1]`
      // constructors can't take block parameters, so we can ignore them safely
      val (trigids, crigids, FunctionType(_, _, vpms, _, ret)) = Context.instantiate(sym.toType)

      // (5) given a scrutinee of `List[Int]`, we learn `?t1 -> Int`
      Context.unify(ret, sc)
      //
      //      // (6) check for existential type variables
      //      // at the moment we do not allow existential type parameters on constructors.
      //      val skolems = Context.skolems(rigids)
      //      if (skolems.nonEmpty) {
      //        Context.error(s"Unbound type variables in constructor ${id}: ${skolems.map(_.underlying).mkString(", ")}")
      //      }

      //        // (7) refine parameter types of constructor
      //        // i.e. `(Int, List[Int])`
      //        val constructorParams = vpms map { p => Context.unifier substitute p }

      // (8) check nested patterns
      var bindings = Map.empty[Symbol, ValueType]

      (patterns, vpms) match {
        case (pats, pars) =>
          if (pats.size != pars.size)
            Context.error(s"Wrong number of pattern arguments, given ${pats.size}, expected ${pars.size}.")

          (pats zip pars) foreach {
            case (pat, par: ValueType) =>
              bindings ++= checkPattern(par, pat)
            case _ =>
              Context.panic("Should not happen, since constructors can only take value parameters")
          }
      }
      bindings
  }

  //</editor-fold>

  //<editor-fold desc="statements and definitions">

  def checkStmt(stmt: Stmt)(implicit C: Context): TyperResult[ValueType] = {
    precheckStmts(stmt);
    checkStmts(stmt)
  }

  def precheckStmts(stmt: Stmt)(implicit C: Context): Unit = stmt match {
    case source.DefStmt(d, rest) =>
      precheckDef(d); precheckStmts(rest)
    case source.ExprStmt(d, rest) => precheckStmts(rest)
    case source.BlockStmt(s)      => ()
    case source.Return(e)         => ()
  }

  def checkStmts(stmt: Stmt)(implicit C: Context): TyperResult[ValueType] =
    check(stmt) {
      case source.DefStmt(d @ source.FunDef(id, tparams, vparams, bparams, ret, body), rest) =>
        val sym = d.symbol
        sym.vparams foreach Context.bind
        sym.bparams foreach Context.bind

        val precheckedCapt = C.lookupCapture(sym)

        val lexical = CaptureOf(sym)

        val tpe / capt = Context.at(body) {
          Context.withRegion(lexical) {
            Context.withUnificationScope {
              sym.ret match {
                case Some(tpe) => body checkAgainst tpe
                case None      => checkStmt(body)
              }
            }
          }
        }

        if (freeCapture(tpe) contains lexical) Context.at(body) { Context.error(s"Self region ${id} must not escape as part of the function's return type") }

        // TODO check whether the subtraction here works in presence of unification variables
        val captWithoutBoundParams = capt -- CaptureSet(sym.bparams.map(CaptureOf)) -- CaptureSet(lexical)

        Context.bind(sym, sym.toType(tpe), captWithoutBoundParams)
        Context.annotateInferredType(d, tpe)
        Context.annotateInferredCapt(d, captWithoutBoundParams)

        // since we do not have capture annotations for now, we do not need subsumption here and this is really equality
        C.unify(captWithoutBoundParams, precheckedCapt)

        checkStmts(rest)

      case source.DefStmt(d @ source.ValDef(id, annot, binding), rest) =>
        val tpeBind / captBind = d.symbol.tpe match {
          case Some(t) =>
            binding checkAgainst t
          case None => checkStmt(binding)
        }
        Context.bind(d.symbol, tpeBind)

        val tpeRest / captRest = checkStmts(rest)

        tpeRest / (captBind ++ captRest)

      case source.DefStmt(d @ source.VarDef(id, annot, binding), rest) =>
        val sym = d.symbol

        // TODO do not ignore the capture set here!
        val tpeBind / captBind = sym.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding)
        }
        val stTpe = BlockTypeApp(TState, List(tpeBind))

        // we use the current region as an approximation for the state
        val captState = C.region // CaptureOf(sym)

        Context.bind(sym, stTpe, CaptureSet(captState)) // TODO use correct capture set here

        val tpeRest / captRest = checkStmts(rest)

        // TODO check non escaping of sym
        tpeRest / (captBind ++ captRest)

      case source.DefStmt(d @ source.ExternFun(pure, id, tparams, vparams, tpe, body), rest) =>
        d.symbol.vparams map { p => Context.bind(p) }
        checkStmts(rest)

      // All other defs have already been prechecked
      case source.DefStmt(_, rest) =>
        checkStmts(rest)

      // <expr> ; <stmt>
      case source.ExprStmt(e, rest) =>
        val _ / captExpr = checkExpr(e)
        val tpe / captRest = checkStmts(rest)
        tpe / (captExpr ++ captRest)

      case source.Return(e)        => checkExpr(e)

      case source.BlockStmt(stmts) => checkStmt(stmts)
    }

  // not really checking, only if defs are fully annotated, we add them to the typeDB
  // this is necessary for mutually recursive definitions
  //
  // we also need to create fresh capture variables to collect constraints
  def precheckDef(d: Def)(implicit C: Context): Unit = Context.focusing(d) {
    case d @ source.FunDef(id, tparams, vparams, bparams, ret, body) =>
      val funSym = d.symbol
      val funCapt = CaptureSet(C.freshCaptVar(CaptureOf(funSym)))
      Context.bind(funSym, funCapt)
      funSym.ret.foreach { annot => Context.bind(d.symbol, d.symbol.toType) }

    case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
      Context.bind(d.symbol, d.symbol.toType, Pure)

    case d @ source.InterfaceDef(id, tparams, ops) =>
      d.symbol.ops.foreach { op =>
        val tpe = op.toType
        wellformed(tpe)
        Context.bind(op, tpe, Pure)
      }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.symbol
        Context.bind(sym, sym.toType, Pure)

        sym.fields.foreach { field =>
          val tpe = field.toType
          wellformed(tpe)
          Context.bind(field, tpe, Pure)
        }
      }

    case _ => ()
  }

  //</editor-fold>

  //<editor-fold desc="arguments and parameters">

  def checkBlockArgument(arg: source.BlockArg)(implicit C: Context): TyperResult[BlockType] = checkBlock(arg) {
    case arg: source.FunctionArg  => checkFunctionArgument(arg)
    case arg: source.InterfaceArg => checkExprAsBlock(source.Var(arg.id).inheritPosition(arg))
    case source.UnboxArg(expr) =>
      val tpe / outerCapt = checkExpr(expr)
      // TODO this is a conservative approximation:
      //    the capture of the expr should be part of the capture of the call, but not the block argument!
      tpe match {
        case BoxedType(btpe, capt) => btpe / (capt ++ outerCapt)
        case _                     => C.abort(s"Unboxing requires a boxed type, but got $tpe")
      }
  }

  // Example.
  //   BlockParam: def foo { f: Int => String / Print }
  //   BlockArg: foo { n => println("hello" + n) }
  //     or
  //   BlockArg: foo { (n: Int) => println("hello" + n) }
  def checkFunctionArgument(arg: source.FunctionArg)(implicit C: Context): TyperResult[FunctionType] = arg match {
    case decl @ source.FunctionArg(tparams, vparams, bparams, body) =>

      // A synthetic symbol to identify the anonymous function
      val funSym = Anon(decl)

      val tparamSymbols = tparams.map { p => p.symbol.asTypeVar }
      tparamSymbols.foreach { p => Context.bind(p, p) }
      vparams.foreach { p => Context.bind(p.symbol) }
      bparams.foreach { p => Context.bind(p.symbol) }
      val capts = bparams.map { p => CaptureOf(p.symbol) }

      val lexical = CaptureOf(funSym)

      // TODO should we open a new unification scope here?
      val ret / capt = Context.withRegion(lexical) { checkStmt(body) }

      if (freeCapture(ret) contains lexical) { Context.at(body) { Context.error("The self region of the anonymous block argument must not leave through its type.") } }

      val capture = capt -- CaptureSet(capts) -- CaptureSet(lexical)

      FunctionType(tparamSymbols, capts, vparams.map { p => p.symbol.tpe }, bparams.map { p => p.symbol.tpe }, ret) / capture
  }

  //</editor-fold>

  private def freeCapture(o: Any): Set[Capture] = o match {
    case t: symbols.Capture   => Set(t)
    case BoxedType(tpe, capt) => freeCapture(tpe) ++ capt.captures
    case FunctionType(tparams, cparams, vparams, bparams, ret) =>
      freeCapture(vparams) ++ freeCapture(bparams) ++ freeCapture(ret) -- cparams.toSet
    // case e: Effects            => freeTypeVars(e.toList)
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[Capture]) { case (r, t) => r ++ freeCapture(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[Capture]) { case (r, t) => r ++ freeCapture(t) }
    case _ =>
      Set.empty
  }

  private implicit class ExprOps(expr: Term) {
    def checkAgainst(tpe: ValueType)(implicit C: Context): TyperResult[ValueType] = Context.at(expr) {
      val got / capt = checkExpr(expr)
      C.unify(tpe, got)
      tpe / capt
    }
  }

  private implicit class StmtOps(stmt: Stmt) {
    def checkAgainst(tpe: ValueType)(implicit C: Context): TyperResult[ValueType] = Context.at(stmt) {
      val got / capt = checkStmt(stmt)
      C.unify(tpe, got)
      tpe / capt
    }
  }

  /**
   * Combinators that also store the computed type for a tree in the TypesDB
   */
  def check[T <: Tree](t: T)(f: T => TyperResult[ValueType])(implicit C: Context): TyperResult[ValueType] =
    Context.at(t) {
      val got / capt = f(t)
      wellformed(got)
      C.annotateInferredType(t, got)
      C.annotateInferredCapt(t, capt)
      got / capt
    }

  def checkBlock[T <: Tree](t: T)(f: T => TyperResult[BlockType])(implicit C: Context): TyperResult[BlockType] =
    Context.at(t) {
      val got / capt = f(t)
      wellformed(got)
      C.annotateInferredType(t, got)
      C.annotateInferredCapt(t, capt)
      got / capt
    }
}

trait TyperOps extends ContextOps { self: Context =>

  /**
   * The current unification Scope
   */
  private var scope: UnificationScope = new UnificationScope

  /**
   * The substitutions learnt so far
   */
  private var substitutions: Substitutions = Substitutions.empty

  /**
   * We need to substitute after solving and update the DB again, later.
   */
  private var inferredValueTypes: List[(Tree, ValueType)] = Nil
  private var inferredBlockTypes: List[(Tree, BlockType)] = Nil
  private var inferredCaptures: List[(Tree, CaptureSet)] = Nil

  /**
   * The current lexical region used for mutable variables.
   *
   * None on the toplevel
   */
  private var lexicalRegion: Option[Capture] = None

  private[typer] def initTyperstate(): Unit = {
    scope = new UnificationScope
    inferredValueTypes = List.empty
    inferredBlockTypes = List.empty
    inferredCaptures = List.empty
    valueTypingContext = Map.empty
    blockTypingContext = Map.empty
    substitutions = Substitutions.empty
  }

  private[typer] def commitTypeAnnotations(): Unit = {
    // now also store the typing context in the global database:
    valueTypingContext foreach { case (s, tpe) => assignType(s, tpe) }
    blockTypingContext foreach { case (s, tpe) => assignType(s, tpe) }
    captureContext foreach { case (s, c) => assignCaptureSet(s, c) }

    // Update and write out all inferred types and captures for LSP support
    // This info is currently also used by Transformer!
    inferredValueTypes foreach { case (t, tpe) => annotate(Annotations.InferredValueType, t, subst.substitute(tpe)) }
    inferredBlockTypes foreach { case (t, tpe) => annotate(Annotations.InferredBlockType, t, subst.substitute(tpe)) }
    inferredCaptures foreach { case (t, capt) => annotate(Annotations.InferredCapture, t, subst.substitute(capt)) }
  }

  // Unification
  // ===========

  // opens a fresh unification scope
  private[typer] def withUnificationScope[T <: Type](block: => TyperResult[T]): TyperResult[T] = {
    val outerScope = scope
    scope = new UnificationScope
    val tpe / capt = block
    // leaving scope: solve here and check all are local unification variables are defined...
    val (subst, cs, ccs) = scope.solve

    // TODO applying the substitution to the environment is not enough! We also need to substitute into types and captures
    // that are locally in scope in Typer...

    // The unification variables now go out of scope:
    // use the new substitution to update the defined symbols (typing context) and inferred types (annotated trees).
    valueTypingContext = valueTypingContext.view.mapValues { t => subst.substitute(t) }.toMap
    blockTypingContext = blockTypingContext.view.mapValues { t => subst.substitute(t) }.toMap
    captureContext = captureContext.view.mapValues { c => subst.substitute(c) }.toMap
    substitutions = substitutions.updateWith(subst)

    outerScope.addAllType(cs)
    outerScope.addAllCapt(ccs)
    scope = outerScope

    tpe match {
      case t: ValueType => subst.substitute(t).asInstanceOf[T] / subst.substitute(capt)
      case t: BlockType => subst.substitute(t).asInstanceOf[T] / subst.substitute(capt)
    }
  }

  def unify(t1: ValueType, t2: ValueType) = scope.requireEqual(t1, t2)
  def unify(t1: BlockType, t2: BlockType) = scope.requireEqual(t1, t2)
  def unify(c1: CaptureSet, c2: CaptureSet) = scope.requireEqual(c1, c2)

  def sub(c1: CaptureSet, c2: CaptureSet) = scope.requireSub(c1, c2)

  def subst: Substitutions = substitutions

  // these are different methods because those are constraints that we like to introduce
  // might help refactoring later
  def unify(c1: CaptureSet, c2: Capture): Unit = unify(c1, CaptureSet(Set(c2)))
  def sub(c1: CaptureSet, c2: Capture): Unit = sub(c1, CaptureSet(Set(c2)))

  def instantiate(tpe: FunctionType) = scope.instantiate(tpe)

  // TODO this is only for synthesized capture sets (i.e. when box isn't annotated)
  def freshCaptVar() = scope.freshCaptVar(CaptureParam(NoName))
  def freshCaptVar(underlying: Capture) = scope.freshCaptVar(underlying)

  // Inferred types
  // ==============

  private[typer] def annotateInferredType(t: Tree, e: ValueType) = inferredValueTypes = (t -> e) :: inferredValueTypes
  private[typer] def annotateInferredType(t: Tree, e: BlockType) = inferredBlockTypes = (t -> e) :: inferredBlockTypes
  private[typer] def annotateInferredCapt(t: Tree, e: CaptureSet) = inferredCaptures = (t -> e) :: inferredCaptures

  // The "Typing Context"
  // ====================
  // since symbols are unique, we can use mutable state instead of reader
  private var valueTypingContext: Map[Symbol, ValueType] = Map.empty
  private var blockTypingContext: Map[Symbol, BlockType] = Map.empty
  private var captureContext: Map[Symbol, CaptureSet] = Map.empty

  // first tries to find the type in the local typing context
  // if not found, it tries the global DB, since it might be a symbol of an already checked dependency
  private[typer] def lookup(s: ValueSymbol) =
    valueTypingContext.getOrElse(s, valueTypeOf(s))

  private[typer] def lookup(s: BlockSymbol) = (lookupType(s), lookupCapture(s))

  private[typer] def lookupType(s: BlockSymbol) =
    blockTypingContext.getOrElse(s, blockTypeOf(s))

  private[typer] def lookupCapture(s: BlockSymbol) =
    captureContext.getOrElse(s, captureOf(s))

  private[typer] def bind(s: Symbol, tpe: ValueType): Unit = valueTypingContext += (s -> tpe)

  private[typer] def bind(s: Symbol, tpe: BlockType, capt: CaptureSet): Unit = { bind(s, tpe); bind(s, capt) }

  private[typer] def bind(s: Symbol, tpe: BlockType): Unit = blockTypingContext += (s -> tpe)

  private[typer] def bind(s: Symbol, capt: CaptureSet): Unit = captureContext += (s -> capt)

  private[typer] def bind(bs: Map[Symbol, ValueType]): Unit =
    bs foreach {
      case (v: ValueSymbol, t: ValueType) => bind(v, t)
      //        case (v: BlockSymbol, t: FunctionType) => bind(v, t)
      case other => panic(s"Internal Error: wrong combination of symbols and types: ${other}")
    }

  private[typer] def bind(p: ValueParam): Unit = p match {
    case s @ ValueParam(name, tpe) => bind(s, tpe)
    case s => panic(s"Internal Error: Cannot add $s to typing context.")
  }

  private[typer] def bind(p: BlockParam): Unit = p match {
    case s @ BlockParam(name, tpe) => bind(s, tpe, CaptureSet(CaptureOf(s)))
    case s => panic(s"Internal Error: Cannot add $s to typing context.")
  }

  // Lexical Regions
  // ===============
  def region: Capture = lexicalRegion.getOrElse(abort("Mutable variables are not allowed outside of a function definition"))
  def withRegion[T](c: Capture)(prog: => T): T = {
    val before = lexicalRegion
    lexicalRegion = Some(c)
    val res = prog
    lexicalRegion = before
    res
  }
}
