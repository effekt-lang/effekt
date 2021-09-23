package effekt
package typer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.context.assertions._
import effekt.source.{ AnyPattern, Def, Term, IgnorePattern, MatchPattern, ModuleDecl, Stmt, TagPattern, Tree }
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
        module.defs.foreach { d => checkDef(d) }
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
        case e: ValueSymbol => Context.abort(s"Currently expressions cannot be used as blocks.")
      }

      case source.Select(expr, selector) =>
        checkExprAsBlock(expr) match {
          case (i @ InterfaceType(interface, targs) / capt) =>
            // (1) find the operation
            // try to find an operation with name "selector"
            val op = interface.ops.collect {
              case op if op.name.name == selector.name => op
            } match {
              case Nil      => Context.abort(s"Cannot select ${selector} in type ${i}")
              case List(op) => op
              case _        => Context.abort(s"Multiple operations match ${selector} in type ${i}")
            }

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
        case b: VarBinder   => Context.abort(s"Mutable variables not yet implemented.")
        case b: BlockSymbol => Context.abort(s"Right now blocks cannot be used as expressions.")
        case x: ValueSymbol => Context.lookup(x) / Pure
      }

      case e @ source.Assign(id, expr) =>
        // assert that it is a mutable variable
        val sym = e.definition.asVarBinder
        val _ / capt = expr checkAgainst Context.lookup(sym)

        TUnit / (capt + CaptureOf(sym))

      case source.Box(annotatedCapt, block) =>
        // by introducing a unification scope here, we know that `capt` cannot contain fresh unification variables.
        val tpe / capt = Context withUnificationScope { checkBlockArgument(block) }
        // box { ... }  ~>  box ?C { ... }
        val expectedCapt = annotatedCapt.map(c => c.resolve).getOrElse(CaptureSet(Set(C.freshCaptVar())))
        C.sub(capt, expectedCapt)
        BoxedType(tpe, expectedCapt) / Pure

      case source.Unbox(expr) =>
        Context.abort("Block in expression position: automatic boxing currently not supported.")

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
            val _ / capt = arg checkAgainst paramType

            // here we use unify, not sub since this really models substitution
            C.unify(capt, cvar)
            argCapt ++= capt
        }

        ret / (funCapt ++ argCapt)

      case source.TryHandle(prog, handlers) =>

        val capabilityParams = handlers.map { h => h.capability.symbol }

        // (1) create new unification scope and check handled program (`prog`) with capabilities in scope
        val ret / bodyCapt = Context.withUnificationScope {
          // bind capability types in type environment
          capabilityParams foreach Context.define
          checkStmt(prog)
        }
        // TODO check that the capabilities do NOT occur free in `ret`

        // subtract capability from inferred capture Cp. Outer unification variables cannot possibly contain the fresh capability.
        val bodyCaptWithoutCapabilities = bodyCapt -- CaptureSet(capabilityParams.map(CaptureOf).toSet)

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

                vparamSyms foreach Context.define

                // TODO add constraints on resume capture
                val resumeType = FunctionType(Nil, Nil, List(ret1), Nil, ret)
                val resumeSym = Context.symbolOf(resume).asBlockSymbol

                Context.define(resumeSym, resumeType, CaptureSet(Set(resumeCapture)))

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
          C.unify(CaptureSet(Set(resumeCapture)), residualCapt)

          ret / residualCapt
        }

      case source.Match(sc, clauses) =>

        // (1) Check scrutinee
        // for example. tpe = List[Int]
        val scTpe / scCapt = checkExpr(sc)

        var capt = scCapt

        // (2) check exhaustivity
        checkExhaustivity(scTpe, clauses.map { _.pattern })

        // (3) infer types for all clauses
        val (fstTpe / fstCapt, _) :: tpes = clauses.map {
          case c @ source.MatchClause(p, body) =>
            Context.define(checkPattern(scTpe, p)) in {
              (checkStmt(body), body)
            }
        }
        capt ++= fstCapt

        // (4) unify clauses and collect effects
        val tpeCases = tpes.foldLeft(fstTpe) {
          case (expected, (clauseTpe / clauseCapt, tree)) =>
            capt ++= clauseCapt
            Context.at(tree) { Context.unify(expected, clauseTpe) }
            expected
        }
        tpeCases / capt

      case source.Select(expr, selector) =>
        Context.abort("Block in expression position: automatic boxing currently not supported.")

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

      // TODO implement

      //      // (4) Compute blocktype of this constructor with rigid type vars
      //      // i.e. Cons : `(?t1, List[?t1]) => List[?t1]`
      //      // constructors can't take block parameters, so we can ignore them safely
      //      val (rigids, FunctionType(_, vpms, _, ret)) = Unification.instantiate(sym.toType)
      //
      //      // (5) given a scrutinee of `List[Int]`, we learn `?t1 -> Int`
      //      Context.unify(ret, sc)
      //
      //      // (6) check for existential type variables
      //      // at the moment we do not allow existential type parameters on constructors.
      //      val skolems = Context.skolems(rigids)
      //      if (skolems.nonEmpty) {
      //        Context.error(s"Unbound type variables in constructor ${id}: ${skolems.map(_.underlying).mkString(", ")}")
      //      }
      //
      //      // (7) refine parameter types of constructor
      //      // i.e. `(Int, List[Int])`
      //      val constructorParams = vpms map { p => Context.unifier substitute p }
      //
      //      // (8) check nested patterns
      //      var bindings = Map.empty[Symbol, ValueType]
      //
      //      (patterns, constructorParams) match {
      //        case (pats, pars) =>
      //          if (pats.size != pars.size)
      //            Context.error(s"Wrong number of pattern arguments, given ${pats.size}, expected ${pars.size}.")
      //
      //          (pats zip pars) foreach {
      //            case (pat, par: ValueType) =>
      //              bindings ++= checkPattern(par, pat)
      //            case _ =>
      //              Context.panic("Should not happen, since constructors can only take value parameters")
      //          }
      //      }
      //      bindings
      ???
  }

  //</editor-fold>

  //<editor-fold desc="statements and definitions">

  def checkStmt(stmt: Stmt)(implicit C: Context): TyperResult[ValueType] =
    check(stmt) {
      case source.DefStmt(b, rest) =>
        val t = Context in { precheckDef(b); checkDef(b) }
        val r = checkStmt(rest)
        r

      // <expr> ; <stmt>
      case source.ExprStmt(e, rest) =>
        val _ = checkExpr(e)
        val r = checkStmt(rest)
        r

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
      val funCapt = CaptureSet(Set(C.freshCaptVar(CaptureOf(funSym))))
      Context.define(funSym, funCapt)
      funSym.ret.foreach { annot => Context.define(d.symbol, d.symbol.toType) }

    case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
      Context.define(d.symbol, d.symbol.toType, Pure)

    case d @ source.InterfaceDef(id, tparams, ops) =>
      d.symbol.ops.foreach { op =>
        val tpe = op.toType
        wellformed(tpe)
        Context.define(op, tpe, Pure)
      }

    case source.DataDef(id, tparams, ctors) =>
      ctors.foreach { ctor =>
        val sym = ctor.symbol
        Context.define(sym, sym.toType, Pure)

        sym.fields.foreach { field =>
          val tpe = field.toType
          wellformed(tpe)
          Context.define(field, tpe, Pure)
        }
      }

    case _ => ()
  }

  def checkDef(d: Def)(implicit C: Context): Unit = Context.at(d) {
    d match {
      case d @ source.FunDef(id, tparams, vparams, bparams, ret, body) =>
        val sym = d.symbol
        sym.vparams foreach Context.define
        sym.bparams foreach Context.define

        val precheckedCapt = C.lookupCapture(sym)

        val tpe / capt = Context.withUnificationScope {
          sym.ret match {
            case Some(tpe) => body checkAgainst tpe
            case None      => checkStmt(body)
          }
        }
        // TODO check whether the subtraction here works in presence of unifcation variabels
        val captWithoutBoundParams = capt -- CaptureSet(sym.bparams.map(CaptureOf).toSet)

        Context.define(sym, sym.toType(tpe), captWithoutBoundParams)
        Context.assignType(d, tpe)

        // since we do not have capture annotations for now, we do not need subsumption here and this is really equality
        C.unify(captWithoutBoundParams, precheckedCapt)
        tpe

      case d @ source.ValDef(id, annot, binding) =>
        val tpe / capt = d.symbol.tpe match {
          case Some(t) =>
            binding checkAgainst t
          case None => checkStmt(binding)
        }
        Context.define(d.symbol, tpe)

      case d @ source.VarDef(id, annot, binding) =>
        val tpe / capt = d.symbol.tpe match {
          case Some(t) => binding checkAgainst t
          case None    => checkStmt(binding)
        }
        Context.define(d.symbol, tpe)
        tpe

      case d @ source.ExternFun(pure, id, tparams, vparams, tpe, body) =>
        d.symbol.vparams map { p => Context.define(p) }

      // all other definitions have already been prechecked
      case d => ()
    }
  }

  //</editor-fold>

  //<editor-fold desc="arguments and parameters">

  def checkBlockArgument(arg: source.BlockArg)(implicit C: Context): TyperResult[BlockType] = arg match {
    case arg: source.FunctionArg  => checkFunctionArgument(arg)
    case arg: source.InterfaceArg => checkCapabilityArgument(arg)
  }

  def checkCapabilityArgument(arg: source.InterfaceArg)(implicit C: Context): TyperResult[BlockType] =
    C.lookupType(arg.definition) / C.lookupCapture(arg.definition)

  // Example.
  //   BlockParam: def foo { f: Int => String / Print }
  //   BlockArg: foo { n => println("hello" + n) }
  //     or
  //   BlockArg: foo { (n: Int) => println("hello" + n) }
  def checkFunctionArgument(arg: source.FunctionArg)(implicit C: Context): TyperResult[FunctionType] = arg match {
    case source.FunctionArg(tparams, vparams, bparams, body) =>
      val tparamSymbols = tparams.map { p => p.symbol.asTypeVar }
      tparamSymbols.foreach { p => Context.define(p, p) }
      vparams.foreach { p => Context.define(p.symbol) }
      bparams.foreach { p => Context.define(p.symbol) }
      val capts = bparams.map { p => CaptureOf(p.symbol) }

      // TODO should we open a new unification scope here?
      val ret / capt = checkStmt(body)

      FunctionType(tparamSymbols, capts, vparams.map { p => p.symbol.tpe }, bparams.map { p => p.symbol.tpe }, ret) / (capt -- CaptureSet(capts.toSet))
  }

  //</editor-fold>

  private def freeTypeVars(o: Any): Set[TypeVar] = o match {
    case t: symbols.TypeVar => Set(t)
    /** TODO: do something about cparams */
    case FunctionType(tparams, cparams, vparams, bparams, ret) =>
      freeTypeVars(vparams) ++ freeTypeVars(bparams) ++ freeTypeVars(ret) -- tparams.toSet
    // case e: Effects            => freeTypeVars(e.toList)
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
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

  private implicit class BlockArgOps(block: source.BlockArg) {
    def checkAgainst(tpe: BlockType)(implicit C: Context): TyperResult[BlockType] = Context.at(block) {
      val got / capt = checkBlockArgument(block)
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
      C.assignType(t, got)
      got / capt
    }

  def checkBlock[T <: Tree](t: T)(f: T => TyperResult[BlockType])(implicit C: Context): TyperResult[BlockType] =
    Context.at(t) {
      val got / capt = f(t)
      wellformed(got)
      C.assignType(t, got)
      got / capt
    }
}

trait TyperOps extends ContextOps { self: Context =>

  /**
   * The current unification Scope
   */
  private var scope: UnificationScope = new UnificationScope

  /**
   * Annotations added by typer
   *
   * The annotations are immutable and can be backtracked.
   */
  private var annotations: Annotations = Annotations.empty

  /**
   * We need to substitute after solving and update the DB again, later.
   */
  private var inferredValueTypes: List[(Tree, ValueType)] = Nil
  private var inferredBlockTypes: List[(Tree, BlockType)] = Nil

  private[typer] def initTyperstate(): Unit = {
    scope = new UnificationScope
    annotations = Annotations.empty
    inferredValueTypes = List.empty
    inferredBlockTypes = List.empty
    valueTypingContext = Map.empty
    blockTypingContext = Map.empty
  }

  private[typer] def commitTypeAnnotations(): Unit = {
    // TODO substitute and commit
    annotations.commit()
    //    annotate(Annotations.Unifier, module, currentUnifier)

    println(">>>>>>>>>>>>>>>>>\nDone Typechecking!\n>>>>>>>>>>>>>>>>>\n")

    // now also store the typing context in the global database:
    valueTypingContext foreach { case (s, tpe) => assignType(s, tpe) }
    blockTypingContext foreach { case (s, tpe) => assignType(s, tpe) }
    captureContext foreach {
      case (s, c) =>
        println(s"${s} @ $c");
        assignCaptureSet(s, c)
    }
  }

  // Unification
  // ===========

  // opens a fresh unification scope
  private[typer] def withUnificationScope[R](block: => R): R = {
    val outerScope = scope
    scope = new UnificationScope
    println(s"entering scope ${scope.id}")
    val res = block
    // leaving scope: solve here and check all are local unification variables are defined...
    val (typeSubst, captSubst, cs, ccs) = scope.solve

    // TODO applying the substitution to the environment is not enough! We also need to substitute into types and captures
    // that are locally in scope in Typer...

    // The unification variables now go out of scope:
    // use the substitution to update the defined symbols (typing context) and inferred types (annotated trees).
    valueTypingContext = valueTypingContext.view.mapValues { t => captSubst.substitute(typeSubst.substitute(t)) }.toMap
    blockTypingContext = blockTypingContext.view.mapValues { t => captSubst.substitute(typeSubst.substitute(t)) }.toMap
    captureContext = captureContext.view.mapValues { c => captSubst.substitute(c) }.toMap
    outerScope.addAllType(cs)
    outerScope.addAllCapt(ccs)

    //    println(s"leaving scope ${scope.id}")
    //    println(s"found substitutions: ${typeSubst}")
    //    println(s"unsolved constraints: ${cs}")
    println(s"Found capture substitution: ${captSubst}")
    scope = outerScope
    res
  }

  def unify(t1: ValueType, t2: ValueType) = scope.requireEqual(t1, t2)
  def unify(t1: BlockType, t2: BlockType) = scope.requireEqual(t1, t2)
  def unify(c1: CaptureSet, c2: CaptureSet) = scope.requireEqual(c1, c2)

  def sub(c1: CaptureSet, c2: CaptureSet) = scope.requireSub(c1, c2)

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

  private[typer] def assignType(t: Tree, e: ValueType): Context = {
    annotations.annotate(Annotations.InferredType, t, e)
    this
  }

  // TODO actually store in DB
  private[typer] def assignType(t: Tree, e: BlockType): Context = this

  // this also needs to be backtrackable to interact correctly with overload resolution
  private[typer] def annotateBlockArgument(t: source.FunctionArg, tpe: FunctionType): Context = {
    annotations.annotate(Annotations.BlockArgumentType, t, tpe)
    this
  }

  private[typer] def annotateTypeArgs(call: source.Call, targs: List[symbols.ValueType]): Context = {
    annotations.annotate(Annotations.TypeArguments, call, targs)
    this
  }

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

  private[typer] def define(s: Symbol, tpe: ValueType): Context = {
    valueTypingContext += (s -> tpe); this
  }

  private[typer] def define(s: Symbol, tpe: BlockType, capt: CaptureSet): Context = {
    define(s, tpe); define(s, capt); this
  }

  private[typer] def define(s: Symbol, tpe: BlockType): Context = {
    blockTypingContext += (s -> tpe)
    this
  }

  private[typer] def define(s: Symbol, capt: CaptureSet): Context = {
    captureContext += (s -> capt); this
  }

  private[typer] def define(bs: Map[Symbol, ValueType]): Context = {
    bs foreach {
      case (v: ValueSymbol, t: ValueType) => define(v, t)
      //        case (v: BlockSymbol, t: FunctionType) => define(v, t)
      case other => panic(s"Internal Error: wrong combination of symbols and types: ${other}")
    }; this
  }

  private[typer] def define(p: ValueParam): Context = p match {
    case s @ ValueParam(name, tpe) =>
      define(s, tpe); this
    case s => panic(s"Internal Error: Cannot add $s to typing context.")
  }

  private[typer] def define(p: BlockParam): Context = p match {
    case s @ BlockParam(name, tpe) => define(s, tpe, CaptureSet(Set(CaptureOf(s))))
    case s => panic(s"Internal Error: Cannot add $s to typing context.")
  }
}
