package effekt
package core

import scala.collection.mutable
import effekt.PhaseResult.CoreTransformed
import effekt.context.Context
import effekt.util.Trampoline
import effekt.util.messages.ErrorMessageReifier

/**
 * Synthesizes `repr: (A) {rose[Label, Unit]} => Unit` functions, one per distinct [[ValueType]] `A` encountered.
 *
 * Every call `repr[T](x)` in the program is rewritten to an invocation of a
 * concrete, per-type function that performs `$cap.rose(label) { children }` calls directly in Core IR.
 */
object Repr extends Phase[CoreTransformed, CoreTransformed] {
  override val phaseName: String = "repr"

  private val REPR           = "repr"
  private val REPR_BUILTIN   = "reprBuiltin"
  private val ROSE_INTERFACE = "rose"
  private val ROSE_OPERATION = "labelled"
  private val LABEL_TYPE     = "Llabel"

  /** Names that DCE must preserve for the Repr pass to function. */
  def requiredNames(core: ModuleDecl)(using Context): Set[Id] = {
    val dctx = DeclarationContext(core.declarations, core.externs)

    val roseIds = dctx.interfaces.values
      .filter(_.id.name.name == ROSE_INTERFACE)
      .flatMap(i => i.id +: i.properties.map(_.id)).toSet

    val labelIds = dctx.datas.values
      .filter(_.id.name.name == LABEL_TYPE)
      .flatMap(d => d.id +: d.constructors.map(_.id)).toSet

    val builtinIds = core.definitions.collect {
      case Toplevel.Def(id, _) if id.name.name == REPR_BUILTIN => id
    }.toSet

    roseIds ++ labelIds ++ builtinIds
  }

  case class RoseMetadata(
    operationId:   Id,
    interfaceType: BlockType.Interface,
    operationType: BlockType,
    capParam:      BlockParam
  ) {
    def capVar: Block.BlockVar =
      Block.BlockVar(capParam.id, interfaceType, Set.empty)
  }

  object RoseMetadata {
    def apply(using dctx: DeclarationContext)(using Context): RoseMetadata = {
      val ifaceDecl = dctx.interfaces.values
        .find(_.id.name.name == ROSE_INTERFACE)
        .getOrElse(Context.abort(
          pretty"Repr phase: cannot find '${ROSE_INTERFACE}' interface. Is the 'effekt.effekt' module imported?"))

      val op = ifaceDecl.properties
        .find(_.id.name.name == ROSE_OPERATION)
        .getOrElse(Context.abort(
          pretty"Repr phase: '${ROSE_INTERFACE}' interface has no '${ROSE_OPERATION}' operation."))

      val labelDecl = dctx.datas.values
        .find(_.id.name.name == LABEL_TYPE)
        .getOrElse(Context.abort(
          pretty"Repr phase: cannot find '${LABEL_TYPE}' data type. Is the 'effekt.effekt' module imported?"))

      val labelTpe = ValueType.Data(labelDecl.id, Nil)
      val ifaceTpe: BlockType.Interface = BlockType.Interface(ifaceDecl.id, List(labelTpe, Type.TUnit))

      val tparamSubst: Map[Id, ValueType] = (ifaceDecl.tparams zip List(labelTpe, Type.TUnit)).toMap
      val opTpe = Type.substitute(op.tpe, tparamSubst, Map.empty)

      val capId = Id("$rose")
      val capParam = BlockParam(capId, ifaceTpe, Set.empty)

      RoseMetadata(op.id, ifaceTpe, opTpe, capParam)
    }
  }

  case class LabelMetadata(
    decl:    Declaration.Data,
    dataTpe: ValueType.Data
  ) {
    private def ctor(name: String)(using Context): Constructor =
      decl.constructors.find(_.id.name.name == name).getOrElse(
        Context.abort(pretty"Repr phase: cannot find Label constructor '${name}'"))

    private def make(ctorName: String, fields: List[Expr])(using Context): Expr =
      Expr.Make(dataTpe, ctor(ctorName).id, Nil, fields)

    inline def app(name: String)(using Context): Expr =
      make("App",    List(Expr.Literal(name, Type.TString)))
    inline def prop(name: String)(using Context): Expr =
      make("Prop",   List(Expr.Literal(name, Type.TString)))
    inline def opaque(name: String)(using Context): Expr =
      make("Opaque", List(Expr.Literal(name, Type.TString)))
    inline def literal(text: String)(using Context): Expr =
      make("Literal", List(Expr.Literal(text, Type.TString)))
  }

  object LabelMetadata {
    def apply(using dctx: DeclarationContext)(using Context): LabelMetadata = {
      val decl = dctx.datas.values
        .find(_.id.name.name == LABEL_TYPE)
        .getOrElse(Context.abort(
          pretty"Repr phase: cannot find '${LABEL_TYPE}'. Is the 'effekt.effekt' module imported?"))

      LabelMetadata(decl, ValueType.Data(decl.id, Nil))
    }
  }

  /** Small DSL for building the Core IR fragments used in generated repr code. */
  private object Emit {
    inline def v(id: Id, tpe: ValueType): Expr = Expr.ValueVar(id, tpe)
    inline def retUnit: Stmt                   = Stmt.Return(Expr.Literal((), Type.TUnit))

    /** `$cap.rose(label) { () => childrenBody }` */
    inline def invokeRose(cap: Block.BlockVar, label: Expr)(inline childrenBody: => Stmt)(using ctx: ReprContext): Stmt = {
      val childrenBlock = BlockLit(Nil, Nil, Nil, Nil, childrenBody)
      Stmt.Invoke(cap, ctx.rose.operationId, ctx.rose.operationType, List(), List(label), List(childrenBlock))
    }

    /** `$cap.rose(label) { () }` (empty children). */
    inline def invokeRoseLeaf(cap: Block.BlockVar, label: Expr)(using ReprContext): Stmt =
      invokeRose(cap, label) { retUnit }

    /** `reprFn(valueExpr) { cap }`, call a repr function with the rose capability. */
    inline def callRepr(reprFn: Block.BlockVar, valueExpr: Expr, cap: Block.BlockVar): Stmt =
      Stmt.App(reprFn, Nil, List(valueExpr), List(cap))
  }

  /** Central context for the phase, contains metadata + a counter */
  class ReprContext(
    val reprNames:    mutable.Map[ValueType, Id]           = mutable.Map.empty,
    val reprDefns:    mutable.Map[ValueType, Toplevel.Def] = mutable.Map.empty,
    val tparamLookup: mutable.Map[Id, ValueType]           = mutable.Map.empty,
    val defsByName:   Map[String, List[Toplevel]],
    val rose:         RoseMetadata,
    val label:        LabelMetadata,
    private var _counter: Int = 0
  ) {
    def generatedDefs: List[Toplevel.Def] = reprDefns.values.toList
    def freshReprId: Id = { val n = _counter; _counter += 1; Id(s"repr$n") }

    /** `(vt) { rose[Label, Unit] } => Unit` — the type of every generated repr fn. */
    inline def reprFunctionType(vt: ValueType): BlockType =
      BlockType.Function(Nil, List(rose.capParam.id), List(vt), List(rose.interfaceType), Type.TUnit)
  }

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) =>
      given dctx: DeclarationContext = DeclarationContext(core.declarations, core.externs)
      given ctx: ReprContext = new ReprContext(
        defsByName = core.definitions.groupBy(_.id.name.name),
        rose       = RoseMetadata(using dctx),
        label      = LabelMetadata(using dctx),
      )
      val rewriter = new ReprRewrite()
      val transformed = rewriter.rewrite(core)
      Some(CoreTransformed(source, tree, mod, transformed))
  }

  private class ReprRewrite(using ctx: ReprContext, C: Context, dctx: DeclarationContext)
      extends core.Tree.TrampolinedRewrite {

    override def rewrite(stmt: Stmt): Trampoline[Stmt] = stmt match {
      // intercept `repr[T](vargs; bargs)`
      case Stmt.App(Block.BlockVar(bid, _, _), List(targ), vargs, bargs) if bid.name.name == REPR =>
        for {
          vargs2 <- all(vargs, rewrite)
          bargs2 <- all(bargs, rewrite)
        } yield Stmt.App(getReprBlockVar(targ), Nil, vargs2, bargs2)

      case other => super.rewrite(other)
    }

    // Override ModuleDecl to append generated defs at the end
    override def rewrite(m: ModuleDecl): ModuleDecl = {
      val base = super.rewrite(m)
      base.copy(definitions = base.definitions ++ ctx.generatedDefs)
    }
  }

  private def getReprBlockVar(vt: ValueType)(using ctx: ReprContext)(using Context, DeclarationContext): Block.BlockVar = {
    val id = ctx.reprNames.getOrElse(vt, generateReprFor(vt))
    Block.BlockVar(id, ctx.reprFunctionType(vt), Set(ctx.rose.capParam.id))
  }

  /**
   * Allocate a fresh id, build `def reprN(value: paramTpe) { $cap }: Unit`,
   * and register it.  The id is registered *before* evaluating [[body]] so that
   * recursive types that call [[getReprBlockVar]] inside [[body]] find the id.
   */
  private def makeReprDef(vt: ValueType, paramTpe: ValueType)(body: (Id, Block.BlockVar) => Stmt)(using ctx: ReprContext)(using Context): Toplevel.Def = {
    val fid  = ctx.freshReprId
    ctx.reprNames += (vt -> fid)
    val pId  = Id("value")
    val cap  = ctx.rose.capParam
    val defn: Toplevel.Def = Toplevel.Def(fid,
      BlockLit(Nil, List(cap.id), List(ValueParam(pId, paramTpe)), List(cap),
        body(pId, ctx.rose.capVar)))
    ctx.reprDefns += (vt -> defn)
    defn
  }

  /**
   * Generate (and register) a repr function for [[vt]], provided it doesn't exist already.
   */
  private def generateReprFor(vt: ValueType)(using ctx: ReprContext, dctx: DeclarationContext)(using Context): Id = vt match {
    case ValueType.Data(name, targs) =>
      val resolved = targs map lookupType
      val dataTpe  = ValueType.Data(name, resolved)

      findReprBuiltinFor(name, resolved) match {
        // 1. Prefer an explicit 'reprBuiltin' for 'name'
        case Some(bv) =>
          val reprBlocks: List[Block] = resolved.map(getReprBlockVar)
          makeReprDef(vt, dataTpe) { (pId, cap) =>
            Stmt.App(bv, resolved, List(Emit.v(pId, dataTpe)), reprBlocks :+ cap)
          }.id

        case None =>
          // 2. Otherwise, try a structural derivation from Declaration.Data
          dctx.datas.get(name) match {
            case Some(dataDecl) if dataDecl.constructors.nonEmpty =>
              ctx.tparamLookup ++= (dataDecl.tparams zip resolved)
              // Register the id before building the body: recursive field
              // types may call getReprBlockVar(dataTpe) during derivation.
              val fid = ctx.freshReprId
              ctx.reprNames += (vt -> fid)
              val defn = deriveStructurally(dataDecl, fid, resolved, dataTpe)
              ctx.reprDefns += (vt -> defn)
              defn.id

            // 3. No data declaration and no 'reprBuiltin' ~> opaque fallback (for externs)
            case _ =>
              makeReprDef(vt, dataTpe) { (_, cap) =>
                Emit.invokeRoseLeaf(cap, ctx.label.opaque(name.name.name))
              }.id
          }
      }

    case ValueType.Var(name) =>
      val concrete = ctx.tparamLookup.getOrElse(name,
        Context.abort(pretty"Repr phase: unbound type variable '${name}'; too much type indirection?"))

      ctx.reprNames.get(concrete) match {
        case Some(existingId) =>
          ctx.reprNames += (vt -> existingId) // alias only; no new def!
          existingId
        case None =>
          val inner = generateReprFor(concrete)
          ctx.reprNames.get(concrete).foreach { id => ctx.reprNames += (vt -> id) }
          inner
      }

    // boxed ~> opaque
    case ValueType.Boxed(_, _) =>
      makeReprDef(vt, vt) { (_, cap) =>
        Emit.invokeRoseLeaf(cap, ctx.label.opaque(s"box"))
      }.id
  }

  /**
   * Build a repr function that matches on all constructors of [[decl]].
   *
   * Generated structure for `type Foo { Bar(x: Int, y: Bool) ; Baz() }`:
   * {{{
   *   def repr0(value: Foo) { $cap: rose[Label, Unit] }: Unit = value match {
   *     case Bar(x, y) =>
   *       $cap.rose(App("Bar")) {
   *         val _unit_1 = $cap.rose(Prop("x")) { repr1(x) { $cap } }
   *         val _unit_2 = $cap.rose(Prop("y")) { repr2(y) { $cap } }
   *         ()
   *       }
   *     case Baz() =>
   *       val _unit_3 = $cap.rose(App("Baz")) { () }
   *       ()
   *   }
   * }}}
   */
  private def deriveStructurally(decl: Declaration.Data, fid: Id, targs: List[ValueType], dataTpe: ValueType)(using ctx: ReprContext, dctx: DeclarationContext)(using Context): Toplevel.Def = {
    val valueId = Id("value")
    val cap     = ctx.rose.capParam
    val capVar  = ctx.rose.capVar

    val clauses: List[(Id, BlockLit)] = decl.constructors.map { constr =>
      val fieldParams = constr.fields.map {
        case Field(fid, tpe) => ValueParam(fid, lookupType(tpe))
      }
      constr.id -> BlockLit(Nil, Nil, fieldParams, Nil,
        constructorBranch(constr.id.name.name, fieldParams, capVar))
    }

    Toplevel.Def(fid,
      BlockLit(Nil, List(cap.id), List(ValueParam(valueId, dataTpe)), List(cap),
        Stmt.Match(Emit.v(valueId, dataTpe), Type.TUnit, clauses, None)))
  }

  /**
   * Build one match branch body for a constructor.
   *
   * Uses `foldRight` so the last field is a tail [[Stmt.Invoke]], and all
   * preceding fields are sequenced with [[Emit.seq]].
   * The whole children body is wrapped in `$cap.rose(App(name)) { … }`.
   */
  private def constructorBranch(ctorName: String, fields: List[ValueParam], cap: Block.BlockVar)(using ctx: ReprContext, dctx: DeclarationContext)(using Context): Stmt =
    Emit.invokeRose(cap, ctx.label.app(ctorName)) {
      val bindings = fields.map { case ValueParam(fid, ftpe) =>
        val rhs = Emit.invokeRose(cap, ctx.label.prop(fid.name.name)) {
          Emit.callRepr(getReprBlockVar(ftpe), Emit.v(fid, ftpe), cap)
        }
        Binding.Val(Id("_unit"), rhs)
      }
      Binding(bindings, Emit.retUnit)
    }

  /** Find `def reprBuiltin[A,…](value: D[A,…]) { reprA } … { $cap }: Unit` for a data type. */
  private def findReprBuiltinFor(name: Id, targs: List[ValueType])(using ctx: ReprContext, dctx: DeclarationContext)(using Context): Option[Block.BlockVar] = {
    val expectedBparams = targs.length + 1 // one per each targ + one extra for the capability

    def matches(vps: List[ValueParam], bps: List[BlockParam]): Boolean =
      bps.length == expectedBparams && (vps match {
        case List(ValueParam(_, ValueType.Data(n, _))) => n == name
        case _                                         => false
      })

    ctx.defsByName.getOrElse(REPR_BUILTIN, Nil).collectFirst {
      case Toplevel.Def(id, lit @ BlockLit(tps, cps, vps, bps, _)) if matches(vps, bps) =>
        Block.BlockVar(id,
          BlockType.Function(tps, cps, vps.map(_.tpe), bps.map(_.tpe), Type.TUnit), cps.toSet)
    }
  }

  /** Recursively substitute type variables via the current `tparamLookup`. */
  private def lookupType(vt: ValueType)(using ctx: ReprContext): ValueType = vt match {
    case ValueType.Data(name, targs) => ValueType.Data(name, targs map lookupType)
    case ValueType.Var(name)         => ctx.tparamLookup.getOrElse(name, vt)
    case _                           => vt
  }
}