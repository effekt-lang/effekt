package effekt
package lifted

import effekt.symbols.builtins


/**
 * Design Decisions:
 * - we don't track effects anymore (no stack-shape, no capture sets)
 * - evidence simply has kind EV.
 * - boxing is not supported (yet), but we include it as an unsafe feature.
 */
sealed trait Type

enum ValueType extends Type {
  case Var(name: Id)
  case Data(name: Id, targs: List[ValueType])
  case Boxed(tpe: BlockType) // WARNING not supported
}

case class EvidenceType() extends Type

enum BlockType extends Type {

  //   [A, B, C] (EV, EV, X, Y, Z, (EV, T) => T)    =>    T
  //    ^^^^^^^   ^^^^^^  ^^^^^^^  ^^^^^^^^^^^^^         ^^^
  //    tparams   evid.   vparams    bparams            result
  case Function(tparams: List[Id], eparams: List[EvidenceType], vparams: List[ValueType], bparams: List[BlockType], result: List[ValueType])
  case Interface(name: Id, targs: List[ValueType])
}

object Type {

  // The subtyping lattice
  val TTop = ValueType.Data(builtins.TopSymbol, Nil)
  val TBottom = ValueType.Data(builtins.BottomSymbol, Nil)

  val TUnit   = ValueType.Data(builtins.UnitSymbol, Nil)
  val TInt = ValueType.Data(builtins.IntSymbol, Nil)
  val TBoolean = ValueType.Data(builtins.BooleanSymbol, Nil)
  val TString = ValueType.Data(builtins.StringSymbol, Nil)
  val TDouble = ValueType.Data(builtins.DoubleSymbol, Nil)

  val TRegion = BlockType.Interface(builtins.RegionSymbol, Nil)

  /**
   * Function types are the only type constructor that we have subtyping on.
   *
   * Copy and paste from core
   */
  def merge(tpe1: ValueType, tpe2: ValueType, covariant: Boolean): ValueType = (tpe1, tpe2) match {
    case (ValueType.Boxed(btpe1), ValueType.Boxed(btpe2)) =>
      ValueType.Boxed(merge(btpe1, btpe2, covariant))
    case (tpe1, tpe2) if covariant =>
      if (isSubtype(tpe1, tpe2)) tpe2 else tpe1
    case (tpe1, tpe2) if !covariant =>
      if (isSubtype(tpe1, tpe2)) tpe1 else tpe2
    case _ => tpe1
  }
  private def isSubtype(tpe1: ValueType, tpe2: ValueType): Boolean = (tpe1, tpe2) match {
    case (tpe1, TTop) => true
    case (TBottom, tpe1) => true
    case _ => false // conservative :)
  }

  def merge(tpe1: BlockType, tpe2: BlockType, covariant: Boolean): BlockType = (tpe1, tpe2) match {
    case (BlockType.Function(tparams1, eparams1, vparams1, bparams1, results1), tpe2: BlockType.Function) =>
      val BlockType.Function(_, eparams2, vparams2, bparams2, results2) = instantiate(tpe2, tparams1.map(ValueType.Var.apply))
      val vparams = (vparams1 zip vparams2).map { case (tpe1, tpe2) => merge(tpe1, tpe2, !covariant) }
      val bparams = (bparams1 zip bparams2).map { case (tpe1, tpe2) => merge(tpe1, tpe2, !covariant) }
      val results = (results1 zip results2).map { case (tpe1, tpe2) => merge(tpe1, tpe2, covariant) }
      BlockType.Function(tparams1, eparams1, vparams, bparams, results)
    case (tpe1, tpe2) => tpe1
  }

  def instantiate(f: BlockType.Function, targs: List[ValueType]): BlockType.Function = f match {
    case BlockType.Function(tparams, eparams, vparams, bparams, results) =>
      assert(targs.size == tparams.size, "Wrong number of type arguments")

      val vsubst = (tparams zip targs).toMap
      BlockType.Function(Nil, eparams,
        vparams.map { tpe => substitute(tpe, vsubst) },
        bparams.map { tpe => substitute(tpe, vsubst) },
        results.map { tpe => substitute(tpe, vsubst) })
  }

  def substitute(tpe: BlockType, vsubst: Map[Id, ValueType]): BlockType = tpe match {
    case BlockType.Function(tparams, eparams, vparams, bparams, results) =>
      // names are unique symbols so shadowing should NOT take place; we still subtract to be safe.
      val vsubstLocal = vsubst -- tparams

      BlockType.Function(tparams, eparams,
        vparams.map { tpe => substitute(tpe, vsubstLocal) },
        bparams.map { tpe => substitute(tpe, vsubstLocal) },
        results.map { tpe => substitute(tpe, vsubstLocal) })

    case BlockType.Interface(sym, targs) =>
      BlockType.Interface(sym, targs map { tpe => substitute(tpe, vsubst) })
  }

  def substitute(tpe: ValueType, vsubst: Map[Id, ValueType]): ValueType = tpe match {
    case ValueType.Var(id) if vsubst.isDefinedAt(id) => vsubst(id)
    case ValueType.Var(id) => tpe
    case ValueType.Data(sym, targs) => ValueType.Data(sym, targs.map(t => substitute(t, vsubst)))
    case ValueType.Boxed(tpe) => ValueType.Boxed(substitute(tpe, vsubst))
  }

  def inferType(block: Block): BlockType = block match {
    case Block.BlockVar(id, tpe) => tpe

    case Block.BlockLit(tparams, params, body) =>

      val eparams = params.collect { case Param.EvidenceParam(id) => EvidenceType() }
      val vparams = params.collect { case Param.ValueParam(id, tpe) => tpe }
      val bparams = params.collect { case Param.BlockParam(id, tpe) => tpe }
      BlockType.Function(tparams, eparams, vparams, bparams, body.tpe)

    case Block.Member(b, field, tpe) => tpe
    case Block.Unbox(pure) => pure.tpe.asInstanceOf[ValueType.Boxed].tpe
    case Block.New(impl) => impl.tpe
  }
  def inferType(stmt: Stmt): List[ValueType] = stmt match {
    case Stmt.Scope(definitions, body) => body.tpe
    case Stmt.Return(exprs) => exprs.flatMap {_.tpe}
    case Stmt.Val(id, binding, body) => body.tpe
    case Stmt.App(callee, targs, args) => instantiate(callee.functionType, targs).result

    case Stmt.If(cond, thn, els) =>
      (thn.tpe zip els.tpe).map { case (tpe1, tpe2) => merge(tpe1, tpe2, covariant = true) }

    case Stmt.Match(scrutinee, clauses, default) =>
      val allTypes = clauses.flatMap { case (_, cl) => cl.returnType } ++ default.map(_.tpe).toList.flatten
      List(allTypes.fold(TBottom) { case (tpe1, tpe2) => merge(tpe1, tpe2, covariant = true) })

    case Stmt.State(id, init, region, ev, body) => body.tpe
    case Stmt.Try(body, handler) => body.returnType
    case Stmt.Region(body) => body.returnType

    case Stmt.Shift(ev, body) =>
      // the annotated argument type on resume is our return type here
      val Some(tpe: BlockType.Function) = body.params.collectFirst { case b: Param.BlockParam => b.tpe }: @unchecked
      tpe.vparams

    case Stmt.Hole() => List(TBottom)
  }

  def inferType(expr: Expr): List[ValueType] = expr match {
    // case DirectApp(callee, targs, vargs, bargs) => instantiate(callee.functionType, targs).result
    case Expr.Run(s) => s.tpe match {
      case List(tpe) => List(tpe)
      case _ => throw new Exception("Run should have exactly one return type") // TODO MRV: correct exception?
    }
    case Expr.ValueVar(id, tpe) => List(tpe)
    case Expr.Literal(value, tpe) => List(tpe)
    case Expr.PureApp(callee, targs, args) => instantiate(callee.functionType, targs).result
    case Expr.Select(target, field, annotatedType) => List(annotatedType)
    case Expr.Box(block) => List(ValueType.Boxed(block.tpe))
  }

  extension (block: Block) {
    def returnType: List[ValueType] = block.functionType.result
    def functionType: BlockType.Function = block.tpe.asInstanceOf
  }
}
