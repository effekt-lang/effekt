package effekt
package core

import effekt.symbols.TrackedParam.BlockParam
import symbols.{ Symbol, TypeSymbol, builtins }
import symbols.Name

class Id(n: String) extends symbols.Symbol {
  val name = Name.local(n)
}

type Capture = Symbol
type Captures = Set[Capture]


/**
 * Design Decisions:
 * - [ ] TypeConstructor vs. flat ValueType (right now the latter)
 * - [ ] Locally Nameless vs. Named (right now the latter)
 */
sealed trait Type

enum ValueType extends Type {
  case Var(name: Id)
  case Data(symbol: TypeSymbol, targs: List[ValueType])
  case Record(symbol: TypeSymbol, targs: List[ValueType])
  case Boxed(tpe: BlockType, capt: Captures)
  case Extern(symbol: TypeSymbol, targs: List[ValueType])
}

enum BlockType extends Type {

  // [A, B, C] (X, Y, Z)   {  f  :   S }    =>    T
  //  ^^^^^^^   ^^^^^^^     ^^^^^^^^^^^          ^^^
  //  tparams   vparams   cparams zip bparams   result
  case Function(tparams: List[Id], cparams: List[Id], vparams: List[ValueType], bparams: List[BlockType], result: ValueType)
  case Interface(symbol: TypeSymbol, targs: List[ValueType])
  case Extern(symbol: TypeSymbol, targs: List[ValueType])
}

object Type {

  // The subtyping lattice
  val Bottom = ValueType.Extern(builtins.BottomSymbol, Nil)
  def join(tpe1: ValueType, tpe2: ValueType): ValueType = tpe1 // TODO implement properly

  def instantiate(f: BlockType.Function, targs: List[ValueType], cargs: List[Captures]): BlockType.Function = f match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      assert(targs.size != tparams.size, "Wrong number of type arguments")
      assert(cargs.size != cparams.size, "Wrong number of capture arguments")
      val vsubst = (tparams zip targs).toMap
      val csubst = (cparams zip cargs).toMap
      BlockType.Function(Nil, Nil,
        vparams.map { tpe => substitute(tpe, vsubst, Map.empty) },
        bparams.map { tpe => substitute(tpe, vsubst, Map.empty) },
        substitute(result, vsubst, csubst))
  }

  def substitute(capt: Captures, csubst: Map[Id, Captures]): Captures = capt.flatMap {
    case id: Id if csubst.isDefinedAt(id) => csubst(id)
    case c => Set(c)
  }

  def substitute(tpe: BlockType, vsubst: Map[Id, ValueType], csubst: Map[Id, Captures]): BlockType = tpe match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      // names are unique symbols so shadowing should NOT take place; we still subtract to be safe.
      val vsubstLocal = vsubst -- tparams
      val csubstLocal = csubst -- cparams

      BlockType.Function(tparams, cparams,
        vparams.map { tpe => substitute(tpe, vsubstLocal, csubst) }, // technically in source, cparams are not bound in value arguments
        bparams.map { tpe => substitute(tpe, vsubstLocal, csubstLocal) },
        substitute(result, vsubstLocal, csubstLocal))

    case BlockType.Interface(sym, targs) =>
      BlockType.Interface(sym, targs map { tpe => substitute(tpe, vsubst, csubst) })

    case BlockType.Extern(sym, targs) =>
      BlockType.Extern(sym, targs map { tpe => substitute(tpe, vsubst, csubst) })
  }

  def substitute(tpe: ValueType, vsubst: Map[Id, ValueType], csubst: Map[Id, Captures]): ValueType = tpe match {
    case ValueType.Var(id) if vsubst.isDefinedAt(id) => vsubst(id)
    case ValueType.Var(id) => tpe

    case ValueType.Data(sym, targs)   => ValueType.Data(sym, targs.map(t => substitute(t, vsubst, csubst)))
    case ValueType.Record(sym, targs) => ValueType.Record(sym, targs.map(t => substitute(t, vsubst, csubst)))
    case ValueType.Extern(sym, targs) => ValueType.Extern(sym, targs.map(t => substitute(t, vsubst, csubst)))


    case ValueType.Boxed(tpe, capt) =>
      ValueType.Boxed(substitute(tpe, vsubst, csubst), substitute(capt, csubst))
  }

  def inferType(block: Block): BlockType = block match {
    case Block.BlockVar(id /*, tpe, capt */ ) => ??? // tpe
    case Block.BlockLit(/* tparams, cparams, */ params, body) =>
      val tparams = Nil // TODO
      val cparams = Nil // TODO
      val vparams = params.collect { case p: Param.ValueParam => ??? : ValueType /*p.tpe*/ }
      val bparams = params.collect { case b: Param.BlockParam => ??? : BlockType }

      BlockType.Function(tparams, cparams, vparams, bparams, body.tpe)
    case Block.Member(b, field /*, tpe */) => ??? // tpe
    case Block.Unbox(pure) => pure.tpe.asInstanceOf[ValueType.Boxed].tpe
    case Block.New(impl) => ??? // impl.tpe
  }
  def inferCapt(block: Block): Captures = block match {
    case Block.BlockVar(id /*, tpe, capt */ ) => ??? // capt
    case Block.BlockLit(/* tparams, cparams, */ params, body) =>
      // body.capt -- bparams.map { case (name, _) => Capture.FreeVar(name) }
      ???
    case Block.Member(block, field) => ???
    case Block.Unbox(pure) => ???
    case Block.New(impl) => ???
  }

  /**
   * def inferType(block: Block): BlockType = block match {
   *  case Block.BlockVar(name, tpe, capt) => tpe
   *  case Block.BlockLit(tparams, vparams, bparams, body) =>
   *  BlockType.Function(tparams, vparams, bparams, body.tpe)
   *
   *  case Block.New(instance) => instance.tpe
   *  case Block.Member(recv, sel, tpe) => tpe
   *  case Block.Unbox(expr) => expr.tpe.asInstanceOf[ValueType.Boxed].tpe
   *  }
   *
   *  def inferCapt(block: Block): Captures = block match {
   *  case Block.BlockVar(name, tpe, capt) => capt
   *  case Block.Member(recv, sel, tpe) => recv.capt
   *  case Block.BlockLit(tparams, vparams, bparams, body) => body.capt -- bparams.map { case (name, _) => Capture.FreeVar(name) }
   *  case Block.Unbox(expr) => expr.tpe.asInstanceOf[ValueType.Boxed].capt
   *  case Block.New(instance) => instance.capt
   *  }
   */

  def inferType(stmt: Stmt): ValueType = stmt match {
    case Stmt.Scope(definitions, body) => body.tpe
    case Stmt.Return(expr) => expr.tpe
    case Stmt.Val(id, annotatedType, binding, body) => body.tpe
    case Stmt.App(callee, targs, args) =>
      val bargs = args.collect { case b: Block => b }
      val realTargs = ??? // targs
      instantiate(callee.functionType, realTargs, bargs.map(_.capt)).result

    case Stmt.If(cond, thn, els) => join(thn.tpe, els.tpe)
    case Stmt.Match(scrutinee, clauses, default) =>
      val allTypes = clauses.map { case (_, cl) => cl.returnType } ++ default.map(_.tpe).toList
      allTypes.fold(Bottom)(join)

    case Stmt.State(id, init, region, body) => body.tpe
    case Stmt.Try(body, answerType, handler) => body.returnType
    case Stmt.Region(body, answerType) => body.returnType

    case Stmt.Hole => Bottom
  }

  def inferCapt(defn: Definition): Captures = defn match {
    case Definition.Def(id, tpe, block) => block.capt
    case Definition.Let(id, tpe, binding) => binding.capt
  }

  def inferCapt(arg: Argument): Captures = arg match {
    case pure: Pure => Set.empty
    case block: Block => block.capt
  }

  def inferCapt(stmt: Stmt): Captures = stmt match {
    case Stmt.Scope(definitions, body) =>
      definitions.foldLeft(body.capt)(_ ++ _.capt)
    case Stmt.Return(expr) => Set.empty
    case Stmt.Val(id, annotatedType, binding, body) => binding.capt ++ body.capt
    case Stmt.App(callee, targs, args) => callee.capt ++ args.flatMap(inferCapt).toSet
    case Stmt.If(cond, thn, els) => thn.capt ++ els.capt
    case Stmt.Match(scrutinee, clauses, default) => clauses.flatMap { (_, cl) => cl.capt }.toSet ++ default.toSet.flatMap(s => s.capt)
    case Stmt.State(id, init, region, body) => Set(region) ++ body.capt
    case Stmt.Try(body, answerType, handlers) => body.capt ++ handlers.flatMap(_.capt).toSet
    case Stmt.Region(body, answerType) => body.capt
    case Stmt.Hole => Set.empty
  }

  def inferType(expr: Expr): ValueType = expr match {
    case DirectApp(callee, targs, vargs) =>
      instantiate(callee.functionType, ??? /* targs */, Nil).result
    case Run(s, tpe) => s.tpe
    case Pure.ValueVar(id /*, tpe*/) => ??? // tpe
    case Pure.Literal(value, tpe) => ??? //tpe
    case Pure.PureApp(callee, targs, args) => instantiate(callee.functionType, ??? /*targs*/ , Nil).result
    case Pure.Select(target, field /*, annotatedType*/) => ??? // annotatedType
    case Pure.Box(block /*, capt*/) => ValueType.Boxed(block.tpe, ???)
  }

  /**
   * Invariant: can only be {} or {io}
   */
  def inferCapt(expr: Expr): Captures = expr match {
    case DirectApp(callee, targs, args) => callee.capt ++ args.flatMap {
      case p: Pure => Set.empty
      case b: Block => b.capt
    }
    case Run(s, _) => s.capt
    case pure: Pure => Set.empty
  }

  extension (block: Block) {
    def returnType: ValueType = block.functionType.result
    def functionType: BlockType.Function = block.tpe.asInstanceOf
  }
}
