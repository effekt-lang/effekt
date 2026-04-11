package effekt
package cpsds

import core.Id
import scala.collection.mutable

class FunctionInfo(
  val params: List[Id],
  val body: Stmt,
  val internalCalls: mutable.ListBuffer[List[Expr]] = mutable.ListBuffer.empty,
  val externalCalls: mutable.ListBuffer[List[Expr]] = mutable.ListBuffer.empty
) {
  def isRecursive: Boolean = internalCalls.nonEmpty

  def callCount: Int = internalCalls.size + externalCalls.size

  def isCalledOnce: Boolean = callCount == 1 && !isRecursive

  def allCalls: Iterable[List[Expr]] = internalCalls ++ externalCalls

  def staticArguments: List[Option[Expr]] =
    if allCalls.isEmpty then params.map(_ => None)
    else
      val all = allCalls.toList
      val first = all.head
      first.zipWithIndex.map { case (arg, idx) =>
        if all.forall(call => call.length > idx && call(idx) == arg)
        then Some(arg)
        else None
      }
}

class UsageInfo(
  var references: Int = 0
) {
  def isUnused: Boolean = references == 0
  def isUsedOnce: Boolean = references == 1
}

class Analysis(
  val functions: mutable.Map[Id, FunctionInfo] = mutable.Map.empty,
  val usage: mutable.Map[Id, UsageInfo] = mutable.Map.empty,
  var stack: List[Id] = Nil
) {

  private def use(id: Id): Unit =
    usage.getOrElseUpdate(id, UsageInfo()).references += 1

  def process(e: Expr): Unit = e match {
    case Expr.Variable(id) => use(id)
    case Expr.Literal(_, _) => ()
    case Expr.Make(_, _, vargs) => vargs.foreach(process)
    case Expr.Abort => ()
    case Expr.Return => ()
    case Expr.Toplevel => ()
  }

  def process(s: Stmt): Unit = s match {
    case Stmt.Def(id, params, body, rest) =>
      functions(id) = FunctionInfo(params, body)
      val before = stack
      stack = id :: stack
      process(body)
      stack = before
      process(rest)

    case Stmt.New(id, _, operations, rest) =>
      operations.foreach { op => process(op.body) }
      process(rest)

    case Stmt.Val(id, binding, rest) =>
      process(binding); process(rest)

    case Stmt.Let(id, binding, rest) =>
      process(binding); process(rest)

    case Stmt.App(id, args) =>
      use(id)
      args.foreach(process)
      functions.get(id).foreach { info =>
        if stack.contains(id) then
          info.internalCalls += args
        else
          info.externalCalls += args
      }

    case Stmt.Invoke(id, method, args) =>
      use(id)
      args.foreach(process)

    case Stmt.Run(id, callee, args, _, rest) =>
      use(callee)
      args.foreach(process)
      process(rest)

    case Stmt.If(cond, thn, els) =>
      process(cond); process(thn); process(els)

    case Stmt.Match(scrutinee, clauses, default) =>
      process(scrutinee)
      clauses.foreach { case (_, cl) => process(cl.body) }
      default.foreach(process)

    case Stmt.Region(id, ks, rest) =>
      process(ks); process(rest)

    case Stmt.Alloc(_, init, region, rest) =>
      use(region)
      process(init); process(rest)

    case Stmt.Var(_, init, ks, rest) =>
      process(init); process(ks); process(rest)

    case Stmt.Dealloc(ref, rest) =>
      use(ref); process(rest)

    case Stmt.Get(ref, _, rest) =>
      use(ref); process(rest)

    case Stmt.Put(ref, value, rest) =>
      use(ref); process(value); process(rest)

    case Stmt.Reset(_, _, _, body, ks1, k1) =>
      process(body); process(ks1); process(k1)

    case Stmt.Shift(prompt, _, _, _, body, ks1, k1) =>
      use(prompt); process(body); process(ks1); process(k1)

    case Stmt.Resume(r, _, _, body, ks1, k1) =>
      use(r); process(body); process(ks1); process(k1)

    case Stmt.Hole(_) => ()
  }

  def process(d: ToplevelDefinition): Unit = d match {
    case ToplevelDefinition.Def(id, params, body) =>
      functions(id) = FunctionInfo(params, body)
      val before = stack
      stack = id :: stack
      process(body)
      stack = before
    case ToplevelDefinition.Val(id, binding) => process(binding)
    case ToplevelDefinition.Let(id, binding) => process(binding)
  }

  def process(m: ModuleDecl): Unit =
    m.definitions.foreach(process)
}

object Analysis {
  def apply(m: ModuleDecl): Analysis = {
    val analysis = new Analysis()
    analysis.process(m)
    analysis
  }
}
