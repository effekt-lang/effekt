package effekt
package lifted
package mono

import core.Id
import effekt.source.NoSource
import effekt.util.messages.{ DebugMessaging, ErrorReporter }
import kiama.util.Positions

class MonomorphizationTests extends munit.FunSuite {

  val hof = Id("hof")
  val f = Id("f")
  val main = Id("main")
  val ev_f = Id("ev_f")

  object messages extends DebugMessaging
  given ErrorReporter with { var focus = NoSource; val messaging = messages; val positions = new Positions }


  test ("simple") {

    // f = { (EV) => return 42 }
    val f_block = Block.BlockLit(Nil, List(Param.EvidenceParam(ev_f)),
      Stmt.Return(Expr.Literal(42, lifted.Type.TInt)))
    val f_def = Definition.Def(f, f_block)

    // main = { () => f }
    val main_fun = Definition.Def(main, Block.BlockLit(Nil, List(),
      Stmt.App(Block.BlockVar(f, f_block.tpe), Nil, List(Evidence(Nil)))))

    val m = ModuleDecl("test", Nil, Nil, Nil, List(f_def, main_fun), Nil)
    given a : FlowAnalysis()
    analyze(m)

    val f_ftpe = a.flowTypeForBinder(f)

    // α0() <: [<>]()
    val costraint = Constraint.B(
      f_ftpe,
      FlowType.Function(Evidences.Concrete(List(Ev(Nil))), Nil))

    assert(a.constraints.contains(costraint))
  }

  extension (ev: Evidences) {
    def assertVar: Evidences.FlowVar = ev match {
      case v @ Evidences.FlowVar(id, arity) => v
      case _ => fail("should be a variable")
    }
  }

  extension (f: FlowType) {
    def assertFun: FlowType.Function = f match {
      case tpe : FlowType.Function => tpe
      case _ => fail("flowtype should be a function type")
    }
  }


}
