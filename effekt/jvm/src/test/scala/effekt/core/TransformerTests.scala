package effekt.core

import effekt.*
import effekt.PhaseResult.CoreTransformed
import effekt.context.{Context, IOModuleDB}
import effekt.util.PlainMessaging
import kiama.util.StringSource

class TransformerTests extends CoreTests {

  object messaging extends PlainMessaging

  object context extends Context with IOModuleDB {
    val messaging = TransformerTests.this.messaging

    object frontend extends CompileToCore

    override lazy val compiler = frontend.asInstanceOf
  }

  test("handler capabilities use the captures of their local binding") {
    val source = StringSource(
      """effect tick(): Unit
        |
        |def main() =
        |  try { do tick() } with tick { resume(()); println("tick") }
        |""".stripMargin,
      "handler-captures.effekt"
    )

    val config = new EffektConfig(Seq("--Koutput", "string"))
    config.verify()
    context.setup(config)

    val core = context.frontend.Core(source)(using context) match {
      case Some(CoreTransformed(_, _, _, core)) => core
      case None => fail(messaging.formatMessages(context.messaging.buffer))
    }

    val (handler, receiver) = core.definitions.collectFirst {
      case Toplevel.Def(_, BlockLit(_, _, _, _,
            Reset(BlockLit(_, _, _, _,
              Def(capability, handler @ New(_),
                Invoke(receiver @ BlockVar(id, _, _), _, _, _, _, _))))))
          if capability == id => (handler, receiver)
    }.getOrElse(fail("Expected a handled capability invocation in main"))

    assertEquals(receiver.capt, handler.capt)
  }
}
