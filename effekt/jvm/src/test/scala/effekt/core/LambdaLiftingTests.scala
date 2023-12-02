package effekt.core


class LambdaLiftingTests extends CorePhaseTests(LambdaLifting) {

  test("toplevel functions stay unchanged"){
    val from =
      """module main
        |
        |def id = { ['A](a: 'A) => return a: 'A }
        |""".stripMargin
    assertTransformsTo(from, from)
  }

  test("local functions are lifted out"){
    val from =
      """module main
        |
        |def outer = { () =>
        |   def local = { () => return 42 }
        |   val res = (local: () => Int @ {})();
        |   return res:Int
        |}
        |""".stripMargin

    val to =
      """module main
        |
        |def outer = { () =>
        |   {
        |     val res = (local: () => Int @ {})();
        |     return res:Int
        |   }
        |}
        |
        |def local = { () => return 42 }
        |""".stripMargin
    assertTransformsTo(from, to)
  }

}
