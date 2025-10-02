package effekt
package core

import effekt.core.Type.*

class TypeInferenceTests extends CoreTests {

  val SomeC: Id    = Id("Some")
  val NoneC: Id    = Id("None")
  val OptionId: Id = Id("Option")
  def OptionT(tpe: core.ValueType): core.ValueType.Data =
    core.ValueType.Data(OptionId, List(tpe))

  test("infer Make type") {
    val intOptionTy = OptionT(TInt)

    // Some[Int](42)
    val option = Make(intOptionTy, SomeC, List(), List(Literal(42, TInt)))

    // Option[Int]
    val expected = intOptionTy

    assertEquals(option.tpe, expected)
  }
}
