package effekt
package util

class ClosestTests extends munit.FunSuite {

  def closestName(name: String, candidates: String*): List[String] = closest(name, List(candidates))(identity)

  test("a name equal up to case wins over a closer spelling") {
    assertEquals(closestName("Mytype", "Mytypes", "MyType"), List("MyType"))
  }

  test("only the closest candidates, in the order given") {
    assertEquals(closestName("sentence", "sentences", "sentinel", "sentence1"), List("sentences", "sentence1"))
  }

  test("each name once, at its first occurrence") {
    assertEquals(closestName("lenght", "length", "size", "length"), List("length"))
  }

  test("nothing beyond the limit") {
    assertEquals(closestName("Person", "Pair", "Option"), Nil)
  }

  test("a one-letter name is one edit from every other one-letter name") {
    assertEquals(closestName("A", "T", "B"), List("T", "B"))
  }

  test("of equally close names, only those in the nearest scope that has any") {
    assertEquals(closest("A", List(List("T"), List("B", "C")))(identity), List("T"))
    assertEquals(closest("printn", List(Nil, List("print", "println")))(identity), List("print", "println"))
  }

  test("a closer name in a farther scope wins over a farther name in a nearer one") {
    assertEquals(closest("lenght", List(List("lengths"), List("length")))(identity), List("length"))
  }
}
