package effekt
package util

class ClosestTests extends munit.FunSuite {

  def closestName(name: String, candidates: String*): List[String] = CloseMatches(name, List(candidates))(identity).closest

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
    assertEquals(CloseMatches("A", List(List("T"), List("B", "C")))(identity).closest, List("T"))
    assertEquals(CloseMatches("printn", List(Nil, List("print", "println")))(identity).closest, List("print", "println"))
  }

  test("a closer name in a farther scope wins over a farther name in a nearer one") {
    assertEquals(CloseMatches("lenght", List(List("lengths"), List("length")))(identity).closest, List("length"))
  }

  test("a filter is not called on matches farther than the closest that pass it") {
    var asked = List.empty[String]
    val kept = CloseMatches("abcdef", List(List("abcdgh", "abcdeg")))(identity).filter { c => asked = c :: asked; true }.closest
    assertEquals(kept, List("abcdeg"))
    assertEquals(asked, List("abcdeg"))
  }
}
