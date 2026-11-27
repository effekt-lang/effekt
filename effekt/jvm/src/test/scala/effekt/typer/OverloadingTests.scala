package effekt
package typer

import effekt.util.messages.{Diagnostic, Failure, Message, Severity, StructuredMessage}

class OverloadingTests extends munit.FunSuite {

  def diagnostic(text: String) = Diagnostic(Severity.Error, None, Nil, Message(StructuredMessage(StringContext(text), Nil)), None)
  def rejected(name: String) = Failure.Rejected(Vector(diagnostic(name)))
  def undecided(name: String) = Failure.Undecided(diagnostic(name), Vector.empty)

  // a candidate's name says what trying it gives
  def attempt(name: String): Either[Failure, String] =
    if (name.startsWith("fit")) Right(s"result of $name")
    else if (name.startsWith("undecided")) Left(undecided(name))
    else Left(rejected(name))

  def resolveIn(scopes: List[String]*): Outcome[String, String] = resolveOverloaded(scopes.iterator)(attempt)

  test("the innermost scope with a fit decides") {
    assertEquals(resolveIn(List("rejected"), List("fit"), List("fit2")), Outcome.Resolved("fit", "result of fit"))
  }

  test("two fits in one scope are ambiguous") {
    assertEquals(resolveIn(List("fit1", "rejected", "fit2")), Outcome.Ambiguous(List("fit1", "fit2")))
  }

  test("two fits are ambiguous whatever is undecided") {
    assertEquals(resolveIn(List("undecided", "fit1", "fit2")), Outcome.Ambiguous(List("fit1", "fit2")))
  }

  test("an undecided candidate competes with a fit") {
    assertEquals(resolveIn(List("undecided", "fit")),
      Outcome.Undecided(List(("fit", None), ("undecided", Some(undecided("undecided"))))))
  }

  test("a scope with only undecided candidates decides") {
    assertEquals(resolveIn(List("undecided1", "rejected", "undecided2"), List("fit")),
      Outcome.Undecided(List(("undecided1", Some(undecided("undecided1"))), ("undecided2", Some(undecided("undecided2"))))))
  }

  test("scopes with only rejected candidates, and empty scopes, fall through") {
    assertEquals(resolveIn(List("rejected"), Nil, List("fit")), Outcome.Resolved("fit", "result of fit"))
  }

  test("no fit anywhere: every scope's failures, innermost first") {
    assertEquals(resolveIn(List("rejected1", "rejected2"), Nil, List("rejected3")),
      Outcome.Failed(List("rejected1", "rejected2", "rejected3").map { name => (name, rejected(name)) }))
  }

  test("no scopes: failed with nothing tried") {
    assertEquals(resolveIn(), Outcome.Failed(Nil))
  }

  test("a scope after the deciding one is not reached") {
    var reached = 0
    var attempted = List.empty[String]
    val scopes = Iterator(List("rejected"), List("fit"), List("fit2")).map { scope => reached += 1; scope }
    val outcome = resolveOverloaded(scopes) { name => attempted = attempted :+ name; attempt(name) }

    assertEquals(outcome, Outcome.Resolved("fit", "result of fit"))
    assertEquals(reached, 2)
    assertEquals(attempted, List("rejected", "fit"))
  }
}
