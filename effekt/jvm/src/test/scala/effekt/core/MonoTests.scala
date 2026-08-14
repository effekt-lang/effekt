package effekt
package core

import java.io.File

import TypeArg.*


class MonoTests extends CorePhaseTests(Mono) {

  private val BaseTInt: Base = Base(Id("Int"), Nil)
  private val BaseTString: Base = Base(Id("String"), Nil)
  private val BaseTChar: Base = Base(Id("Char"), Nil)
  private val BaseTBool: Base = Base(Id("Bool"), Nil)

  private var functionIds: Map[String, FunctionId] = Map.empty

  private def id(name: String): FunctionId =
    functionIds.getOrElse(name, {
      val result = Id(name)
      functionIds += name -> result
      result
    })

  // Product append

  test("product append: empty with empty") {
    assertEquals(productAppend(List(Nil), Nil), List(Nil))
  }

  test("product append: starts with empty list") {
    val first = productAppend(List(Nil), List(1, 2))
    val result = productAppend(first, List(3, 4))

    assertEquals(result, List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
  }

  test("product append: unequal numbers of values") {
    val first = productAppend(List(List(1)), List(2, 3))
    val result = productAppend(first, List(4))

    assertEquals(result, List(List(1, 2, 4), List(1, 3, 4)))
  }

  test("product append: empty map product") {
    val start: Substitutions = List(
      Map(id("a") -> Vector(BaseTInt)),
      Map(id("a") -> Vector(BaseTString)))

    assertEquals(mapProductAppend(start, Nil), start)
  }

  test("product append: single map product") {
    val start: Substitutions = List(
      Map(id("a") -> Vector(BaseTInt)),
      Map(id("a") -> Vector(BaseTString)))
    val append: Variants = List(id("b") -> Vector(BaseTBool))
    val expected: Substitutions = List(
      Map(id("a") -> Vector(BaseTInt), id("b") -> Vector(BaseTBool)),
      Map(id("a") -> Vector(BaseTString), id("b") -> Vector(BaseTBool)))

    assertEquals(mapProductAppend(start, append), expected)
  }

  test("product append: multiple map products") {
    val start: Substitutions = List(
      Map(id("a") -> Vector(BaseTInt)),
      Map(id("a") -> Vector(BaseTString)))
    val withB = mapProductAppend(start, List(id("b") -> Vector(BaseTBool)))
    val result = mapProductAppend(withB, List(
      id("c") -> Vector(BaseTString),
      id("c") -> Vector(BaseTInt)))
    val expected: Substitutions = List(
      Map(id("a") -> Vector(BaseTInt), id("b") -> Vector(BaseTBool), id("c") -> Vector(BaseTString)),
      Map(id("a") -> Vector(BaseTInt), id("b") -> Vector(BaseTBool), id("c") -> Vector(BaseTInt)),
      Map(id("a") -> Vector(BaseTString), id("b") -> Vector(BaseTBool), id("c") -> Vector(BaseTString)),
      Map(id("a") -> Vector(BaseTString), id("b") -> Vector(BaseTBool), id("c") -> Vector(BaseTInt)))

    assertEquals(result, expected)
  }

  test("product append: multiple types in a variant") {
    val start: Substitutions = List(
      Map(id("a") -> Vector(BaseTInt, BaseTString)),
      Map(id("a") -> Vector(BaseTString, BaseTInt)))
    val append: Variants = List(
      id("b") -> Vector(BaseTBool, BaseTBool),
      id("b") -> Vector(BaseTInt, BaseTInt))
    val expected: Substitutions = List(
      Map(id("a") -> Vector(BaseTInt, BaseTString), id("b") -> Vector(BaseTBool, BaseTBool)),
      Map(id("a") -> Vector(BaseTInt, BaseTString), id("b") -> Vector(BaseTInt, BaseTInt)),
      Map(id("a") -> Vector(BaseTString, BaseTInt), id("b") -> Vector(BaseTBool, BaseTBool)),
      Map(id("a") -> Vector(BaseTString, BaseTInt), id("b") -> Vector(BaseTInt, BaseTInt)))

    assertEquals(mapProductAppend(start, append), expected)
  }

  // Solve

  test("solve: simple polymorphic function") {
    val constraints = List(
      MonoConstraint(Vector(BaseTInt), id("a")),
      MonoConstraint(Vector(BaseTString), id("a")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(BaseTInt), Vector(BaseTString)))

    assertEquals(solveConstraints(constraints), expected)
  }

  test("solve: call to another polymorphic function") {
    val constraints = List(
      MonoConstraint(Vector(Var(id("b"), 0)), id("a")),
      MonoConstraint(Vector(BaseTInt), id("a")),
      MonoConstraint(Vector(BaseTString), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
      id("b") -> Set(Vector(BaseTString)))

    assertEquals(solveConstraints(constraints), expected)
  }

  test("solve: multiple type arguments") {
    val constraints = List(
      MonoConstraint(Vector(BaseTInt, BaseTString), id("a")),
      MonoConstraint(Vector(BaseTBool, BaseTChar), id("a")),
      MonoConstraint(Vector(BaseTBool, BaseTString), id("a")))
    val expected: Solution = Map(
      id("a") -> Set(
        Vector(BaseTInt, BaseTString),
        Vector(BaseTBool, BaseTChar),
        Vector(BaseTBool, BaseTString)))

    assertEquals(solveConstraints(constraints), expected)
  }

  test("solve: swapped type arguments") {
    val constraints = List(
      MonoConstraint(Vector(Var(id("b"), 1), Var(id("b"), 0)), id("a")),
      MonoConstraint(Vector(BaseTString, BaseTBool), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(BaseTBool, BaseTString)),
      id("b") -> Set(Vector(BaseTString, BaseTBool)))

    assertEquals(solveConstraints(constraints), expected)
  }

  test("solve: recursive polymorphic function") {
    val constraints = List(
      MonoConstraint(Vector(Var(id("a"), 0)), id("a")),
      MonoConstraint(Vector(BaseTInt), id("a")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(BaseTInt)))

    assertEquals(solveConstraints(constraints), expected)
  }

  test("solve: mutually recursive polymorphic functions") {
    val constraints = List(
      MonoConstraint(Vector(Var(id("b"), 0)), id("a")),
      MonoConstraint(Vector(Var(id("a"), 0)), id("b")),
      MonoConstraint(Vector(BaseTInt), id("a")),
      MonoConstraint(Vector(BaseTString), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
      id("b") -> Set(Vector(BaseTInt), Vector(BaseTString)))

    assertEquals(solveConstraints(constraints), expected)
  }

  test("solve: product of variables") {
    val constraints = List(
      MonoConstraint(Vector(BaseTInt), id("a")),
      MonoConstraint(Vector(BaseTString), id("a")),
      MonoConstraint(Vector(BaseTBool), id("b")),
      MonoConstraint(Vector(Var(id("a"), 0), Var(id("b"), 0)), id("c")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
      id("b") -> Set(Vector(BaseTBool)),
      id("c") -> Set(Vector(BaseTInt, BaseTBool), Vector(BaseTString, BaseTBool)))

    assertEquals(solveConstraints(constraints), expected)
  }

  test("solve: correlated components from one variable") {
    val constraints = List(
      MonoConstraint(Vector(BaseTInt, BaseTString), id("a")),
      MonoConstraint(Vector(BaseTChar, BaseTBool), id("a")),
      MonoConstraint(Vector(Var(id("a"), 0), Var(id("a"), 1)), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(BaseTInt, BaseTString), Vector(BaseTChar, BaseTBool)),
      id("b") -> Set(Vector(BaseTInt, BaseTString), Vector(BaseTChar, BaseTBool)))

    assertEquals(solveConstraints(constraints), expected)
  }

  test("solve: nested constraints") {
    val constraints = List(
      MonoConstraint(Vector(Base(id("Weighted"), List(Var(id("b"), 0)))), id("a")),
      MonoConstraint(Vector(BaseTInt), id("b")))
    val expected: Solution = Map(
      id("b") -> Set(Vector(BaseTInt)),
      id("a") -> Set(Vector(Base(id("Weighted"), List(BaseTInt)))))

    assertEquals(solveConstraints(constraints), expected)
  }

  private def collectConstraints(input: ModuleDecl): MonoConstraints = {
    given MonoFindContext = MonoFindContext()
    input match {
      case ModuleDecl(_, _, declarations, externs, definitions, _) =>
        findConstraints(definitions) ++
          externs.flatMap(findConstraints) ++
          declarations.flatMap(findConstraints)
    }
  }

  private def showTypeArg(arg: TypeArg): String = arg match {
    case Base(tpe, Nil) => tpe.name.name
    case Base(tpe, targs) => s"${tpe.name.name}[${targs.map(showTypeArg).mkString(", ")}]"
    case Var(owner, position) => s"${owner.name.name}.$position"
    case Boxed(_, _) => "BOXED"
  }

  private def showConstraint(constraint: MonoConstraint): String =
    s"${constraint.lower.map(showTypeArg).mkString("<", ", ", ">")} <: ${constraint.upper.name.name}"

  private def showConstraints(input: ModuleDecl): String =
    collectConstraints(input)
      .filter(_.lower.nonEmpty)
      .map(showConstraint)
      .distinct
      .sorted
      .mkString("\n")

  registerCoreIRTests(
    new File("examples/core/mono"),
    CoreIRAnalysis("MONO_COLLECT_CONSTRAINTS", showConstraints),
    CoreIRTransform("MONO_SPECIALIZE", transform))
}
