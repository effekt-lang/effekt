package effekt
package core

import java.io.File

import Mono.{Flow, Flows, FlowVar, Solution, TypeArg}
import Mono.TypeArg.*
import effekt.util.PlainMessaging
import effekt.util.messages.FatalPhaseError


class MonoTests extends CoreTests {

  private val TInt: Data = Data(Id("Int"), Nil)
  private val TString: Data = Data(Id("String"), Nil)
  private val TChar: Data = Data(Id("Char"), Nil)
  private val TBool: Data = Data(Id("Bool"), Nil)

  private var flowVars: Map[String, FlowVar] = Map.empty

  private def id(name: String): FlowVar =
    flowVars.getOrElse(name, {
      val result = Id(name)
      flowVars += name -> result
      result
    })

  // Product append

  test("product append: empty with empty") {
    assertEquals(Mono.solve.productAppend(List(Nil), Nil), Nil)
  }

  test("product append: starts with empty list") {
    val first = Mono.solve.productAppend(List(Nil), List(1, 2))
    val result = Mono.solve.productAppend(first, List(3, 4))

    assertEquals(result, List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
  }

  test("product append: unequal numbers of values") {
    val first = Mono.solve.productAppend(List(List(1)), List(2, 3))
    val result = Mono.solve.productAppend(first, List(4))

    assertEquals(result, List(List(1, 2, 4), List(1, 3, 4)))
  }

  test("product append: empty map product has no combinations") {
    val start: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt)),
      Map(id("a") -> Vector(TString)))

    assertEquals(Mono.solve.mapProductAppend(start, Nil), Nil)
  }

  test("product append: single map product") {
    val start: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt)),
      Map(id("a") -> Vector(TString)))
    val append: Mono.solve.Variants = List(id("b") -> Vector(TBool))
    val expected: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt), id("b") -> Vector(TBool)),
      Map(id("a") -> Vector(TString), id("b") -> Vector(TBool)))

    assertEquals(Mono.solve.mapProductAppend(start, append), expected)
  }

  test("product append: multiple map products") {
    val start: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt)),
      Map(id("a") -> Vector(TString)))
    val withB = Mono.solve.mapProductAppend(start, List(id("b") -> Vector(TBool)))
    val result = Mono.solve.mapProductAppend(withB, List(
      id("c") -> Vector(TString),
      id("c") -> Vector(TInt)))
    val expected: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt), id("b") -> Vector(TBool), id("c") -> Vector(TString)),
      Map(id("a") -> Vector(TInt), id("b") -> Vector(TBool), id("c") -> Vector(TInt)),
      Map(id("a") -> Vector(TString), id("b") -> Vector(TBool), id("c") -> Vector(TString)),
      Map(id("a") -> Vector(TString), id("b") -> Vector(TBool), id("c") -> Vector(TInt)))

    assertEquals(result, expected)
  }

  test("product append: multiple types in a variant") {
    val start: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt, TString)),
      Map(id("a") -> Vector(TString, TInt)))
    val append: Mono.solve.Variants = List(
      id("b") -> Vector(TBool, TBool),
      id("b") -> Vector(TInt, TInt))
    val expected: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt, TString), id("b") -> Vector(TBool, TBool)),
      Map(id("a") -> Vector(TInt, TString), id("b") -> Vector(TInt, TInt)),
      Map(id("a") -> Vector(TString, TInt), id("b") -> Vector(TBool, TBool)),
      Map(id("a") -> Vector(TString, TInt), id("b") -> Vector(TInt, TInt)))

    assertEquals(Mono.solve.mapProductAppend(start, append), expected)
  }

  // Solve

  test("solve: simple polymorphic function") {
    val constraints = List(
      Flow(Vector(TInt), id("a")),
      Flow(Vector(TString), id("a")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt), Vector(TString)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: monomorphic demand") {
    val constraints = List(Flow(Vector.empty, id("a")))
    val expected: Solution = Map(id("a") -> Set(Vector.empty))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: call to another polymorphic function") {
    val constraints = List(
      Flow(Vector(Var(id("b"), 0)), id("a")),
      Flow(Vector(TInt), id("a")),
      Flow(Vector(TString), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt), Vector(TString)),
      id("b") -> Set(Vector(TString)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: unconstrained variable contributes no variants") {
    val constraints = List(
      Flow(Vector(Var(id("none"), 0)), id("maybe")),
      Flow(Vector(TInt), id("maybe")))
    val expected: Solution = Map(
      id("maybe") -> Set(Vector(TInt)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: multiple type arguments") {
    val constraints = List(
      Flow(Vector(TInt, TString), id("a")),
      Flow(Vector(TBool, TChar), id("a")),
      Flow(Vector(TBool, TString), id("a")))
    val expected: Solution = Map(
      id("a") -> Set(
        Vector(TInt, TString),
        Vector(TBool, TChar),
        Vector(TBool, TString)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: swapped type arguments") {
    val constraints = List(
      Flow(Vector(Var(id("b"), 1), Var(id("b"), 0)), id("a")),
      Flow(Vector(TString, TBool), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TBool, TString)),
      id("b") -> Set(Vector(TString, TBool)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: recursive polymorphic function") {
    val constraints = List(
      Flow(Vector(Var(id("a"), 0)), id("a")),
      Flow(Vector(TInt), id("a")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: mutually recursive polymorphic functions") {
    val constraints = List(
      Flow(Vector(Var(id("b"), 0)), id("a")),
      Flow(Vector(Var(id("a"), 0)), id("b")),
      Flow(Vector(TInt), id("a")),
      Flow(Vector(TString), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt), Vector(TString)),
      id("b") -> Set(Vector(TInt), Vector(TString)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: product of variables") {
    val constraints = List(
      Flow(Vector(TInt), id("a")),
      Flow(Vector(TString), id("a")),
      Flow(Vector(TBool), id("b")),
      Flow(Vector(Var(id("a"), 0), Var(id("b"), 0)), id("c")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt), Vector(TString)),
      id("b") -> Set(Vector(TBool)),
      id("c") -> Set(Vector(TInt, TBool), Vector(TString, TBool)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: correlated components from one variable") {
    val constraints = List(
      Flow(Vector(TInt, TString), id("a")),
      Flow(Vector(TChar, TBool), id("a")),
      Flow(Vector(Var(id("a"), 0), Var(id("a"), 1)), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt, TString), Vector(TChar, TBool)),
      id("b") -> Set(Vector(TInt, TString), Vector(TChar, TBool)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: nested constraints") {
    val constraints = List(
      Flow(Vector(Data(id("Weighted"), List(Var(id("b"), 0)))), id("a")),
      Flow(Vector(TInt), id("b")))
    val expected: Solution = Map(
      id("b") -> Set(Vector(TInt)),
      id("a") -> Set(Vector(Data(id("Weighted"), List(TInt)))))

    assertEquals(Mono.solve(constraints), expected)
  }

  private def collectConstraints(input: ModuleDecl): Flows = Mono.collect(input)

  private def showTypeArg(arg: TypeArg): String = arg match {
    case Data(tpe, Nil) => tpe.name.name
    case Data(tpe, targs) => s"${tpe.name.name}[${targs.map(showTypeArg).mkString(", ")}]"
    case Var(owner, position) => s"${owner.name.name}.$position"
    case Boxed(tpe, captures) =>
      ReparsablePrettyPrinter.format(ValueType.Boxed(tpe, captures))
  }

  private def showConstraint(constraint: Flow): String =
    s"${constraint.from.map(showTypeArg).mkString("<", ", ", ">")} <: ${constraint.to.name.name}"

  private def showConstraints(input: ModuleDecl): String =
    collectConstraints(input)
      .map(showConstraint)
      .distinct
      .sorted
      .mkString("\n")

  private def showSolution(input: ModuleDecl): String =
    try {
      val bindings = Mono.solve(collectConstraints(input))
        .toList
        .sortBy { case (parameter, _) => (parameter.name.name, parameter.id) }
        .map { case (parameter, variants) =>
          val image = variants
            .map(_.map(showTypeArg).mkString("<", ", ", ">"))
            .toList
            .sorted
            .mkString("{ ", ", ", " }")
          s"${parameter.name.name} ↦ $image"
        }
      if bindings.isEmpty then "S = ∅" else bindings.mkString("S = ", ",\n    ", "")
    } catch {
      case FatalPhaseError(message) =>
        s"no finite solution: ${PlainMessaging().formatContent(message)}"
    }

  registerCoreIRTests(
    new File("examples/core/mono"),
    CoreIRTransform("MONO_PREPROCESS", Mono.preprocess.apply),
    CoreIRAnalysis("MONO_COLLECT_CONSTRAINTS", showConstraints),
    CoreIRAnalysis("MONO_SOLVE", showSolution),
    CoreIRTransform("MONO_SPECIALIZE", input =>
      Mono.specialize(input, Mono.solve(Mono.collect(input)))))
}
