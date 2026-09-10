package effekt
package core

import java.io.File

import Mono.{Flow, Flows, FlowType, FlowVar, GroundType, MonoBlockType, MonoCapture, Projection, Solution}
import Mono.MonoValueType.*
import effekt.util.PlainMessaging
import effekt.util.messages.FatalPhaseError


class MonoTests extends CoreTests {

  private val TInt: GroundType = Data(Id("Int"), Nil)
  private val TString: GroundType = Data(Id("String"), Nil)
  private val TChar: GroundType = Data(Id("Char"), Nil)
  private val TBool: GroundType = Data(Id("Bool"), Nil)

  private var flowVars: Map[String, FlowVar] = Map.empty

  private def id(name: String): FlowVar =
    flowVars.getOrElse(name, {
      val result = Id(name)
      flowVars += name -> result
      result
    })

  private def variable(owner: String, position: Int): FlowType =
    Var(Projection(id(owner), position))

  // Preprocess

  test("preprocess: shadowed alpha-equivalent boxed schemas share one encoding") {
    def definition(name: String): Toplevel.Def = {
      val outer = Id("A")
      val inner = Id("A")
      val parameter = Id("f")
      val thunk = BlockType.Function(Nil, Nil, Nil, Nil, ValueType.Var(inner))
      val schema = BlockType.Function(
        List(inner),
        Nil,
        List(ValueType.Boxed(thunk, Set.empty)),
        Nil,
        ValueType.Var(inner)
      )

      Toplevel.Def(
        Id(name),
        BlockLit(
          List(outer),
          Nil,
          Nil,
          List(BlockParam(parameter, schema, Set(parameter))),
          Return(Literal((), Type.TUnit))
        )
      )
    }

    val result = Mono.preprocess(ModuleDecl(
      "shadowing",
      Nil,
      Nil,
      Nil,
      List(definition("first"), definition("second")),
      Nil
    ))
    val encodings = result.declarations.collect { case interface: Declaration.Interface => interface }

    assertEquals(encodings.size, 1)
    assertEquals(encodings.head.tparams, Nil)
  }

  // Substitution product

  test("substitution product: empty product has no combinations") {
    val start: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt)),
      Map(id("a") -> Vector(TString)))

    assertEquals(Mono.solve.mapProductAppend(start, Nil), Nil)
  }

  test("substitution product: single product") {
    val start: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt)),
      Map(id("a") -> Vector(TString)))
    val append: Mono.solve.Variants = List(id("b") -> Vector(TBool))
    val expected: Mono.solve.Substitutions = List(
      Map(id("a") -> Vector(TInt), id("b") -> Vector(TBool)),
      Map(id("a") -> Vector(TString), id("b") -> Vector(TBool)))

    assertEquals(Mono.solve.mapProductAppend(start, append), expected)
  }

  test("substitution product: multiple products") {
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

  test("substitution product: multiple types in a variant") {
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
      Flow(Vector(variable("b", 0)), id("a")),
      Flow(Vector(TInt), id("a")),
      Flow(Vector(TString), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt), Vector(TString)),
      id("b") -> Set(Vector(TString)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: unconstrained variable contributes no variants") {
    val constraints = List(
      Flow(Vector(variable("none", 0)), id("maybe")),
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
      Flow(Vector(variable("b", 1), variable("b", 0)), id("a")),
      Flow(Vector(TString, TBool), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TBool, TString)),
      id("b") -> Set(Vector(TString, TBool)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: recursive polymorphic function") {
    val constraints = List(
      Flow(Vector(variable("a", 0)), id("a")),
      Flow(Vector(TInt), id("a")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: recursive permutation is finite") {
    val constraints = List(
      Flow(Vector(variable("a", 1), variable("a", 0)), id("a")),
      Flow(Vector(TInt, TString), id("a")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt, TString), Vector(TString, TInt)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: detects growth across parameter positions") {
    val constraints = List(
      Flow(Vector(
        variable("a", 1),
        Data(id("List"), List(variable("a", 0)))), id("a")),
      Flow(Vector(TInt, TString), id("a")))

    intercept[FatalPhaseError](Mono.solve(constraints))
  }

  test("solve: detects growth through mutual recursion") {
    val constraints = List(
      Flow(Vector(Data(id("List"), List(variable("b", 0)))), id("a")),
      Flow(Vector(variable("a", 0)), id("b")),
      Flow(Vector(TInt), id("a")))

    intercept[FatalPhaseError](Mono.solve(constraints))
  }

  test("solve: detects growth inside boxed types") {
    val recursiveBox: FlowType = Boxed(
      MonoBlockType.Function(0, 0, Nil, Nil, variable("a", 0)),
      Set.empty)
    val constraints = List(
      Flow(Vector(recursiveBox), id("a")),
      Flow(Vector(TInt), id("a")))

    intercept[FatalPhaseError](Mono.solve(constraints))
  }

  test("solve: mutually recursive polymorphic functions") {
    val constraints = List(
      Flow(Vector(variable("b", 0)), id("a")),
      Flow(Vector(variable("a", 0)), id("b")),
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
      Flow(Vector(variable("a", 0), variable("b", 0)), id("c")))
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
      Flow(Vector(variable("a", 0), variable("a", 1)), id("b")))
    val expected: Solution = Map(
      id("a") -> Set(Vector(TInt, TString), Vector(TChar, TBool)),
      id("b") -> Set(Vector(TInt, TString), Vector(TChar, TBool)))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: nested constraints") {
    val constraints = List(
      Flow(Vector(Data(id("Weighted"), List(variable("b", 0)))), id("a")),
      Flow(Vector(TInt), id("b")))
    val expected: Solution = Map(
      id("b") -> Set(Vector(TInt)),
      id("a") -> Set(Vector(Data(id("Weighted"), List(TInt)))))

    assertEquals(Mono.solve(constraints), expected)
  }

  test("solve: free variables inside boxed types are solved") {
    val openBox: FlowType = Boxed(
      MonoBlockType.Function(0, 0, Nil, Nil, variable("a", 0)),
      Set.empty)
    val closedBox: GroundType = Boxed(
      MonoBlockType.Function(0, 0, Nil, Nil, TInt),
      Set.empty)
    val constraints = List(
      Flow(Vector(openBox), id("b")),
      Flow(Vector(TInt), id("a")))

    assertEquals(Mono.solve(constraints), Map(
      id("a") -> Set(Vector(TInt)),
      id("b") -> Set(Vector(closedBox))))
  }

  test("solve: unresolved variables inside boxed types are not ground") {
    val openBox: FlowType = Boxed(
      MonoBlockType.Function(0, 0, Nil, Nil, variable("a", 0)),
      Set.empty)

    assertEquals(Mono.solve(List(Flow(Vector(openBox), id("b")))), Map(
      id("b") -> Set.empty))
  }

  test("solve: locally bound variables inside boxed types are ground") {
    val polymorphicBox: GroundType = Boxed(
      MonoBlockType.Function(
        1,
        1,
        List(Bound(0, 0), Boxed(MonoBlockType.Interface(id("Thunk"), Nil), Set(MonoCapture.Bound(0, 0)))),
        Nil,
        Bound(0, 0)),
      Set.empty)

    assertEquals(Mono.solve(List(Flow(Vector(polymorphicBox), id("a")))), Map(
      id("a") -> Set(Vector(polymorphicBox))))
  }

  test("solve: ill-scoped bound variables inside boxed types are not ground") {
    val illScoped: FlowType = Boxed(
      MonoBlockType.Function(0, 0, Nil, Nil, Bound(0, 0)),
      Set.empty)

    assertEquals(Mono.solve(List(Flow(Vector(illScoped), id("a")))), Map(
      id("a") -> Set.empty))
  }

  private def collectConstraints(input: ModuleDecl): Flows = Mono.collect(input)

  private def showFlowType(arg: FlowType): String = arg match {
    case Data(tpe, Nil) => tpe.name.name
    case Data(tpe, targs) => s"${tpe.name.name}[${targs.map(showFlowType).mkString(", ")}]"
    case Var(Projection(owner, position)) => s"${owner.name.name}.$position"
    case Bound(level, position) => s"$level.$position"
    case Boxed(tpe, captures) =>
      s"${showBlockType(tpe)} at ${showCaptures(captures)}"
  }

  private def showBlockType(tpe: MonoBlockType[Projection]): String = tpe match {
    case MonoBlockType.Function(tarity, _, vparams, bparams, result) =>
      val tparams = if tarity == 0 then "" else List.tabulate(tarity)(i => s"'$i").mkString("[", ", ", "]")
      val values = vparams.map(showFlowType).mkString("(", ", ", ")")
      val blocks = if bparams.isEmpty then "" else bparams.map(showBlockType).mkString("{", ", ", "}")
      s"$tparams$values$blocks => ${showFlowType(result)}"
    case MonoBlockType.Interface(name, Nil) => name.name.name
    case MonoBlockType.Interface(name, targs) => s"${name.name.name}[${targs.map(showFlowType).mkString(", ")}]"
  }

  private def showCaptures(captures: Set[MonoCapture]): String =
    captures.toList.map {
      case MonoCapture.Bound(level, position) => s"$level.$position"
      case MonoCapture.Named(id) => id.name.name
    }.sorted.mkString("{", ", ", "}")

  private def showConstraint(constraint: Flow): String =
    s"${constraint.from.map(showFlowType).mkString("<", ", ", ">")} <: ${constraint.to.name.name}"

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
            .map(_.map(showFlowType).mkString("<", ", ", ">"))
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
