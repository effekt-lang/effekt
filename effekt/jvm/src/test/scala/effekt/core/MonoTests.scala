package effekt
package core


abstract class AbstractMonoTests extends CorePhaseTests(Mono) {
    import TypeArg.*

    implicit def stringBaseT(name: String): Base = Base(Id(name), List())

    val BaseTInt: Base = "Int"
    val BaseTString: Base = "String"
    val BaseTChar: Base = "Char"
    val BaseTBool: Base = "Bool"
    val BaseTDouble: Base = "Double"

    val fnId: Map[String, FunctionId] = Map(
        "a" -> Id("a"),
        "b" -> Id("b"),
        "c" -> Id("c"),
        "d" -> Id("d"),
        "e" -> Id("e"),
        "f" -> Id("f"),
    )
}

class MonoTests extends AbstractMonoTests {

    import TypeArg.*

    test("simple polymorphic function") {
        val constraints = List(
            Constraint(Vector(BaseTInt), fnId("a")),
            Constraint(Vector(BaseTString), fnId("a"))
        )
        val expectedSolved: Solution = Map(
            fnId("a") -> Set(Vector(BaseTInt), Vector(BaseTString))
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("calling other polymorphic function") {
        val constraints = List(
            Constraint(Vector(Var(fnId("b"), 0)), fnId("a")),
            Constraint(Vector(BaseTInt), fnId("a")),
            Constraint(Vector(BaseTString), fnId("b")),
        )
        val expectedSolved: Solution = Map(
            fnId("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
            fnId("b") -> Set(Vector(BaseTString)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("polymorphic function with multiple type args") {
        val constraints = List(
            Constraint(Vector(BaseTInt, BaseTString), fnId("a")),
            Constraint(Vector(BaseTBool, BaseTChar), fnId("a")),
            Constraint(Vector(BaseTBool, BaseTString), fnId("a")),
        )
        val expectedSolved: Solution = Map(
            fnId("a") -> Set(
                Vector(BaseTInt, BaseTString),
                Vector(BaseTBool, BaseTChar),
                Vector(BaseTBool, BaseTString),
            )
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("calling other polymorphic function with type args swapped") {
        val constraints = List(
            Constraint(Vector(Var(fnId("b"), 1), Var(fnId("b"), 0)), fnId("a")),
            Constraint(Vector(BaseTString, BaseTBool), fnId("b")),
        )
        val expectedSolved: Solution = Map(
            fnId("a") -> Set(Vector(BaseTBool, BaseTString)),
            fnId("b") -> Set(Vector(BaseTString, BaseTBool)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("recursive polymorphic function") {
        val constraints = List(
            Constraint(Vector(Var(fnId("a"), 0)), fnId("a")),
            Constraint(Vector(BaseTInt), fnId("a")),
        )
        val expectedSolved: Solution = Map(
            fnId("a") -> Set(Vector(BaseTInt)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("mutually recursive polymorphic functions") {
        val constraints = List(
            Constraint(Vector(Var(fnId("b"), 0)), fnId("a")),
            Constraint(Vector(Var(fnId("a"), 0)), fnId("b")),
            Constraint(Vector(BaseTInt), fnId("a")),
            Constraint(Vector(BaseTString), fnId("b")),
        )
        val expectedSolved: Solution = Map(
            fnId("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
            fnId("b") -> Set(Vector(BaseTInt), Vector(BaseTString)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("correct product of vars") {
        val constraints = List(
            Constraint(Vector(BaseTInt), fnId("a")),
            Constraint(Vector(BaseTString), fnId("a")),
            Constraint(Vector(BaseTBool), fnId("b")),
            Constraint(Vector(Var(fnId("a"), 0), Var(fnId("b"), 0)), fnId("c")),
        )

        val expectedSolved: Solution = Map(
            fnId("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
            fnId("b") -> Set(Vector(BaseTBool)),
            fnId("c") -> Set(Vector(BaseTInt, BaseTBool), Vector(BaseTString, BaseTBool)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("nested constraints") {
        val constraints = List(
            Constraint(Vector(Base(fnId("Weighted"), List(Var(fnId("b"), 0)))), fnId("a")),
            Constraint(Vector(BaseTInt), fnId("b"))
        )

        val expectedSolved: Solution = Map(
            fnId("b") -> Set(Vector(BaseTInt)),
            fnId("a") -> Set(Vector(Base(fnId("Weighted"), List(BaseTInt))))
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }
}
