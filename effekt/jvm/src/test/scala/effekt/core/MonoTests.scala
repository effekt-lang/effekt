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

    var fnId: Map[String, FunctionId] = Map()

    def id(name: String): FunctionId = 
        fnId.getOrElse(name, {
            val storedId = Id(name)
            fnId += (name -> storedId)
            storedId
        })
}

class MonoProductAppendTests extends AbstractMonoTests {

    test("product append start empty append empty") {
        val start = List(List.empty)
        val append1 = List.empty

        val res = productAppend(start, append1)
        val expected = List(List.empty)

        assertEquals(res, expected)
    }

    test("product append start with empty list") {
        val start = List(List.empty)
        val append1 = List(1, 2)
        val append2 = List(3, 4)

        var res = productAppend(start, append1)
        res = productAppend(res, append2)

        val expected = List(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
        assertEquals(res, expected)
    }

    test("product append unequal number of added values") {
        val start = List(List(1)) 
        val append1 = List(2,3)
        val append2 = List(4)

        var res = productAppend(start, append1)
        res = productAppend(res, append2)
        val expected = List(List(1,2,4), List(1,3,4))
    }

}

class MonoTests extends AbstractMonoTests {

    import TypeArg.*

    test("simple polymorphic function") {
        val constraints = List(
            Constraint(Vector(BaseTInt), id("a")),
            Constraint(Vector(BaseTString), id("a"))
        )
        val expectedSolved: Solution = Map(
            id("a") -> Set(Vector(BaseTInt), Vector(BaseTString))
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("calling other polymorphic function") {
        val constraints = List(
            Constraint(Vector(Var(id("b"), 0)), id("a")),
            Constraint(Vector(BaseTInt), id("a")),
            Constraint(Vector(BaseTString), id("b")),
        )
        val expectedSolved: Solution = Map(
            id("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
            id("b") -> Set(Vector(BaseTString)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("polymorphic function with multiple type args") {
        val constraints = List(
            Constraint(Vector(BaseTInt, BaseTString), id("a")),
            Constraint(Vector(BaseTBool, BaseTChar), id("a")),
            Constraint(Vector(BaseTBool, BaseTString), id("a")),
        )
        val expectedSolved: Solution = Map(
            id("a") -> Set(
                Vector(BaseTInt, BaseTString),
                Vector(BaseTBool, BaseTChar),
                Vector(BaseTBool, BaseTString),
            )
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("calling other polymorphic function with type args swapped") {
        val constraints = List(
            Constraint(Vector(Var(id("b"), 1), Var(id("b"), 0)), id("a")),
            Constraint(Vector(BaseTString, BaseTBool), id("b")),
        )
        val expectedSolved: Solution = Map(
            id("a") -> Set(Vector(BaseTBool, BaseTString)),
            id("b") -> Set(Vector(BaseTString, BaseTBool)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("recursive polymorphic function") {
        val constraints = List(
            Constraint(Vector(Var(id("a"), 0)), id("a")),
            Constraint(Vector(BaseTInt), id("a")),
        )
        val expectedSolved: Solution = Map(
            id("a") -> Set(Vector(BaseTInt)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("mutually recursive polymorphic functions") {
        val constraints = List(
            Constraint(Vector(Var(id("b"), 0)), id("a")),
            Constraint(Vector(Var(id("a"), 0)), id("b")),
            Constraint(Vector(BaseTInt), id("a")),
            Constraint(Vector(BaseTString), id("b")),
        )
        val expectedSolved: Solution = Map(
            id("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
            id("b") -> Set(Vector(BaseTInt), Vector(BaseTString)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("correct product of vars") {
        val constraints = List(
            Constraint(Vector(BaseTInt), id("a")),
            Constraint(Vector(BaseTString), id("a")),
            Constraint(Vector(BaseTBool), id("b")),
            Constraint(Vector(Var(id("a"), 0), Var(id("b"), 0)), id("c")),
        )

        val expectedSolved: Solution = Map(
            id("a") -> Set(Vector(BaseTInt), Vector(BaseTString)),
            id("b") -> Set(Vector(BaseTBool)),
            id("c") -> Set(Vector(BaseTInt, BaseTBool), Vector(BaseTString, BaseTBool)),
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }

    test("nested constraints") {
        val constraints = List(
            Constraint(Vector(Base(id("Weighted"), List(Var(id("b"), 0)))), id("a")),
            Constraint(Vector(BaseTInt), id("b"))
        )

        val expectedSolved: Solution = Map(
            id("b") -> Set(Vector(BaseTInt)),
            id("a") -> Set(Vector(Base(id("Weighted"), List(BaseTInt))))
        )

        assertEquals(solveConstraints(constraints), expectedSolved)
    }
}
