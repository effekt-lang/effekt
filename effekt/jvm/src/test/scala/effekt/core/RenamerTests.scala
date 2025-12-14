package effekt.core

import scala.collection.mutable

/**
 * This is testing the main/core.Renamer using the test/core.TestRenamer.
 */
class RenamerTests extends CoreTests {

  /**
   * Check that the renamed input preserves alpha-equivalence using [[assertAlphaEquivalent]]
   */
  def assertRenamingPreservesAlpha(input: String,
                                   clue: => Any = "Not renamed to given value",
                                   names: Names = Names(defaultNames))(using munit.Location) = {
    val pInput = parse(input, "input", names)
    val renamer = new Renamer(names, "renamed")
    val obtained = renamer(pInput)
    assertAlphaEquivalent(obtained, pInput, clue)
  }

  def assertDefsUnique(in: ModuleDecl,
                       clue: => Any = "Duplicate definition") = {
    val seen = mutable.HashSet.empty[Id]

    def isFresh(id: Id): Unit = {
      assert(!seen.contains(id), clue)
      seen.add(id)
    }

    object check extends Tree.Query[Unit, Unit] {
      override def empty = ()

      override def combine = (_, _) => ()

      override def visit[T](t: T)(visitor: Unit ?=> T => Unit)(using Unit): Unit = {
        visitor(t)
        t match {
          case m: ModuleDecl =>
            m.definitions.foreach { d => isFresh(d.id) }
          case d: Def => isFresh(d.id)
          case v: Val => isFresh(v.id)
          case l: Let => isFresh(l.id)
          case d: Declaration => isFresh(d.id)
          case e: Extern.Def =>
            isFresh(e.id)
            e.tparams.foreach(isFresh);
            e.vparams.foreach { p => isFresh(p.id) }
            e.bparams.foreach { p => isFresh(p.id) };
            e.cparams.foreach { p => isFresh(p) }
          case b: BlockLit =>
            b.tparams.foreach(isFresh);
            b.cparams.foreach(isFresh)
            b.vparams.foreach { p => isFresh(p.id) };
            b.bparams.foreach { p => isFresh(p.id) }
          case i: Implementation =>
            i.operations.foreach { o =>
              o.vparams.foreach { p => isFresh(p.id) }; o.bparams.foreach { p => isFresh(p.id) }
            }
          case _ => ()
        }
      }
    }
    check.query(in)(using ())
  }
  def assertRenamingMakesDefsUnique(input: String,
                                    clue: => Any = "Duplicate definition",
                                    names: Names = Names(defaultNames))(using munit.Location) = {
    val pInput = parse(input, "input", names)
    val renamer = new Renamer(names, "renamed")
    val obtained = renamer(pInput)
    assertDefsUnique(obtained, clue)
  }

  test("No bound local variables"){
    val code =
      """module main
        |
        |def foo = { () =>
        |  return (bar: (Int) => Int @ {})(baz:Int)
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
    assertRenamingMakesDefsUnique(code)
  }

  test("val binding"){
    val code =
      """module main
        |
        |def foo = { () =>
        |  val x = (foo:(Int)=>Int@{})(4) ;
        |  return x:Int
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
    assertRenamingMakesDefsUnique(code)
  }

  test("var binding"){
    val code =
      """module main
        |
        |def foo = { () =>
        |  var x @ global = (foo:(Int)=>Int@{})(4) ;
        |  return x:Int
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
    assertRenamingMakesDefsUnique(code)
  }

  test("function (value) parameters"){
    val code =
      """module main
        |
        |def foo = { (x:Int) =>
        |  return x:Int
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
    assertRenamingMakesDefsUnique(code)
  }

  test("match clauses"){
    val code =
      """module main
        |
        |type Data { X(a:Int, b:Int) }
        |def foo = { () =>
        |  12 match [Int] {
        |    X : {(aa:Int, bb:Int) => return aa:Int }
        |  }
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
    assertRenamingMakesDefsUnique(code)
  }

  test("type parameters"){
    val code =
      """module main
        |
        |def foo = { ['A](a: Identity[A]) =>
        |  return a:Identity[A]
        |}
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
    assertRenamingMakesDefsUnique(code)
  }

  test("pseudo recursive"){
    val code =
      """ module main
        |
        | def bar = { () => return 1 }
        | def main = { () =>
        |   def foo = { () => (bar : () => Unit @ {})() }
        |   def bar = { () => return 2 }
        |   (foo : () => Unit @ {})()
        | }
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
    assertRenamingMakesDefsUnique(code)
  }
  test("shadowing let bindings"){
    val code =
      """ module main
        |
        | def main = { () =>
        |   let x = 1
        |   let x = 2
        |   return x:Int
        | }
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
    assertRenamingMakesDefsUnique(code)
  }
  test("shadowing let bindings inside a def") {
    val code =
      """ module main
        |
        | def main = { () =>
        |   def foo = { () =>
        |     let x = 1
        |     return x: Int
        |   }
        |   let x = 2
        |   def bar = { () =>
        |     let x = 3
        |     return x: Int
        |   }
        |   return x:Int
        | }
        |""".stripMargin
    assertRenamingPreservesAlpha(code)
  }
}
