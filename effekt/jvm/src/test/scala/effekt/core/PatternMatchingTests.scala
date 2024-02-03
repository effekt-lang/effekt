package effekt
package core
import effekt.symbols
import kiama.util.StringSource

import PatternMatchingCompiler.*
import core.Type.{TBoolean, TInt, TUnit}

class PatternMatchingTests extends CoreTests {

  val SomeC    = Id("Some")
  val NoneC    = Id("None")
  val OptionId = Id("Option")
  def OptionT(tpe: core.ValueType): core.ValueType =
    core.ValueType.Data(OptionId, List(tpe))

  val trivalPredicate = core.Return(core.Literal(true, TBoolean))

  def block(name: String, args: ValueType*): core.BlockVar =
    core.BlockVar(Id(name), core.BlockType.Function(Nil, Nil, args.toList, Nil, TUnit), Set.empty)

  def jump(label: BlockVar, args: Pure*) =
    App(label, Nil, args.toList, Nil)

  def variable(name: String, tpe: ValueType): core.ValueVar =
     core.ValueVar(Id(name), tpe)


  test("Simple normalization with renamings") {

    val x = variable("x", TInt)
    val y = variable("y", TInt)
    val z = variable("z", TInt)
    val f = block("f", TInt)

    // case x is y; y is z => f(z)
    val normalized = normalize(Clause(
        List(
          Condition.Patterns(Map(x -> Pattern.Any(y.id))),
          Condition.Patterns(Map(y -> Pattern.Any(z.id)))),
        f, List(z)))

    // case => f(z)
    val expected = Clause(Nil, f, List(x))

    assertEquals(normalized, expected)
  }

  test("Sanity check: compiling empty list of clauses") {
    assertEquals(compile(Nil), core.Hole())
  }

  test("Simple guard") {
    //    sc match {
    //      case x and x > 0 => "hello"
    //      case _ => "world"
    //    }
    // ~=
    //    match {
    //      case sc is x; val p = return x > 0; p? => b1(v)
    //      case sc is _ => b2()
    //    }

    val sc = variable("sc", TInt)
    val x = variable("x", TInt)
    val p = variable("p", TBoolean)
    val b1 = block("b1", TInt)
    val b2 = block("b2")

    val result = compile(List(
      Clause(
        List(
          Condition.Patterns(Map(sc -> Pattern.Any(x.id))),
          Condition.Val(p.id, trivalPredicate),
          Condition.Predicate(p)),
        b1, List(x)),
      Clause(
        List(
          Condition.Patterns(Map(sc -> Pattern.Ignore()))),
        b2, List())))

    val expected =
      Val(p.id, trivalPredicate,
      If(p,
        jump(b1, sc),
        jump(b2)))

    assertAlphaEquivalentStatements(result, expected)


    //val names = Names(defaultNames ++ Map("sc" -> sc.id, "b1" -> b1.id, "b2" -> b2.id))
    //    assertAlphaEquivalentStatements(result, parseStatement(
    //      """
    //        |val p3 = return true;
    //        |if (p3: Boolean)
    //        |  (b1 : (Int) => Unit @ {})(sc: Int)
    //        |else
    //        |  (b2 : () => Unit @ {})()
    //        |
    //        |""".stripMargin,
    //      names = names
    //    ), names)
  }

  test("Match then guard") {

    //    opt match {
    //      case Some(v) and v > 0 => b1(v)
    //      case _ => b2()
    //    }
    // ~=
    //    match {
    //      case opt is Some(v); val p = return v > 0; p? => b1(v)
    //      case opt is _ => b2()
    //    }
    val opt = variable("opt", OptionT(TInt))
    val v = variable("v", TInt)
    val p = variable("p", TBoolean)
    val b1 = block("b1", TInt)
    val b2 = block("b2")

    val tmp = variable("tmp", TInt)

    val result = compile(List(
      Clause(
        List(
          Condition.Patterns(Map(opt -> Pattern.Tag(SomeC, List(Pattern.Any(v.id) -> TInt)))),
          Condition.Val(p.id, trivalPredicate),
          Condition.Predicate(p)),
        b1, List(v)),
      Clause(
        List(
          Condition.Patterns(Map(opt -> Pattern.Ignore()))),
        b2, List())))

    // opt match {
    //   case Some(tmp) => val p = return v > 0; if (p) { b1(tmp) } else { b2() }
    //   case _ => b2()
    // }
    val expected = Match(opt,
      List((SomeC, BlockLit(Nil, Nil, List(ValueParam(tmp.id, tmp.tpe)), Nil,
        Val(p.id, trivalPredicate, If(p,
          App(b1, Nil, List(tmp), Nil),
          App(b2, Nil, Nil, Nil)))))),
      Some(App(b2, Nil, Nil, Nil)))

    assertAlphaEquivalentStatements(result, expected)
  }
}
