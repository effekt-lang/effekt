package effekt.core

import effekt.PhaseResult.CoreTransformed
import effekt.context.{Context, IOModuleDB}
import effekt.core.optimizer.{Deadcode, NewNormalizer, Normalizer, Optimizer}
import effekt.util.PlainMessaging
import effekt.*
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{Source, StringSource}
import munit.Location

class NewNormalizerTests extends CoreTests {
  object plainMessaging extends PlainMessaging
  object context extends Context with IOModuleDB {
    val messaging = plainMessaging

    object frontend extends NormalizeOnly

    override lazy val compiler = frontend.asInstanceOf
  }

  def compileString(content: String): (Id, symbols.Module, ModuleDecl) =
    val config = new EffektConfig(Seq("--Koutput", "string"))
    config.verify()
    context.setup(config)
    context.frontend.compile(StringSource(content, "input.effekt"))(using context).map {
      case (_, decl) => decl
    }.getOrElse {
      val errors = plainMessaging.formatMessages(context.messaging.buffer)
      sys error errors
    }

  def normalize(contents: String): (Id, core.ModuleDecl) = {
    val (main, mod, decl) = compileString(contents)
    (main, decl)
  }

  def assertAlphaEquivalentToplevels(
                                      actual: ModuleDecl,
                                      expected: ModuleDecl,
                                      defNames: List[String],
                                      externNames: List[String] = List(),
                                      declNames: List[String] = List(),
                                      ctorNames: List[(String, String)] = List()
                                    )(using Location): Unit = {

    def findDef(mod: ModuleDecl, name: String) =
      mod.definitions.find(_.id.name.name == name)
        .getOrElse(throw new NoSuchElementException(s"Definition '$name' not found"))

    def findDecl(mod: ModuleDecl, name: String)=
      mod.declarations.find(_.id.name.name == name)
        .getOrElse(throw new NoSuchElementException(s"Declaration '$name' not found"))

    def findCtor(data: Data, name: String) =
      data.constructors.find(_.id.name.name == name)
        .getOrElse(throw new NoSuchElementException(
          s"Constructor '$name' not found in data '${data.id.name.name}'"
        ))

    def findExternDef(mod: ModuleDecl, name: String) =
      mod.externs.collect { case d@Extern.Def(_, _, _, _, _, _, _, _) => d }
        .find(_.id.name.name == name)
        .getOrElse(throw new NoSuchElementException(s"Extern def '$name' not found"))

    val externPairs: List[(Id, Id)] =
      externNames.flatMap { name =>
        val canon = Id(name)
        List(
          findExternDef(actual, name).id -> canon,
          findExternDef(expected, name).id -> canon
        )
      }

    val declPairs: List[(Id, Id)] =
      declNames.flatMap { name =>
        val canon = Id(name)
        List(
          findDecl(actual, name).id -> canon,
          findDecl(expected, name).id -> canon
        )
      }

    val ctorPairs: List[(Id, Id)] =
      ctorNames.flatMap { case (dataName, ctorName) =>
        val canon = Id(ctorName)
        val actualData = findDecl(actual, dataName) match {
          case d: Data => d
          case _: Interface => throw new IllegalArgumentException(
            s"Expected data declaration for '$dataName', found interface"
          )
        }
        val expectedData = findDecl(expected, dataName) match {
          case d: Data => d
          case _: Interface => throw new IllegalArgumentException(
            s"Expected data declaration for '$dataName', found interface"
          )
        }
        List(
          findCtor(actualData, ctorName).id -> canon,
          findCtor(expectedData, ctorName).id -> canon
        )
      }

    def compareOneDef(name: String): Unit = {
      val aDef = findDef(actual, name)
      val eDef = findDef(expected, name)

      val canon = Id(name)
      val pairs: Map[Id, Id] =
        (List(aDef.id -> canon, eDef.id -> canon) ++ externPairs ++ declPairs ++ ctorPairs).toMap

      val renamer = TestRenamer(Names(defaultNames), "$", List(pairs))
      shouldBeEqual(
        renamer(aDef),
        renamer(eDef),
        s"Top-level '$name' is not alpha-equivalent"
      )
    }

    defNames.foreach(compareOneDef)
  }

  // This example shows a box that contains an extern reference.
  // The normalizer is able to unbox this indirection away.
  test("extern in box") {
    val input = """
        |extern def foo: Int = vm"42"
        |
        |def run(): Int = {
        |    val f = box {
        |      box foo
        |    } at { io }
        |
        |    val x = /* unbox */ f()()
        |    return x
        |}
        |
        |def main() = println(run())
        |""".stripMargin

    val expected = parse(
      """
        |module input
        |
        |extern {io} def foo(): Int = vm"42"
        |
        |def run() = {
        |  def f1() = {
        |    def f2() = {
        |      let ! x = (foo: () => Int @ {io})()
        |      return x: Int
        |    }
        |    let y = box {io} f2: () => Int @ {io}
        |    return y: () => Int at {io}
        |  }
        |  val z: () => Int at {io} = (f1: () => (() => Int at {io}) @ {io})();
        |  def r = unbox z: () => Int at {io}
        |  (r: () => Int @ {io})()
        |}
        |
        |""".stripMargin)


    val (mainId, actual) = normalize(input)

    assertAlphaEquivalentToplevels(actual, expected, List("run"), List("foo"))
  }

  // This example shows a box that cannot be normalized away.
  // This is because the box is passed to an extern definition.
  test("box passed to extern") {
    val input =
      """
        |extern def foo(f: => Int at {}) at {io}: Int = vm"42"
        |
        |def run(): Int = {
        |    val f = box {
        |      42
        |    } at {}
        |
        |    val x = foo(f)
        |    return x
        |}
        |
        |def main() = println(run())
        |""".stripMargin

    val expected =
      parse("""
              |module input
              |
              |extern {io} def foo(): Int = vm"42"
              |
              |def run() = {
              |    def f() = {
              |        let x = 42
              |        return x: Int
              |    }
              |    let f_box: => Int at {} = box {} (f: => Int @ {})
              |    let ! x = (foo: (=> Int at {}) => Int @ {io})(f_box: => Int at {})
              |
              |    return x: Int
              |}
              |""".stripMargin
      )

    val (mainId, actual) = normalize(input)

    assertAlphaEquivalentToplevels(actual, expected, List("run"), List("foo"))
  }

  // This example shows an unbox that cannot be normalized away.
  // This is because the box is retrieved from an extern definition.
  test("unbox blocked by extern") {
    val input =
      """
        |extern def foo() at {}: => Int at {} = vm"42"
        |
        |def run(): Int = {
        |    val x = foo()()
        |    return x
        |}
        |
        |def main() = println(run())
        |""".stripMargin

    val expected =
      parse("""
              |module input
              |
              |extern {} def foo(): => Int at {} = vm"42"
              |
              |def run() = {
              |    let f: => Int at {} =
              |      (foo: => (=> Int at {}) @ {})()
              |    def f_unbox = unbox f: => Int at {}
              |    (f_unbox: => Int @ {})()
              |}
              |""".stripMargin
      )

    val (mainId, actual) = normalize(input)

    assertAlphaEquivalentToplevels(actual, expected, List("run"), List("foo"))
  }

  // One might expect the following example to constant fold.
  // However, extern definitions such as infixAdd are currently always neutral.
  test("Extern infixAdd blocks constant folding of mutable variable") {
    val input =
      """
        |def run(): Int = {
        |    var x = 41
        |    x = x + 1
        |    return x
        |}
        |
        |def main() = println(run())
        |""".stripMargin

    val expected =
      parse("""
              |module input
              |
              |extern {} def infixAdd(x: Int, y: Int): Int = vm ""
              |
              |def run() = {
              |   let x1 = 41
              |   let x2 = 1
              |   let x3 = (infixAdd: (Int, Int) => Int @ {})(x1: Int, x2: Int)
              |   return x3: Int
              |}
              |""".stripMargin
      )

    val (mainId, actual) = normalize(input)

    assertAlphaEquivalentToplevels(actual, expected, List("run"), List("infixAdd"))
  }

  // This test case shows mutable variable assignments turning into static let-bindings
  test("Mutable Peano Nats turn into let-bindings") {
    val input =
      """
        |type Nat {
        |  Z()
        |  S(pred: Nat)
        |}
        |
        |def toInt(n: Nat): Int = n match {
        |  case Z() => 0
        |  case S(pred) => 1 + toInt(pred)
        |}
        |
        |def run(): Nat = {
        |    var x = Z()
        |    x = S(x)
        |    return x
        |}
        |
        |def main() = println(run().toInt())
        |""".stripMargin

    val expected =
      parse("""
              |module input
              |
              |type Nat {
              |  Z()
              |  S(pred: Nat)
              |}
              |
              |def run() = {
              |   let x1 = make Nat Z()
              |   let x2 = make Nat S(x1: Nat)
              |   return x2: Nat
              |}
              |""".stripMargin
      )

    val (mainId, actual) = normalize(input)

    assertAlphaEquivalentToplevels(actual, expected, List("run"), List(), List("Nat"), List(("Nat", "Z"), ("Nat", "S")))
  }

  // This test case shows a mutable variable that is captured in an effect handler.
  // The resulting core code shows how the reference is passed to the handler block.
  // Even though the variable is not mutated, the normalizer currently cannot eliminate the reference.
  // This is because the stack used to normalize the handler is currently treated as "unknown".
  test("Mutable variable read in handler") {
    val input =
      """
        |effect bar: Unit
        |
        |extern def foo(x: Int) at {io}: Unit = vm""
        |
        |def run() = {
        |    var x = 1
        |    try {
        |        do bar()
        |    } with bar {
        |        foo(x)
        |    }
        |}
        |
        |def main() = println(run())
        |
        |""".stripMargin

    val (mainId, actual) = normalize(input)

    val expected =
      parse("""
              |module input
              |
              |interface bar {
              |  bar: => Unit
              |}
              |
              |extern {io} def foo(x: Int): Unit = vm""
              |
              |def run() = {
              |  let y = 1
              |
              |  def handle(){q @ p: Prompt[Unit]}{s @ r: Ref[Int]} =
              |    shift (q: Prompt[Unit] @ {p}) {
              |      () { k @ p: Resume[Unit, Unit]} => {
              |        get z: Int = !s @ r;
              |        let ! o = (foo: (Int) => Unit @ {io})(z: Int)
              |        return o: Unit
              |      }
              |    }
              |
              |  var z @ x = y: Int;
              |  reset {
              |    () { q @ p: Prompt[Unit] } =>
              |      (handle: { p: Prompt[Unit] }{ x: Ref[Int] } => Unit @ {})() { q: Prompt[Unit] @ { p } } { z: Ref[Int] @ { x } }
              |  }
              |}
              |""".stripMargin
      )

    assertAlphaEquivalentToplevels(actual, expected, List("run"), declNames=List("bar"), externNames=List("foo"))
  }

  // This test case shows a mutable variable passed to the identity function.
  // Currently, the normalizer is not able to see through the identity function,
  // but it does ignore the mutable variable and just passes the initial value.
  test("Pass mutable variable to identity function uses let binding") {
    val input =
      """
        |def run(): Int = {
        |    def f(x: Int) = x
        |    var x = 42
        |    f(x)
        |}
        |
        |def main() = println(run())
        |
        |""".stripMargin

    val (mainId, actual) = normalize(input)

    val expected =
      parse(
        """
          |module input
          |
          |def run() = {
          |    def f(x: Int) = {
          |        return x: Int
          |    }
          |
          |    let y = 42
          |    var x @ z = y: Int;
          |    (f : (Int) => Int @ {})(y: Int)
          |}
          |""".stripMargin
      )

    assertAlphaEquivalentToplevels(actual, expected, List("run"))
  }

  // This test shows that when the value of a mutable variable is known, we can directly pass the let-bound value.
  test("Mutate mutable variable before passing it to identity function") {
    val input =
      """
        |def run(): Int = {
        |    def f(x: Int) = x
        |    var x = 42
        |    x = 43
        |    f(x)
        |}
        |
        |def main() = println(run())
        |
        |""".stripMargin

    val (mainId, actual) = normalize(input)

    val expected =
      parse(
        """
          |module input
          |
          |def run() = {
          |    def f(x: Int) = {
          |        return x: Int
          |    }
          |
          |    let y = 42
          |    let w = 43
          |    var x @ z = y: Int;
          |    (f : (Int) => Int @ {})(w: Int)
          |}
          |""".stripMargin
      )

    assertAlphaEquivalentToplevels(actual, expected, List("run"))
  }

  // This test shows a mutable reference captured by a block parameter.
  // During normalization, this block parameter gets lifted to a `def`.
  // One might hope for this mutable variable to be eliminated entirely,
  // but currently the normalizer does not inline definitions.
  test("Block param capturing mutable reference can be lifted") {
    val input =
      """
        |def run(): Int = {
        |  def modifyProg { setter: Int => Unit }: Unit = {
        |    setter(2)
        |    ()
        |  }
        |  var x = 1
        |  modifyProg { y => x = y }
        |  x
        |}
        |
        |def main() = println(run())
        |""".stripMargin

    val (mainId, actual) = normalize(input)

    val expected =
      parse(
        """
          |module input
          |
          |def run() = {
          |  def modifyProg(){setter: (Int) => Unit} = {
          |    let x = 2
          |    val tmp: Unit = (setter: (Int) => Unit @ {setter})(x: Int);
          |    let y = ()
          |    return y: Unit
          |  }
          |  let y = 1
          |  var x @ c = y: Int;
          |  def f(y: Int) = {
          |    put x @ c = y: Int;
          |    let z = ()
          |    return z: Unit
          |  }
          |  val tmp: Unit = (modifyProg: (){setter : (Int) => Unit} => Unit @ {})(){f: (Int) => Unit @ {c}};
          |  get o: Int = !x @ c;
          |  return o: Int
          |}
          |""".stripMargin
      )

    assertAlphaEquivalentToplevels(actual, expected, List("run"))
  }

  // This test case shows that we can normalize a potentially recursive block that never actually recurses.
  // The while loop in the surface language is translated to a recursive block in core.
  // Note that the variable `v` and its capture need to be correctly passed around.
  test("Can normalize while loop with non-satisfiable condition") {
    val input =
      """
        |def run() = {
        |  var v = 2;
        |  while (v <= 1) {}
        |  0
        |}
        |
        |def main() = println(run())
        |""".stripMargin

    // Does not throw
    normalize(input)
  }

  test("Mutable variables are added as an extra parameter") {
    val input =
      """
      |def main() = {
      |  var x = 1
      |  def update() = { x = 2 }
      |  update()
      |  println(x)
      |}
      """.stripMargin
    
    normalize(input)
  }

  test("Reset/Shift with mutable variable") {
    val input =
      """
      |effect Eff(): Unit
      |def main() = {
      |  var x = 0
      |  try { 
      |    x = 1
      |    do Eff()
      |  } with Eff { resume(()) }
      |  println(x)
      |}
      """.stripMargin
    
    normalize(input)
  }

  test("Mutable variable and recursive function") {
    val input =
      """
      |def main() = {
      |  var x = 0
      |  def loop(n: Int): Unit = {
      |    if (n == 0) ()
      |    else {
      |      x = x + 1
      |      loop(n - 1)
      |    }
      |  }
      |  loop(5)
      |  println(x)
      |}
      """.stripMargin

    normalize(input)
  }
  
  test("basic region usage") {
    val input =
      """
        |def main() = {
        |  val y = region r {
        |    var x in r = 42
        |    x = x + 1
        |    x
        |  }
        |  println(y)
        |}
        |""".stripMargin

    normalize(input)
  }
  
  test("region parameter") {
    val input =
      """
        |def main() = {
        |  val y = region reg {
        |    def foo(init: Int) {r: Region} = {
        |      var x in r = init
        |      x = x + 1
        |      x
        |    }
        |    foo(42) {reg}
        |  }
        | println(y)
        |}
        |""".stripMargin
        
    normalize(input)
  }

  test("Can lookup top-level def") {
    val input =
      """
        |def top(): Int = 43
        |
        |def main() = {
        |  val x = top() + top()
        |  println(x.show)
        |}
        |""".stripMargin

    // Does not throw
    normalize(input)
  }

  test("Can lookup top-level val") {
    val input =
      """
        |val top: Int = 43
        |
        |def main() = {
        |  val x = top + top
        |  println(x.show)
        |}
        |""".stripMargin

    // Does not throw
    normalize(input)
  }

  // This case tests a subtle aspect of normalizing with block parameters.
  // Consider the provided input program.
  // The normalized program looks as follows, with irrelevant details elided:
  // ```scala
  // () {
  //  def break = ...
  //  def run = ...
  //  let x = 1
  //  def f = (v: Int){f} {y} {p} {
  //    let x = 2
  //    y := x
  //    jump break(){p}
  //  }
  //  val o = reset {{p} =>
  //    var y = x
  //    val tmp = jump run(){f @ [y, p]}
  //    let z = ()
  //    return z
  //  }
  //  jump println(o)
  // }
  // ```
  // As you can see, the block argument supplied to block parameter `prog` of `run` is lifted to a `def f`.
  // This definition needs to abstract over all free captures and variables in the body.
  // In particular, the call side of `f` needs to supply the correct closure.
  // Therefore, it is not possible to simply call `run` as follows:
  // ```scala
  // jump run(){f}
  // ```
  // where `f` would be passed as a block variable.
  // Instead, we need to "eta-expand" this parameter to a block that calls f with the correct captures:
  // ```scala
  // jump run(){f @ [y, p]}
  // ```
  // where `y` and `p` are the correct captures from the reset body.
  test("Block parameters get lifted and captures are passed correctly") {
    val input: String =
      """
        |effect break(): Unit
        |
        |def main() = {
        |    val x = try {
        |      def run { prog: (Int) {() => Unit} => Unit }: Unit = prog(42) { () => () }
        |      var y = 1
        |      run { (v) { f } =>
        |        y = 2
        |        do break()
        |      }
        |      ()
        |    } with break {
        |        ()
        |    }
        |    println(x)
        |}
        |""".stripMargin

    // Does not throw
    normalize(input)
  }
}

/**
 * A "backend" that simply outputs the normalized core module.
 */
class NormalizeOnly extends Compiler[(Id, symbols.Module, ModuleDecl)] {

  def extension = ".effekt-core.ir"

  override def supportedFeatureFlags: List[String] = List("vm")

  override def prettyIR(source: Source, stage: Stage)(using C: Context): Option[Document] = None

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = None

  override def compile(source: Source)(using C: Context): Option[(Map[String, String], (Id, symbols.Module, ModuleDecl))] =
    Optimized.run(source).map { res => (Map.empty, res) }

  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  lazy val Optimized = allToCore(Core) andThen Aggregate map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.ensureMainExists(mod)
      var tree = Deadcode.remove(mainSymbol, core)
      val normalizer = NewNormalizer()
      tree = normalizer.run(tree)
      Normalizer.assertNormal(tree)
      (mainSymbol, mod, tree)
  }
}
