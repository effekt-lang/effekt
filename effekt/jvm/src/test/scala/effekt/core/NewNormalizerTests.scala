package effekt.core

import effekt.PhaseResult.CoreTransformed
import effekt.context.{Context, IOModuleDB}
import effekt.core.optimizer.{NewNormalizer, Normalizer}
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
                                      externNames: List[String]
                                    )(using Location): Unit = {

    def findDef(mod: ModuleDecl, name: String) =
      mod.definitions.find(_.id.name.name == name)
        .getOrElse(throw new NoSuchElementException(s"Definition '$name' not found"))

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

    def compareOneDef(name: String): Unit = {
      val aDef = findDef(actual, name)
      val eDef = findDef(expected, name)

      val canon = Id(name)
      val pairs: Map[Id, Id] =
        (List(aDef.id -> canon, eDef.id -> canon) ++ externPairs).toMap

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
    val input =
      """
        |extern def foo: Int = vm"42"
        |
        |def run(): Int = {
        |    val f = box {
        |      foo
        |    } at { io }
        |
        |    val x = f()()
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
        |  let x = !(foo : () => Int @ {io})()
        |  return x: Int
        |}
        |""".stripMargin
      )

    val (mainId, actual) = normalize(input)

    assertAlphaEquivalentToplevels(actual, expected, List("run"), List("foo"))
  }

  // This example shows a box that cannot be normalized away
  test("box passed to extern") {
    val input =
      """
        |extern {io} def foo(f: => Int at {}): Int = vm"42"
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
              |    let x: Int =
              |      ! (foo: (=> Int at {}) => Int @ {io})(f_box: => Int at {})
              |
              |    return x: Int
              |}
              |""".stripMargin
      )

    val (mainId, actual) = normalize(input)

    assertAlphaEquivalentToplevels(actual, expected, List("run"), List("foo"))
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

  lazy val Optimized = allToCore(Core) andThen Aggregate andThen core.optimizer.Optimizer map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.ensureMainExists(mod)
      val dontInline = NewNormalizer { (id, b) => false }
      val normalTree = dontInline.run(core)
      Normalizer.assertNormal(normalTree)
      (mainSymbol, mod, normalTree)
  }
}
