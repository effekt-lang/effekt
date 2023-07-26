package effekt
import effekt.context.Annotations
import effekt.context.Context
import kiama.util.{Source, StringSource, FileSource}
import effekt.typer.{Typer,Wellformedness}
import effekt.namer.Namer
import effekt.lifted.ModuleDecl
import effekt.util.messages
import effekt.context.IOModuleDB

abstract class AbstractTyperTests extends munit.FunSuite {


  /** A test that first runs the frontend on `input` and then `body`, where the [[Context]] is made available */
  def testTyper(name: String)(src: Source)(body: Context => Unit): Unit = {
    test(name) {
      val compiler = new effekt.Driver {}
      val configs = compiler.createConfig(Seq(
        "--server"
      ))
      configs.verify()
      compiler.context.setup(configs)
      val Frontend = Parser andThen Namer andThen Typer andThen Wellformedness

      given C: Context = compiler.context

      val frontend = Frontend(src)
      val mod = frontend.map(_.mod)
      assert(mod.isDefined, "Running the frontend succeeds")
      compiler.compileSource(src, configs)
      val ctx = compiler.context
      ctx.using(module = mod.get) {
        // run body
        body(ctx)
      }
    }
  }
  def testTyper(name: String)(input: String)(body: Context => Unit): Unit =
    testTyper(name)(StringSource(input, name))(body)
  def testTyperFile(name: String)(filename: String)(body: Context => Unit): Unit =
    testTyper(name)(FileSource(filename))(body)
  extension(C: Context) {
    /** Assert that the unique symbol named `name` has the expected value type */
    def assertValueType(name: String, expected: String, clue: => Any = "value types don't match"): Unit = {
      val syms = C.module.terms(name)
      assert(syms.size == 1, s"There is a unique symbol named '${name}'.")
      val sym = syms.head
      assert(sym.isInstanceOf[symbols.ValueSymbol], s"${sym} is a value symbol.")
      val got = C.annotation(Annotations.ValueType, sym.asInstanceOf[symbols.ValueSymbol])
      assertNoDiff(symbols.TypePrinter.show(got), expected, clue)
    }
    /** Assert that the unique symbol named `name` has the expected block type */
    def assertBlockType(name: String, expected: String, clue: => Any = "block types don't match"): Unit = {
      val syms = C.module.terms(name)
      assert(syms.size == 1, s"There is a unique symbol named '${name}'.")
      val sym = syms.head
      assert(sym.isInstanceOf[symbols.BlockSymbol], s"${sym} is a block symbol.")
      val got = C.annotation(Annotations.BlockType, sym.asInstanceOf[symbols.BlockSymbol])
      assertNoDiff(symbols.TypePrinter.show(got), expected, clue)
    }

    def assertCaptureType(name: String, expected: String, clue: => Any = "capture types don't match"): Unit = {
      val syms = C.module.terms(name)
      assert(syms.size == 1, s"There is a unique symbol named '${name}'.")
      val sym = syms.head
      assert(sym.isInstanceOf[symbols.Captures], s"${sym} is a capture symbol.")
      val got = C.annotation(Annotations.Captures, sym.asInstanceOf[symbols.BlockSymbol])
      assertNoDiff(symbols.TypePrinter.show(got), expected, clue)
    }

    // TODO further assertions (e.g. for captures etc) on the context
  }

}
class TyperTests extends AbstractTyperTests {

  testTyper("Simple example test")(
    """val a : _ = 1
      |""".stripMargin
  ){ C =>
    C.assertBlockType("a", "Int")
  }

  testTyperFile("File test")("examples/pts/typerTest.effekt"){
    C => C.assertValueType("a", "Int")
  }

  testTyperFile("Value type test")("examples/pts/valueType.effekt"){
    C => C.assertValueType("num1", "Int")
  }

//  // Expected an expression, but got a block. in line 12, 28
//  testTyperFile("Simple capture test")("examples/pts/capture2.effekt"){
//    C => C.assertCaptureType("result", "() => Unit at { eff1 }")
//  }

  testTyperFile("Nested types test")("examples/pts/nestedTypes.effekt"){
    C => C.assertValueType("map1", "Map[Int, String]")
  }

}
