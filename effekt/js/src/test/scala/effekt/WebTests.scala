package effekt

import kiama.util.{ FileSource, StringSource }

import utest._
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.collection.mutable

object WebTests extends TestSuite {

  class Loaded(var module: js.Dynamic, var timestamp: Long)

  def tests = Tests {

    val server = effekt.Effekt.LanguageServer()

    val loadedModules = mutable.Map.empty[String, Loaded]

    def load(path: String): js.Dynamic =
      val mod = loadedModules.getOrElse(path, Loaded(null, 0))
      val fullpath = "out/" + path
      val lastModified = server.lastModified(fullpath)
      if (lastModified > mod.timestamp) {
        val contents = server.readFile(fullpath)
        mod.timestamp = lastModified

        js.Dynamic.global.globalThis.load = load
        mod.module = js.eval(
          s"""|(function() {
              |   module = { exports: {} };
              |   ${contents}
              |   return module.exports
              | }).apply(this)""".stripMargin).asInstanceOf[js.Dynamic]
        mod.module
      } else {
        mod.module
      }

    def evaluate[A](includes: List[String], content: String) =
      run[A](includes, s"\n\ndef main() = { ${content} }\n")

    def run[A](includes: List[String], content: String) = {
      server.writeFile("interactive.effekt", includes.map(i => s"import $i").mkString("\n") + s"\n\n${content}")
      val mainFile = server.compileFile("interactive.effekt")

      load(mainFile.replace("out/", "")).main().run().asInstanceOf[A]
    }

    test("Evaluate simple expressions in REPL") {
      val result = evaluate[Int](List(), "1 + 2")
      assert(result == 3)
    }

    test("Evaluate expressions that uses a builtin which relies on an extern literal") {
      evaluate(List(), "println(42)")
    }

    test("Evaluate expressions using stdlib in REPL") {
      val result = evaluate[Int](List("immutable/list"), "Cons(1, Cons(2, Nil())).size")
      assert(result == 2)
    }

    test("Show core on website") {
      server.writeFile("test.effekt",
        """ def main() = println(1 + 2)
          |""".stripMargin)
      val result = server.showCore("test.effekt")

      assert(result.contains("infixAdd"))
    }

    test("Load file with multiline extern strings") {
      val result = evaluate[Int](List("immutable/list", "mutable/heap"), "Cons(1, Cons(2, Nil())).size")
      assert(result == 2)
    }

    test("Exporting functions from a namespace") {
      server.writeFile("foo.effekt",
        """ namespace foo { def bar() = 42 }
          |""".stripMargin)

      val result = evaluate[Int](List("foo"), "foo::bar()")
      assert(result == 42)
    }

    test("Extern resources on website") {
      val result = run[String](Nil,
        """interface Greet { }
          |
          |extern resource my_resource : Greet
          |
          |def main() = "hello"
          |""".stripMargin)

      assert(result == "hello")
    }
  }
}
