package effekt

import effekt.util.messages.Diagnostic

import java.io.File
import sbt.io.*
import sbt.io.syntax.*

/** Every diagnostic records the innermost named phase that was running when it was made. */
class DiagnosticPhaseTests extends munit.FunSuite {

  val dir: File = new File(".") / "out" / "tests" / "diagnostic_phases"

  def diagnostics(main: String, others: (String, String)*): List[Diagnostic] = {
    IO.createDirectory(dir)
    others.foreach { case (name, content) => IO.write(dir / s"$name.effekt", content) }
    val file = dir / "main.effekt"
    IO.write(file, main)
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--compile",
      "--no-exit-on-error",
      "--backend", "js",
      "--out", (dir / "out").getPath
    ))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    compiler.messaging.get.toList
  }

  def reported(ds: List[Diagnostic]): Set[(Option[String], Option[String])] =
    ds.map { d => (d.primary.map { l => new File(l.span.source.name).getName }, d.phase) }.toSet

  def at(file: String, phase: String): (Option[String], Option[String]) = (Some(file), Some(phase))

  test("a parse error is reported by the parser") {
    assertEquals(reported(diagnostics("def main() = ")), Set(at("main.effekt", "parser")))
  }

  test("an unknown name is reported by the namer") {
    assertEquals(reported(diagnostics("def main() = unknownName")), Set(at("main.effekt", "namer")))
  }

  test("a type mismatch is reported by the typer") {
    assertEquals(reported(diagnostics("def main(): Unit = { val x: Int = true; () }")), Set(at("main.effekt", "typer")))
  }

  test("a missing extern body is reported by code generation") {
    val ds = diagnostics("extern def foo() at io: Unit = unknownFeatureFlag \"\"\n\ndef main() = foo()")
    assertEquals(reported(ds), Set(at("main.effekt", "js")))
  }

  test("an error in an imported module is reported by that module's phase, the failed import by the namer") {
    val ds = diagnostics(
      "import out/tests/diagnostic_phases/dep\n\ndef main() = ()",
      "dep" -> "def broken(): Int = true")
    assertEquals(reported(ds), Set(at("dep.effekt", "typer"), at("main.effekt", "namer")))
  }
}
