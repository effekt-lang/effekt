package effekt.core

import java.io.File

class SpecializeTests extends CoreTests {

  private def show(shape: Shape): String =
    if shape.captures.isEmpty then shape.site.name.name
    else shape.site.name.name + shape.captures.map(_.map(show).getOrElse("?")).mkString("(", ", ", ")")

  private def show(m: ModuleDecl): String = {
    val named = names(m)
    instances(named, sites(named)).toList.map { i =>
      s"${show(i.callee)} ↦ ${i.arguments.map(_.map(show).getOrElse("?")).mkString("[", ", ", "]")}"
    }.sorted.mkString("\n")
  }

  private def specialized(input: ModuleDecl): ModuleDecl = {
    val result = specialization(input)
    result.typecheck()
    result
  }

  registerCoreIRTests(
    new File("examples/core/specialize"),
    CoreIRAnalysis("INSTANCES", show),
    CoreIRTransform("SPECIALIZE", specialized)
  )
}
