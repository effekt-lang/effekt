package effekt
package core

import java.io.File

import effekt.core.optimizer.{ Target, cleanup, contify, identify, returnsTo }

class ContificationTests extends CoreTests {

  private def checked(transform: ModuleDecl => ModuleDecl)(input: ModuleDecl): ModuleDecl = {
    val result = transform(input)
    result.typecheck()
    result
  }

  private var analysis: Map[Id, Set[Target]] = null

  private def show(analysis: Map[Id, Set[Target]]): String = analysis.keys.toList.map { f =>
    val shown = analysis(f).toList.map {
      case Target.Known(k) => k.name.name
      case Target.Unknown => "?"
    }.sorted
    s"${f.name.name} ↦ ${shown.mkString(", ")}"
  }.sorted.mkString("\n")

  registerCoreIRTests(
    new File("examples/core/contification"),
    CoreIRTransform("IDENTIFY", checked(identify)),
    CoreIRAnalysis("RETURNS_TO", input => { analysis = returnsTo(input); show(analysis) }),
    CoreIRTransform("CONTIFY", checked(contify(_, analysis))),
    CoreIRTransform("CLEANUP", checked(cleanup))
  )
}
