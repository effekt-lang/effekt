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

  private def show(analysis: Map[Id, Set[Target]]): String = {
    val sameName = analysis.keys.toList.groupBy(_.name.name)
    def shown(id: Id): String = sameName.get(id.name.name) match {
      case Some(ids) if ids.size > 1 => s"${id.name.name}${ids.sortBy(_.id).indexOf(id) + 1}"
      case _ => id.name.name
    }
    analysis.keys.toList.map { f =>
      val targets = analysis(f).toList.map {
        case Target.Known(k) => shown(k.id)
        case Target.Unknown => "?"
      }.sorted
      s"${shown(f)} ↦ ${targets.mkString(", ")}"
    }.sorted.mkString("\n")
  }

  registerCoreIRTests(
    new File("examples/core/contification"),
    CoreIRTransform("IDENTIFY", checked(identify)),
    CoreIRAnalysis("RETURNS_TO", input => { analysis = returnsTo(input); show(analysis) }),
    CoreIRTransform("CONTIFY", checked(contify(_, analysis))),
    CoreIRTransform("CLEANUP", checked(cleanup))
  )
}
