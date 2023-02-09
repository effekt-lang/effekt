package effekt
package lifted

import effekt.context.Context

import mono.*

// TODO in test14.effekt the following looks suspicious:
//    α112(α113()) <: α1()
//
// a113 is resume! This will be fixed by adding an explicit shift.
//
// we do not deal with bidirectional effects, atm


object Monomorphize {

  val phaseName = "lift-inference"

  def run(mod: ModuleDecl)(using C: Context): ModuleDecl = {
    given analysis: FlowAnalysis()
    Evidences.last = -1;
    analyze(mod)

    val constrs = analysis.constraints.map {
      c => c.show
    }.mkString("\n")

    val solved = solve(analysis.constraints)
    val cleaned = cleanup(substitution(solved.toList))

    val subst = cleaned.toList.sortBy(_._1.id).map {
      case (x, Bounds(lower, upper)) =>  lower.map(_.show).mkString(", ") + " <: " + x.show + " <: " + upper.map(_.show).mkString(", ")
    }.mkString("\n")

    given TransformationContext(analysis, cleaned, Map.empty)

    val newDeclarations = mod.decls.map(elaborate).map {
      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    }.mkString("\n")

    val newDefinitions = mod.definitions.map(elaborate).map {
      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    }.mkString("\n")

    C.debug(s"""|Solved:
        |-------
        |${subst}
        |
        |Elaborated Declarations:
        |------------------------
        |${newDeclarations}
        |
        |Elaborated Definitions:
        |------------------------
        |${ newDefinitions }
        |
        |Constraints:
        |-----------
        |${constrs}""".stripMargin)

    mod
  }
}
