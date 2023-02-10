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

// TODO check whether evidence is actually ever used and only specialized wrt. the used evidence.

object Monomorphize {

  val phaseName = "lift-inference"

  def run(mod: ModuleDecl)(using C: Context): ModuleDecl = {
    given analysis: FlowAnalysis()
    Evidences.last = -1;
    analyze(mod)

    val constrs = analysis.constraints.map {
      c => c.show
    }.mkString("\n")

    val (solved, cls) = solve(analysis.constraints)
    val cleaned = cleanup(substitution(solved.toList))

    val subst = cleaned.toList.sortBy(_._1.id).map {
      case (x, Bounds(lower, upper)) =>  lower.map(_.show).mkString(", ") + " <: " + x.show + " <: " + upper.map(_.show).mkString(", ")
    }.mkString("\n")

    given t: TransformationContext(analysis, cleaned, functions = cls)


    val transposed = cls.groupMapReduce({ case (ftpe, n) => n })({ case (ftpe, n) => Set(ftpe)})(_ ++ _)

    transposed foreach {
      case (n, ftpes) => println(System.identityHashCode(n).toString + " -> " + ftpes.map(f => f.evidences.show).mkString(", "))
    }

    val elaborated = elaborate(mod)

    //    val newDeclarations = mod.decls.map(elaborate).map {
    //      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    //    }.mkString("\n")
    //
    //
    //    val newDefinitions = mod.definitions.map(elaborate).map {
    //      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    //    }.mkString("\n")

    C.debug(s"""|Solved:
        |-------
        |${subst}
        |
        |Constraints:
        |-----------
        |${constrs}""".stripMargin)

    println(PrettyPrinter.format(elaborated))

    elaborated
  }
}
