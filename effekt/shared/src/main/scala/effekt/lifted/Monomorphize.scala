package effekt
package lifted

import effekt.context.Context

import mono.*

// TODO Known limitations:
// - we do not deal with bidirectional effects, atm
// - extern functions cannot take block arguments (see liftinference.effekt -- `fancy`)
// - we don't want to support region polymorphic recursion (but what's with recursive functions that use local mutable state?)

// TODO check whether evidence is actually ever used and only specialized wrt. the used evidence.
//   implementation sketch: track equiv classes of evidence projections that are used as arguments to shift.

// TODO mutable state:
//   


object Monomorphize {

  val phaseName = "lift-inference"

  def run(mod: ModuleDecl)(using C: Context): ModuleDecl = try {
    given analysis: FlowAnalysis()
    Evidences.last = -1;
    analyze(mod)

    val constrs = analysis.constraints.map {
      c => c.show
    }.mkString("\n")

    val (solved, cls) = solve(analysis.constraints)

    val allBounds = analysis.variables.toList.map {
      x => x -> solved.getOrElse(x, Bounds(Set.empty, Set(Ev.zero(x.arity))))
    }

    val cleaned = cleanup(substitution(allBounds))

    val subst = cleaned.toList.sortBy(_._1.id).map {
      case (x, Bounds(lower, upper)) =>  lower.map(_.show).mkString(", ") + " <: " + x.show + " <: " + upper.map(_.show).mkString(", ")
    }.mkString("\n")

    given t: TransformationContext(analysis, cleaned, functions = cls)

    val binders = analysis.binders.collect { case (id: Id, ftpe: FlowType.Function) => s"${id.name}: ${ftpe.evidences.show}" }.toList.sorted.mkString("\n")

    C.debug(s"""|Solved:
        |-------
        |${subst}
        |
        |Constraints:
        |-----------
        |${constrs}
        |
        |Binders:
        |${binders}
        |""".stripMargin)

    val elaborated = elaborate(mod)

    //    val newDeclarations = mod.decls.map(elaborate).map {
    //      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    //    }.mkString("\n")
    //
    //
    //    val newDefinitions = mod.definitions.map(elaborate).map {
    //      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    //    }.mkString("\n")



    // println(PrettyPrinter.format(elaborated))

    elaborated
  } finally { }
}
