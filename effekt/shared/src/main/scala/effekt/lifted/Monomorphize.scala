package effekt
package lifted

import effekt.PhaseResult.CoreLifted
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


object Monomorphize extends Phase[CoreLifted, CoreLifted] {

  val phaseName = "lift-inference"

  def run(lifted: CoreLifted)(using Context): Option[CoreLifted] =
    run(lifted.core).map { mono => lifted.copy(core = mono) }

  def run(mod: ModuleDecl)(using C: Context): Option[ModuleDecl] = {
    given analysis: FlowAnalysis()
    Evidences.last = -1;
    analyze(mod)

    val constrs = analysis.constraints.map {
      c => c.show
    }.mkString("\n")

    val binders = analysis.binders.collect { case (id: Id, ftpe: FlowType.Function) => s"${id.name}: ${ftpe.evidences.show}" }.toList.sorted.mkString("\n")

        println(s"""|
            |Constraints:
            |-----------
            |${constrs})
            |""".stripMargin)

        println(s"""|
            |
            |Binders:
            |${binders}
            |""".stripMargin)

    val (solved, cls) = solve(analysis.constraints)

    val allBounds = analysis.variables.toList.map {
      x => x -> solved.getOrElse(x, Bounds(Set.empty, Set(Ev.zero(x.arity))))
    }

    val subst = substitution(allBounds)
    checkPolymorphicRecursion(subst)
    val cleaned = cleanup(subst)

    val prettySubst = substitution(allBounds).toList.sortBy(_._1.id).map {
      case (x, Bounds(lower, upper)) =>  lower.map(_.show).mkString(", ") + " <: " + x.show + " <: " + upper.map(_.show).mkString(", ")
    }.mkString("\n")

    //    println(s"""|
    //        |
    //        |Substitution:
    //        |${prettySubst}
    //        |""".stripMargin)

    val elaborated = elaborate(mod)(using TransformationContext.from(analysis, cleaned, cls))

    //    val newDeclarations = mod.decls.map(elaborate).map {
    //      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    //    }.mkString("\n")
    //
    //
    //    val newDefinitions = mod.definitions.map(elaborate).map {
    //      d => PrettyPrinter.pretty(PrettyPrinter.toDoc(d), 20).layout
    //    }.mkString("\n")



    // println(PrettyPrinter.format(elaborated))

    Some(elaborated)
  }
}
