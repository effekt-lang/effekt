package effekt
package cps

/** The call-target projection of guarded equality analysis. */
object Targets {
  type LocalDefinition = GuardedEquality.LocalDefinition
  type CallTargets = GuardedEquality.CallTargets
  type TargetResult = GuardedEquality.TargetResult

  def targets(definition: ToplevelDefinition): TargetResult =
    GuardedEquality.targets(definition)

  def targets(definition: ToplevelDefinition, roots: Set[core.Id]): TargetResult =
    GuardedEquality.targets(definition, roots)
}
