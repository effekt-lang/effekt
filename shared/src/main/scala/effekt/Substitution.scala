package effekt

import effekt.symbols.{ Type, ValueType, RigidVar, TypeVar, BlockType, Effectful, TypeApp, Sections }
import effekt.symbols.builtins.THole

import effekt.util.messages.ErrorReporter

object subtitutions {

  type Substitutions = Map[TypeVar, ValueType]

  implicit class SubstitutionOps(subst: Substitutions) {

    def add(k: TypeVar, v: ValueType)(implicit report: ErrorReporter): Substitutions = {
      subst.get(k).foreach { v2 =>
        if (v != v2) {
          report.error(s"${k} cannot be instantiated with ${v} and with ${v2} at the same time.")
        }
      }

      // Use new substitution binding to refine right-hand-sides of existing substitutions.
      // Do we need an occurs check?
      val newSubst = Map(k -> v)
      val improvedSubst: Substitutions = subst.map { case (rigid, tpe) => (rigid, newSubst substitute tpe) }
      improvedSubst + (k -> improvedSubst.substitute(v))
    }

    def union(other: Substitutions)(implicit report: ErrorReporter): Substitutions =
      other.foldLeft(subst) { case (subst, (k, v)) => subst.add(k, v) }

    def substitute(t: Type): Type = t match {
      case t: ValueType => substitute(t)
      case b: BlockType => substitute(b)
    }

    def substitute(t: ValueType): ValueType = t match {
      case x: TypeVar =>
        subst.getOrElse(x, x)
      case TypeApp(t, args) =>
        TypeApp(t, args.map { substitute })
      case other => other
    }

    def substitute(e: Effectful): Effectful = e match {
      case Effectful(tpe, effs) => Effectful(substitute(tpe), effs)
    }

    def substitute(t: BlockType): BlockType = t match {
      case BlockType(tps, ps, ret) =>
        val substWithout = subst.filterNot { case (t, _) => ps.contains(t) }
        BlockType(tps, substWithout.substitute(ps), substWithout.substitute(ret))
    }

    def substitute(t: Sections): Sections = t map {
      _ map {
        case v: ValueType => substitute(v)
        case b: BlockType => substitute(b)
      }
    }

    def checkFullyDefined(rigids: List[RigidVar])(implicit report: ErrorReporter): Unit =
      rigids.foreach { tpe =>
        if (!subst.isDefinedAt(tpe))
          report.error(s"Couldn't infer type for ${tpe.underlying}")
      }
  }

  object Substitution {

    def empty: Substitutions = Map.empty

    // The lhs can contain rigid vars that we can compute a mapping for
    // i.e. unify(List[?A], List[Int]) = Map(?A -> Int)
    def unify(tpe1: Type, tpe2: Type)(implicit report: ErrorReporter): Substitutions =
      (tpe1, tpe2) match {

        case (t: ValueType, s: ValueType) =>
          unifyValueTypes(t, s)

        // TODO also consider type parameters here
        case (f1 @ BlockType(_, args1, ret1), f2 @ BlockType(_, args2, ret2)) =>

          if (args1.size != args2.size)
            report.error(s"Section count does not match $f1 vs. $f2")

          (args1 zip args2).foldLeft(unify(ret1.tpe, ret2.tpe)) {
            case (u, (as1, as2)) =>
              if (as1.size != as2.size)
                report.error(s"Argument count does not match $f1 vs. $f2")
              (as1 zip as2).foldLeft(u) { case (u, (a1, a2)) => u union unify(a1, a2) }
          }

        case (t, s) =>
          report.error(s"Expected ${t}, but got ${s}")
          Substitution.empty
      }

    def unifyValueTypes(tpe1: ValueType, tpe2: ValueType)(implicit report: ErrorReporter): Substitutions =
      (tpe1.dealias, tpe2.dealias) match {

        case (t, s) if t == s =>
          Substitution.empty

        case (s: RigidVar, t: ValueType) =>
          Map(s -> t)

        // occurs for example when checking the first argument of `(1 + 2) == 3` against expected
        // type `?R` (since `==: [R] (R, R) => Boolean`)
        case (s: ValueType, t: RigidVar) =>
          Map(t -> s)

        case (TypeApp(t1, args1), TypeApp(t2, args2)) if t1 == t2 =>
          if (args1.size != args2.size)
            report.error(s"Argument count does not match $t1 vs. $t2")

          (args1 zip args2).foldLeft(Substitution.empty) {
            case (u, (a1, a2)) => u union unifyValueTypes(a1, a2)
          }

        case (THole, _) | (_, THole) =>
          Substitution.empty

        case (t, s) =>
          report.error(s"Expected ${tpe1}, but got ${tpe2}")
          Substitution.empty
      }

    /**
     * Instantiate a typescheme with fresh, rigid type variables
     *
     * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
     */
    def instantiate(tpe: BlockType): (List[RigidVar], BlockType) = {
      val BlockType(tparams, params, ret) = tpe
      val subst = tparams.map { p => p -> RigidVar(p) }.toMap
      val rigids = subst.values.toList

      (rigids, BlockType(Nil, subst.substitute(params), subst.substitute(ret)))
    }
  }
}
