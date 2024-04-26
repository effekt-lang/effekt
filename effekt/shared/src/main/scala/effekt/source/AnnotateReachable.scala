package effekt
package source

import effekt.context.{ Annotations, Context }
import scala.collection.mutable
import scala.util.boundary

object AnnotateReachable extends Phase[NameResolved, NameResolved], Tree.Query[AnnotateReachable.Ctx, Set[symbols.Symbol]] {

  override val phaseName: String = "annotate-reachable"

  case class Ctx()
  given Ctx()

  override def empty: Set[symbols.Symbol] = Set.empty
  override def combine(r1: Set[symbols.Symbol], r2: Set[symbols.Symbol]): Set[symbols.Symbol] = r1 union r2

  override def visit[T <: Tree](t: T)(visitor: T => Set[symbols.Symbol])(using Context, Ctx): Set[symbols.Symbol] = t match {
    case d@Def.NamespaceDef(x, defs) => visitor(d); Set.empty // ignore dependencies via namespace symbol (otherwise would be all)
    case d@Def.ExternInclude(_,_,_,_) => visitor(d); Set.empty
    case d: Definition =>
      val sym = Context.symbolOf(d)
      val deps = visitor(t)
      val oldDeps = Context.annotationOption(Annotations.DependsOn, sym).getOrElse(Set.empty)
      Context.annotate(Annotations.DependsOn, sym, oldDeps union deps)
      Set(sym)
    case r: Reference =>
      import effekt.lifted.EvidenceSymbol
      import effekt.symbols.{TermSymbol, TypeSymbol}
      val s = destruct(Context.symbolOf(r))
      visitor(r) union s
    case _ => visitor(t)
  }

  def destruct(symbol: symbols.Symbol): Set[symbols.Symbol] = symbol match {
    case symbols.CallTarget(syms) => syms.toSet.flatten
    case _ => Set(symbol)
  }

  override def run(input: NameResolved)(using C: Context): Option[NameResolved] = input match {
    case r@NameResolved(source, tree, mod) =>
      val _ = query(tree)
      Some(r)
  }

  def isReachable(to: symbols.Symbol, from: symbols.Symbol)(using Context): Option[List[symbols.Symbol]] = boundary {
    val seen: mutable.HashSet[symbols.Symbol] = new mutable.HashSet[symbols.Symbol]()
    val frontier: mutable.Queue[(symbols.Symbol, List[symbols.Symbol])] = mutable.Queue.empty

    def visit(sym: symbols.Symbol, path: List[symbols.Symbol]): Unit = {
      val p = sym :: path
      if(to == sym) {
        boundary.break(Some(p))
      }
      seen.add(sym)
      Context.annotationOption(Annotations.DependsOn, sym).foreach{ deps =>
        deps.foreach{ dep =>
          if(!seen.contains(dep)) {
            frontier.enqueue((dep, p))
          }
        }
      }
    }

    visit(from, Nil)
    while (frontier.nonEmpty) {
      val (cur, p) = frontier.dequeue()
      visit(cur, p)
    }
    None
  }

}
