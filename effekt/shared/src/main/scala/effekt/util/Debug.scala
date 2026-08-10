package effekt
package util

import effekt.symbols.TypePrinter
import effekt.util.Version.DEBUG

inline def debug(inline f: => Unit): Unit =
  inline if DEBUG then f else ()

inline def debug[T](inline f: => T)(inline otherwise: => T): T =
  inline if DEBUG then f else otherwise

inline def assert(inline assertion: Boolean): Unit =
  inline if DEBUG then Predef.assert(assertion)

inline def assert(inline assertion: Boolean, inline msg: => String): Unit =
  inline if DEBUG then Predef.assert(assertion, msg)

val showGeneric: PartialFunction[Any, String] = {
  case l: List[_] =>
    l.map(show).mkString("List(", ", ", ")")
  case o: Option[_] =>
    o.map(show).mkString("Option(", ", ", ")")
  case other => other.toString
}

val show: PartialFunction[Any, String] =
  TypePrinter.show orElse
    core.HumanReadablePrettyPrinter.show orElse
    generator.js.PrettyPrinter.show orElse
    cps.PrettyPrinter.show orElse
    machine.PrettyPrinter.show orElse
    showGeneric

/**
 * Outputs a trace of the given values.
 *
 * Example Usage:
 *    util.trace(source.name, tree.path)
 *
 * Output:
 *   effekt/effekt/shared/src/main/scala/effekt/core/Optimizer.scala:30:20
 *     source.name: ds.effekt
 *     tree.path: ds
 */
inline def trace(inline values: Any*): Unit =
  ${ debugMacros.traceCode('values) }

object debugMacros {
  import scala.quoted.*
  import scala.io.AnsiColor.*

  def traceCode(values: Expr[Seq[Any]])(using Q: Quotes): Expr[Unit] =
    import Q.reflect.*

    val term = values.asTerm
    val position: Position = term.pos
    val line   = position.startLine + 1 // to one-based
    val column = position.startColumn + 1
    val file    = position.sourceFile.path
    val posString = Expr.apply(s"${WHITE_B}${UNDERLINED}${file}:${line}:${column}${RESET}")


    values match {
      case Varargs(exprs) =>
        val traces = exprs.map { value =>
          val term = value.asTerm
          val renderedTerm = Expr.apply(s"${BOLD}${term.show}:${RESET}")
          '{
            val result = ${value}
            val renderedValue = show(result)
            "  " + ${renderedTerm} + " " + renderedValue
          }
        }
        '{
          println(${posString})
          ${Expr.ofSeq(traces)}.foreach(println)
        }

      case _ => report.errorAndAbort("Expected explicit varargs sequence. ", values)
    }
}
