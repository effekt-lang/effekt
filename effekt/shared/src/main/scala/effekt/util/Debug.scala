package effekt
package util

import effekt.symbols.TypePrinter


val showGeneric: PartialFunction[Any, String] = {
  case other => other.toString
}

val show: PartialFunction[Any, String] =
  TypePrinter.show orElse core.PrettyPrinter.show orElse showGeneric

inline def debug[A](inline value: A): A =
  ${ debugMacros.debugCode('value) }

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

  def debugCode[A: Type](value: Expr[A])(using Q: Quotes): Expr[A] =
    import Q.reflect.*

    val term = value.asTerm
    val position: Position = term.pos
    val line   = position.startLine + 1 // to one-based
    val column = position.startColumn + 1
    val file    = position.sourceFile.path
    val posString = Expr.apply(s"${WHITE_B}${UNDERLINED}${file}:${line}:${column}${RESET}")

    val renderedTerm = Expr.apply(s"${BOLD}${term.show}:${RESET}")

    '{
      println(${posString})
      val result = ${value}
      val renderedValue = show(result)
      if (renderedValue.length > 100) {
        println(${renderedTerm})
        println(renderedValue)
      } else {
        println(${renderedTerm} + " " + renderedValue)
      }
      result
    }

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
