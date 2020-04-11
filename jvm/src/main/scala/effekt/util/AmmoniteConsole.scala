package effekt
package util

import org.bitbucket.inkytonik.kiama.util.Console
import ammonite.terminal.{ Filter, TermState, Terminal }
import ammonite.terminal.filters._
import ammonite.terminal.filters.GUILikeFilters.SelectionFilter
import java.io.{ InputStreamReader, OutputStreamWriter }

import ammonite.terminal.LazyList.~:

/**
 * A console that provides line editing using Ammonite.
 *
 * The implementation is based on the test found in the Ammonite repository:
 *    https://github.com/lihaoyi/Ammonite/blob/master/terminal/src/test/scala/ammonite/terminal/TestMain.scala
 */
object AmmoniteConsole extends Console {

  val reader = new InputStreamReader(System.in)

  val cutPaste = ReadlineFilters.CutPasteFilter()

  private var history = List.empty[String]

  val selection = GUILikeFilters.SelectionFilter(indent = 4)

  def multilineFilter: Filter = Filter.partial {
    case TermState(13 ~: rest, b, c, _) if b.count(_ == '(') != b.count(_ == ')') =>
      BasicFilters.injectNewLine(b, c, rest)
    case TermState(13 ~: rest, b, c, _) if b.count(_ == '{') != b.count(_ == '}') =>
      BasicFilters.injectNewLine(b, c, rest)
  }

  def readLineOption(prompt: String): Option[String] = {

    val historyFilter = new HistoryFilter(() => history.toVector, fansi.Color.Blue)

    Terminal.readLine(
      Console.MAGENTA + s"${prompt} " + Console.RESET,
      reader,
      new OutputStreamWriter(System.out),
      Filter.merge(
        UndoFilter(),
        cutPaste,
        historyFilter,
        multilineFilter,
        selection,
        BasicFilters.tabFilter(4),
        GUILikeFilters.altFilter,
        GUILikeFilters.fnFilter,
        ReadlineFilters.navFilter,
        BasicFilters.all
      ),
      displayTransform = (buffer, cursor) => {
        // and highlight the selection
        val ansiBuffer = fansi.Str(highlightCommand(buffer))
        val (newBuffer, cursorOffset) = SelectionFilter.mangleBuffer(
          selection, ansiBuffer, cursor, fansi.Reversed.On
        )
        val newNewBuffer = HistoryFilter.mangleBuffer(
          historyFilter, newBuffer, cursor,
          fansi.Color.Green
        )

        (newNewBuffer, cursorOffset)
      }
    ) map {
        case line =>
          history = line :: history;
          line
      }
  }

  override def readLine(prompt: String): String =
    readLineOption(prompt).getOrElse { sys error "no input" }

  // highlight commands starting with :
  private def highlightCommand(b: Vector[Char]): Vector[Char] =
    if (b.size > 0 && b.apply(0) == ':') {
      val parts = b.mkString("").split(" ")
      val highlighted = Console.MAGENTA + parts.head + Console.RESET
      (highlighted +: parts.tail).mkString(" ").toVector
    } else {
      b
    }

  /**
   * Print representation for usage messages.
   */
  override def toString: String =
    "AmmoniteConsole"

}
