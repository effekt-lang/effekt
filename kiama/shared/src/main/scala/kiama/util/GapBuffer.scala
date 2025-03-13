package kiama
package util

/**
 * An immutable gap buffer implementation optimized for line-based text operations,
 * with support for LSP-style position-based edits.
 *
 * A gap buffer represents text as two sequences separated by a gap, allowing efficient
 * insertions and deletions near a cursor position. This implementation:
 * - Stores each line as a Rope for efficient string operations
 * - Maintains cursor position between left and right sequences
 * - Supports LSP position-based edits with line/character coordinates
 * - Returns new buffer instances on modifications rather than mutating state
 *
 * Performance characteristics:
 * - O(1) operations at cursor position
 * - O(n) cursor movement where n is distance moved
 * - O(1) line access relative to cursor
 */
case class GapBuffer(
  /** Lines before the current cursor position */
  leftLines: Vector[Rope] = Vector.empty,
  /** Lines after the current cursor position */
  rightLines: Vector[Rope] = Vector.empty,
  /** Current line number where the gap is positioned */
  cursorLine: Int = 0,
  /** Current character position within the cursor line */
  cursorChar: Int = 0
) {

  /** Returns line content at given index, or None if index out of bounds */
  def line(index: Int): Option[String] = {
    if (index < 0 || index >= lineCount) None
    else if (index < leftLines.size) Some(leftLines(index).toString)
    else Some(rightLines(index - leftLines.size).toString)
  }

  /** Total number of lines in the buffer */
  def lineCount: Int = leftLines.size + rightLines.size

  /** Length of the line at the given index, or 0 if index is invalid */
  def lineLength(index: Int): Int = line(index).map(_.length).getOrElse(0)

  /** Full content of buffer as a single string */
  def content: String = {
    val sb = new StringBuilder
    leftLines.foreach(r => sb.append(r).append('\n'))
    rightLines.foreach(r => sb.append(r).append('\n'))
    if (sb.nonEmpty) sb.init.toString else ""
  }

  /** Returns new buffer with cursor moved to specified line and character position */
  def moveTo(line: Int, char: Int = 0): GapBuffer = {
    require(line >= 0 && line <= lineCount)
    require(char >= 0 && char <= lineLength(line))

    val withLineMove = if (line < cursorLine) {
      var left = leftLines
      var right = rightLines
      var pos = cursorLine
      while (pos > line && left.nonEmpty) {
        right = right.prepended(left.last)
        left = left.init
        pos -= 1
      }
      copy(leftLines = left, rightLines = right, cursorLine = pos)
    } else if (line > cursorLine) {
      var left = leftLines
      var right = rightLines
      var pos = cursorLine
      while (pos < line && right.nonEmpty) {
        left = left.appended(right.head)
        right = right.tail
        pos += 1
      }
      copy(leftLines = left, rightLines = right, cursorLine = pos)
    } else this

    withLineMove.copy(cursorChar = char)
  }

  /**
   * Returns new buffer with text replaced between start and end positions.
   * Positions are LSP-style {line, character} coordinates.
   */
  def replaceRange(
    startLine: Int,
    startChar: Int,
    endLine: Int,
    endChar: Int,
    text: String
  ): GapBuffer = {
    require(startLine >= 0 && startLine <= lineCount)
    require(endLine >= 0 && endLine <= lineCount)
    require(startLine <= endLine)
    require(startChar >= 0 && startChar <= lineLength(startLine))
    require(endChar >= 0 && endChar <= lineLength(endLine))

    // Move to start position
    val atStart = moveTo(startLine, startChar)

    // Handle replacements
    val lines = text.split("\n", -1) // -1 to keep empty trailing strings
    if (lines.isEmpty) return atStart

    // Prepare replacement lines:
    // - First line: combine with start of original line
    // - Middle lines: use as is
    // - Last line: combine with end of original line
    val firstLine = atStart.line(startLine).getOrElse("")
    val lastLine = atStart.line(endLine).getOrElse("")

    val newLines = {
      if (text.isEmpty) {
        // Empty replacement, just join start and end
        Vector(Rope(firstLine.substring(0, startChar) + lastLine.substring(endChar)))
      } else if (lines.length == 1) {
        // Single line replacement - no newlines in input
        Vector(Rope(firstLine.substring(0, startChar) + lines(0) + lastLine.substring(endChar)))
      } else {
        // Multi-line replacement
        val firstNewLine = Rope(firstLine.substring(0, startChar) + lines.head)
        val middleLines = lines.slice(1, lines.length - 1).map(Rope(_)).toVector
        val lastNewLine = Rope(lines.last + lastLine.substring(endChar))
        firstNewLine +: middleLines :+ lastNewLine
      }
    }

    // Replace the affected range of lines with new lines
    val (before, after) = if (startLine < leftLines.size) {
      val (b, rest) = leftLines.splitAt(startLine)
      val a = if (endLine >= leftLines.size)
        rightLines.drop(endLine - leftLines.size + 1)
      else
        rest.drop(endLine - startLine + 1) ++ rightLines
      (b, a)
    } else {
      val rstartLine = startLine - leftLines.size
      val rendLine = endLine - leftLines.size
      (leftLines, rightLines.slice(0, rstartLine) ++ rightLines.drop(rendLine + 1))
    }

    atStart.copy(
      leftLines = before ++ newLines,
      rightLines = after,
      cursorChar = 0
    )
  }
}

object GapBuffer {
  /** Creates a new GapBuffer from initial content */
  def apply(content: String): GapBuffer = {
    if (content.isEmpty) GapBuffer()
    else {
      val lines = content.split("\n").map(Rope(_)).toVector
      GapBuffer(leftLines = lines)
    }
  }
  val empty = GapBuffer("")
}
