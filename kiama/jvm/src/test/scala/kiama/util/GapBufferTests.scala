package kiama
package util

class GapBufferTests extends munit.FunSuite {

  test("empty buffer") {
    val buffer = GapBuffer("")
    assert(buffer.lineCount == 0)
    assert(buffer.content == "")
  }

  test("single line") {
    val buffer = GapBuffer("test")
    assert(buffer.lineCount == 1)
    assert(buffer.line(0).contains("test"))
    assert(buffer.content == "test")
  }

  test("multi line") {
    val buffer = GapBuffer("line1\nline2\nline3")
    assert(buffer.lineCount == 3)
    assert(buffer.line(0).contains("line1"))
    assert(buffer.line(1).contains("line2"))
    assert(buffer.line(2).contains("line3"))
  }

  test("replaceRange - empty replacement") {
    val buffer = GapBuffer("test")
    val modified = buffer.replaceRange(0, 0, 0, 0, "")
    assert(modified.content == "test")
    assert(modified.lineCount == 1)
  }

  test("replaceRange - single line insert") {
    val buffer = GapBuffer("test")
    val modified = buffer.replaceRange(0, 0, 0, 0, "Hello, ")
    assert(modified.content == "Hello, test")
    assert(modified.lineCount == 1)
  }

  test("replaceRange - multi line insert") {
    val buffer = GapBuffer("test")
    val modified = buffer.replaceRange(0, 0, 0, 0, "Hello!\n\n\n")
    assert(modified.lineCount == 4)
    assert(modified.line(0).contains("Hello!"))
    assert(modified.line(1).contains(""))
    assert(modified.line(2).contains(""))
    assert(modified.line(3).contains("test"))
  }

  test("replaceRange - replace entire line") {
    val buffer = GapBuffer("test")
    val modified = buffer.replaceRange(0, 0, 0, 4, "replaced")
    assert(modified.content == "replaced")
    assert(modified.lineCount == 1)
  }

  test("replaceRange - replace across lines") {
    val buffer = GapBuffer("line1\nline2\nline3")
    val modified = buffer.replaceRange(0, 2, 1, 2, "ne1-li")
    assert(modified.content == "line1-line2\nline3")
    assert(modified.lineCount == 2)
  }

  test("edge cases") {
    val buffer = GapBuffer("test")
    test("invalid line access") {
      assert(buffer.line(-1).isEmpty)
      assert(buffer.line(1).isEmpty)
    }

    test("line length") {
      assert(buffer.lineLength(0) == 4)
      assert(buffer.lineLength(1) == 0)
    }
  }

  test("compound edits") {
    test("incrementally building content") {
      var buffer = GapBuffer("")
      assert(buffer.content == "")

      buffer = buffer.replaceRange(0, 0, 0, 0, "def")
      assert(buffer.content == "def")

      buffer = buffer.replaceRange(0, 3, 0, 3, " main() {\n  ")
      assert(buffer.content == "def main() {\n  ")

      buffer = buffer.replaceRange(1, 2, 1, 2, "println(")
      assert(buffer.content == "def main() {\n  println(")

      buffer = buffer.replaceRange(1, 10, 1, 10, "\"Hello!\"")
      assert(buffer.content == "def main() {\n  println(\"Hello!\"")

      buffer = buffer.replaceRange(1, 18, 1, 18, ")")
      assert(buffer.content == "def main() {\n  println(\"Hello!\")")

      buffer = buffer.replaceRange(1, 19, 1, 19, "\n}")
      assert(buffer.content == "def main() {\n  println(\"Hello!\")\n}")
    }

    test("multiple line replacements") {
      var buffer = GapBuffer("line1\nline2\nline3\nline4\nline5")
      assert(buffer.lineCount == 5)

      // Replace lines 2-4 with new content
      buffer = buffer.replaceRange(1, 0, 3, 5, "newline2\ninserted\nnewline4")
      assert(buffer.lineCount == 5)
      assert(buffer.line(0).contains("line1"))
      assert(buffer.line(1).contains("newline2"))
      assert(buffer.line(2).contains("inserted"))
      assert(buffer.line(3).contains("newline4"))
      assert(buffer.line(4).contains("line5"))

      // Now replace part of first line and add content
      buffer = buffer.replaceRange(0, 2, 1, 3, "REPLACED\nmore")
      assert(buffer.content == "liREPLACED\nmoreline2\ninserted\nnewline4\nline5")

      // Delete some lines
      buffer = buffer.replaceRange(1, 0, 3, 0, "")
      assert(buffer.lineCount == 3)
      assert(buffer.line(0).contains("liREPLACED"))
      assert(buffer.line(1).contains("newline4"))
      assert(buffer.line(2).contains("line5"))
    }

    test("editing with empty lines") {
      var buffer = GapBuffer("first\n\n\nlast")
      assert(buffer.lineCount == 4)

      // Insert into empty line
      buffer = buffer.replaceRange(1, 0, 1, 0, "middle")
      assert(buffer.lineCount == 4)
      assert(buffer.line(1).contains("middle"))

      // Replace empty line
      buffer = buffer.replaceRange(2, 0, 2, 0, "also here")
      assert(buffer.lineCount == 4)
      assert(buffer.line(0).contains("first"))
      assert(buffer.line(1).contains("middle"))
      assert(buffer.line(2).contains("also here"))
      assert(buffer.line(3).contains("last"))

      // Create more empty lines
      buffer = buffer.replaceRange(2, 4, 2, 9, "\n\n\nhere")
      assert(buffer.lineCount == 7)
      assert(buffer.line(2).contains("also"))
      assert(buffer.line(3).contains(""))
      assert(buffer.line(4).contains(""))
      assert(buffer.line(5).contains("here"))
      assert(buffer.line(6).contains("last"))
    }

    test("replacing multiple lines with single line") {
      var buffer = GapBuffer("one\ntwo\nthree\nfour\nfive")

      // Replace 3 lines with single line
      buffer = buffer.replaceRange(1, 0, 3, 4, "REPLACED")
      assert(buffer.lineCount == 3)
      assert(buffer.line(0).contains("one"))
      assert(buffer.line(1).contains("REPLACED"))
      assert(buffer.line(2).contains("five"))

      // Replace 2 lines with single line leaving partial content
      buffer = buffer.replaceRange(0, 2, 1, 3, "NEW")
      assert(buffer.lineCount == 2)
      assert(buffer.line(0).contains("onNEWLACED"))
      assert(buffer.line(1).contains("five"))
    }

    test("replacing lines with more lines") {
      var buffer = GapBuffer("1\n2\n3\n4\n5")

      // Replace 2 lines with 3 lines
      buffer = buffer.replaceRange(1, 0, 2, 1, "two\nextra\nthree")
      assert(buffer.lineCount == 6)
      assert(buffer.line(0).contains("1"))
      assert(buffer.line(1).contains("two"))
      assert(buffer.line(2).contains("extra"))
      assert(buffer.line(3).contains("three"))
      assert(buffer.line(4).contains("4"))
      assert(buffer.line(5).contains("5"))

      // Replace 3 lines with 2 lines
      buffer = buffer.replaceRange(2, 0, 4, 1, "merged1\nmerged2")
      assert(buffer.lineCount == 5)
      assert(buffer.line(0).contains("1"))
      assert(buffer.line(1).contains("two"))
      assert(buffer.line(2).contains("merged1"))
      assert(buffer.line(3).contains("merged2"))
      assert(buffer.line(4).contains("5"))
    }

    test("incremental line removal") {
      var buffer = GapBuffer("a\nb\nc\nd\ne")
      assert(buffer.content == "a\nb\nc\nd\ne")

      // Remove 2 lines
      buffer = buffer.replaceRange(1, 0, 2, 1, "")
      assert(buffer.content == "a\n\nd\ne")

      // Remove another 2 lines but preserve part of last line
      buffer = buffer.replaceRange(0, 1, 3, 0, "")
      assert(buffer.content == "ae")

      // Remove all content
      buffer = buffer.replaceRange(0, 0, 0, 2, "")
      assert(buffer.content == "")
    }

    test("mixed transformations") {
      var buffer = GapBuffer("1\n2\n3\n4\n5")

      // First replace multiple lines with single
      buffer = buffer.replaceRange(1, 0, 3, 1, "merged")
      assert(buffer.lineCount == 3)
      assert(buffer.line(1).contains("merged"))
      assert(buffer.content == "1\nmerged\n5")

      // Then replace single line with multiple
      buffer = buffer.replaceRange(1, 0, 1, 6, "a\nb\nc")
      assert(buffer.content == "1\na\nb\nc\n5")
      assert(buffer.lineCount == 5)
      assert(buffer.line(1).contains("a"))
      assert(buffer.line(2).contains("b"))
      assert(buffer.line(3).contains("c"))

      // Replace all with single line
      buffer = buffer.replaceRange(0, 0, 4, 1, "only line")
      assert(buffer.content == "only line")
      assert(buffer.lineCount == 1)
      assert(buffer.line(0).contains("only line"))
    }

  }
}
