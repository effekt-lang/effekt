package effekt

import org.bitbucket.inkytonik.kiama.util.{ Positions, StringSource }
import org.bitbucket.inkytonik.kiama.parsing.{ Success }

import org.junit.Test
import org.junit.Assert._

class ParserTests extends Parser(new Positions) {

  @Test def parseAddition() =
    shouldParse(expr, "1 + 2")

  def shouldParse[T](p: PackratParser[T], str: String) =
    parseAll(p, StringSource(str)) match {
      case Success(result, rest) => assert(true)
      case f                     => fail(f.toMessage)
    }
}
