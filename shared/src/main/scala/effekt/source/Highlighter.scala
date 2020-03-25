//package effekt.source
//
//import effekt.Parser
//import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter
//import org.bitbucket.inkytonik.kiama.parsing.{ Input, NoSuccess, Success }
//import org.bitbucket.inkytonik.kiama.util.{ Position, Positions, Source, StringSource }
//
//import scala.language.implicitConversions
//
//
//// Colors adopted from Dotty Compiler
//object colors {
//  val Clear      = Console.RESET
//  val Comment    = Console.BLUE
//  val Keyword    = Console.YELLOW
//  val Definition = Console.CYAN
//  val Literal    = Console.RED
//  val String     = Console.GREEN
//}
//
//object Highlighter {
//
//
//
//  // We can reuse the positions mapping, traverse the tree and add highlighting
//  // to the original input stream.
//  //
//  // However, for comments that are not part of the tree, we need a lexer!
//
//
//}
//
//object Lexer extends Parser(new Positions) {
//
//  import scala.collection.mutable.ArrayBuffer
//
//  case class Annotation(from: Int, to: Int, color: String)
//
//  val annotations: ArrayBuffer[Annotation] = ArrayBuffer.empty
//
//  var input = Input(StringSource("foo"), 0)
//
//  lazy val spaces = rep1(regex("""\s*""".r))
//
////  while (!input.atEnd) {
//    val before = input
//    skipSpaces()
//    println(tryConsume(rep1(singleline))) //foreach { case (from, to, _) => annotate(from, to, colors.Comment) }
//    println(tryConsume(literals))
////    foreach {
////      case (from, to, s: StringLit) => annotate(from, to, colors.String)
////      case (from, to, DoubleLit(_) | IntLit(_)) => annotate(from, to, colors.Literal)
////      case (from, to, d: BooleanLit) => annotate(from, to, colors.Keyword)
////      case _ => ()
////    }
////    tryConsume(anyKeyword) foreach { case (from, to, _) => annotate(from, to, colors.Keyword) }
////    if (before.offset == input.offset)
////      sys error "couldnt read"
//////  }
//
//  def annotate(from: Int, to: Int, color: String): Unit =
//    annotations.addOne(Annotation(from, to, color))
//
//  def tryConsume[R](p: Parser[R]): Option[(Int, Int, R)] = {
//    val before = input
//    p(input) match {
//      case Success(res, next) =>
//        input = next
//        Some((before.offset, next.offset, res))
//      case NoSuccess(msg, next) =>
//        input = next // does that make sense?
//        println(msg)
//        None
//    }
//  }
//
//  def skipSpaces(): Unit =
//    tryConsume(spaces)
//
//}