package effekt

import scala.language.implicitConversions

import effekt.lexer.TokenKind
import effekt.lexer.TokenKind.*

enum Precedence {
  case LeftBindsTighter
  case RightBindsTighter
  case Ambiguous
}

case class PrecedenceEntry(looser: Set[TokenKind], tighter: Set[TokenKind])

class PrecedenceBuilder {
  private var relations: List[PrecedenceEntry] = List.empty

  def add(looser: Set[TokenKind], tighter: Set[TokenKind]): PrecedenceBuilder = {
    relations = PrecedenceEntry(looser, tighter) :: relations
    this
  }

  def build(): List[PrecedenceEntry] = relations.reverse
}

object PrecedenceBuilder {
  def apply(init: PrecedenceBuilder => Unit): List[PrecedenceEntry] = {
    val builder = new PrecedenceBuilder
    init(builder)
    builder.build()
  }
}

extension (looser: Set[TokenKind]) {
  def ?<(tighter: Set[TokenKind])(using builder: PrecedenceBuilder): Unit =
    builder.add(looser, tighter)
}

def precedenceComparison(relations: List[PrecedenceEntry]): (TokenKind, TokenKind) => Precedence = {
  val precedenceGraph = transitiveClosureGraph(relations)

  (left: TokenKind, right: TokenKind) =>
    if (left == right) Precedence.LeftBindsTighter // left associativity is the default for infix operators
    else if (precedenceGraph.contains((left, right))) Precedence.RightBindsTighter
    else if (precedenceGraph.contains((right, left))) Precedence.LeftBindsTighter
    else Precedence.Ambiguous
}

private def transitiveClosureGraph(relations: List[PrecedenceEntry]): Set[(TokenKind, TokenKind)] = {
  var graph = relations.flatMap { r =>
    for {
      l <- r.looser
      t <- r.tighter
    } yield (l, t)
  }.toSet

  var size = 0
  while (size != graph.size) {
    size = graph.size
    for {
      (a, b) <- graph
      (c, d) <- graph
      if b == c
    } graph += ((a, d))
  }
  graph
}
