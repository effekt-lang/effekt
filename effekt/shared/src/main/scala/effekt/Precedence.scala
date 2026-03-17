package effekt

import scala.language.implicitConversions

import effekt.lexer.TokenKind
import effekt.lexer.TokenKind.*

object Precedence {

  enum Precedence {
    case LeftBindsTighter
    case RightBindsTighter
    case Ambiguous
  }

  case class PartialOrd(looser: Set[TokenKind], tighter: Set[TokenKind])

  class PrecedenceBuilder {
    private var relations: List[PartialOrd] = List.empty

    def add(looser: Set[TokenKind], tighter: Set[TokenKind]): PrecedenceBuilder = {
      relations = PartialOrd(looser, tighter) :: relations
      this
    }

    def build(): List[PartialOrd] = relations.reverse
  }

  object PrecedenceBuilder {
    def apply(init: PrecedenceBuilder => Unit): List[PartialOrd] = {
      val builder = new PrecedenceBuilder
      init(builder)
      builder.build()
    }
  }

  extension (looser: Set[TokenKind]) {
    def ?<(tighter: Set[TokenKind])(using builder: PrecedenceBuilder): Unit =
      builder.add(looser, tighter)
  }
  
  def precedenceComparison(relations: List[PartialOrd]): (TokenKind, TokenKind) => Precedence = {
    val precedenceGraph = transitiveClosureGraph(relations)

    (left: TokenKind, right: TokenKind) =>
      if (left == right) Precedence.Ambiguous
      else if (precedenceGraph.contains((left, right))) Precedence.RightBindsTighter
      else if (precedenceGraph.contains((right, left))) Precedence.LeftBindsTighter
      else Precedence.Ambiguous
  }
  
  private def transitiveClosureGraph(relations: List[PartialOrd]): Set[(TokenKind, TokenKind)] = {
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
  
  val precedenceRules = precedenceComparison(PrecedenceBuilder { builder =>
    given PrecedenceBuilder = builder
    Set(`||`) ?< Set(`&&`)
    Set(`&&`) ?< Set(`===`, `!==`)
    
  })
}