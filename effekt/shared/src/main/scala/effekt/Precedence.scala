package effekt

import effekt.lexer.TokenKind

enum Associativity {
  case Left
  case Right
  case None
}

enum Precedence {
  case LeftBindsTighter
  case RightBindsTighter
  case Ambiguous
}

extension (looser: TokenKind) {
  def ?<(tighter: TokenKind)(using table: PrecedenceTable): Unit =
    table.relations = (looser, tighter) :: table.relations

  def =?=(tighter: TokenKind)(using table: PrecedenceTable): Unit =
    looser ?< tighter; tighter ?< looser
}

extension (looser: Set[TokenKind]) {
  def ?<(tighter: Set[TokenKind])(using table: PrecedenceTable): Unit = {
    for { l <- looser; t <- tighter } l ?< t
  }
  def =~=(tighter: Set[TokenKind])(using table: PrecedenceTable): Unit =
    for {l <- looser; t <- tighter} l =?= t
}

/**
 * Encodes the precedence of infix operators as a partial order.
 * Additionally, each operator's associativity is kept tracked of with a default of left-associativity if not specified otherwise.
 * For convenience, we break the antisymmetric property on purpose by defining `+ ?< -` as well as `- ?< +` such that for example `1 + 2 - 3` can be written without parentheses by
 * falling back to the operators' associativity for disambiguation.
 * 
 * Usage:
 * 
 */
class PrecedenceTable {
  var assocMap:   Map[TokenKind, Associativity] = Map.empty
  var relations:  List[(TokenKind, TokenKind)] = List.empty
  def operators = relations.flatMap { case (l, t) => Set(l, t) }.toSet

  def declare(assoc: Associativity, ops: TokenKind*): Unit =
    ops.foreach { op =>
      assocMap = assocMap + (op -> assoc)
    }

  private lazy val transitiveClosure: Set[(TokenKind, TokenKind)] = {
    var graph = relations.toSet
    var size  = 0
    while (size != graph.size) {
      size = graph.size
      for { (a, b) <- graph; (c, d) <- graph; if b == c }
        graph += ((a, d))
    }
    graph
  }

  def compare(left: TokenKind, right: TokenKind): Precedence = {
    // same operator, or explicitly declared at the same level: use associativity
    // left associative is the default if not declared otherwise
    if (left == right || transitiveClosure((left, right)) && transitiveClosure((right, left))) {
      (assocMap.getOrElse(left, Associativity.Left), assocMap.getOrElse(right, Associativity.Left)) match {
        case (Associativity.Left, _)  | (_, Associativity.Left)  => Precedence.LeftBindsTighter
        case (Associativity.Right, _) | (_, Associativity.Right) => Precedence.RightBindsTighter
        case _                                                   => Precedence.Ambiguous
      }
    } else if (transitiveClosure((left, right)))  Precedence.RightBindsTighter
    else if (transitiveClosure((right, left)))  Precedence.LeftBindsTighter
    else Precedence.Ambiguous
  }
}

object PrecedenceTable {
  def apply(init: PrecedenceTable => Unit): PrecedenceTable = {
    val table = new PrecedenceTable
    init(table)
    table
  }
}
