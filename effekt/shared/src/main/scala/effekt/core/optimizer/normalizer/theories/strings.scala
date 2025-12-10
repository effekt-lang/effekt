package effekt.core.optimizer.normalizer.theories

import effekt.core.Block.BlockVar
import effekt.core.{Expr, Id, Type}
import effekt.core.optimizer.normalizer.semantics.Neutral

/**
 * Theory for strings: free extension of the string monoid
 * Compare https://arxiv.org/pdf/2306.15375
 *
 * Invariant: there are no adjacent literals in a StringRep
 */
object strings {
  case class StringRep(value: List[String | Neutral]) {
    val free: Set[Id] = value.collect { case n: Neutral => n.free }.flatten.toSet
    def isLiteral: Boolean = value.length == 1 && value.head.isInstanceOf[String]
    def show: String = {
      val terms = value.map {
        case s: String => s""""$s""""
        case n: Neutral => "<neutral>"
      }
      terms.mkString(" ++ ")
    }
  }
  
  def embed(value: String): StringRep = StringRep(List(value))
  def embed(value: Neutral): StringRep = StringRep(List(value))
  
  def reify(value: StringRep, embedBuiltinName: String => BlockVar, embedNeutral: Neutral => Expr): Expr = value match {
    case StringRep(parts) =>
      parts.map {
        case s: String => Expr.Literal(s, Type.TString)
        case n: Neutral    => embedNeutral(n)
      }.reduceLeft { (l, r) =>
        Expr.PureApp(embedBuiltinName("effekt::infixConcat(String, String)"), List(), List(l, r))
      }
  }

  def concat(l: StringRep, r: StringRep): StringRep = (l, r) match {
    case (StringRep(xs), StringRep(ys)) =>
      (xs, ys) match {
        // fuse trailing / leading string literals at the boundary (if any)
        case (init :+ (s1: String), (s2: String) :: tail) =>
          val concatenated: String | Neutral = s1 + s2
          StringRep((init :+ concatenated) ::: tail)
        case _ =>
          StringRep(xs ::: ys)
      }
  }
}
