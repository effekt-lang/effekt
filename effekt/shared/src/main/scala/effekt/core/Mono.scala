package effekt
package core

import effekt.context.Context
import effekt.lexer.TokenKind

object Mono extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "mono"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    Some(input)
  }

}