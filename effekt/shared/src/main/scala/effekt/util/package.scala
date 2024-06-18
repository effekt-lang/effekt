package effekt
package util

def intercalate[A](a : List[A], b : List[A]): List[A] = a match {
  case first :: rest => first :: intercalate(b, rest)
  case _             => b
}

extension(s: String) def escape: String = s.flatMap(_.escape)
extension(ch: Char) def escape: String = ch match {
  case '\b' => "\\b"
  case '\t' => "\\t"
  case '\n' => "\\n"
  case '\f' => "\\f"
  case '\r' => "\\r"
  case '"' => "\\\""
  //case '\'' => "\\\'"
  case '\\' => "\\\\"
  case ch if ch.toInt >= 32 && ch.toInt <= 126 => String.valueOf(ch)
  case ch => "\\u%04x".format(ch.toInt)
}