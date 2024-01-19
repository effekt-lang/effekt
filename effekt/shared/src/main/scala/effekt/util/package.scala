package effekt
package util

def intercalate[A](a : List[A], b : List[A]): List[A] = a match {
  case first :: rest => first :: intercalate(b, rest)
  case _             => b
}
