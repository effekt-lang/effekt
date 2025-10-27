package effekt.util

def editDistance(a: String, b: String, limit: Int): Option[Int] = {
  val n = a.length
  val m = b.length
  if (n == 0) return Some(m)
  if (m == 0) return Some(n)

  // Early exit if minimum distance exceeds limit
  val minDist = Math.abs(n - m)
  if (minDist > limit) return None

  // Strip common prefix
  val prefixLen = a.view.zip(b.view).takeWhile { case (a, b) => a == b }.size

  // Strip common suffix (but don't overlap with prefix)
  val suffixLen = a.view.drop(prefixLen).reverse
    .zip(b.view.drop(prefixLen).reverse)
    .takeWhile { case (a, b) => a == b }
    .size

  val s1 = a.substring(prefixLen, n - suffixLen)
  val s2 = b.substring(prefixLen, m - suffixLen)

  // After stripping, if one is empty, distance is just the other's length
  if (s1.isEmpty) return Some(s2.length)
  if (s2.isEmpty) return Some(s1.length)

  val distance = levenshtein(s1, s2)
  if (distance <= limit) Some(distance) else None
}

inline def levenshtein(s1: String, s2: String): Int = {
  val n = s1.length
  val m = s2.length

  val d = Array.ofDim[Int](n + 1, m + 1)
  for (i <- 0 to n) { d(i)(0) = i }
  for (j <- 0 to m) { d(0)(j) = j }

  for {
    i <- 1 to n; s1_i = s1(i - 1)
    j <- 1 to m; s2_j = s2(j - 1)
  } do {
    val costDelete     = d(i - 1)(j) + 1
    val costInsert     = d(i)(j - 1) + 1
    val costSubstitute = d(i - 1)(j - 1) + (if (s1_i == s2_j) 0 else 1)

    d(i)(j) = costDelete min costInsert min costSubstitute

    // Transposition: swap adjacent characters (Bolo ~> Bool)
    if (i > 1 && j > 1 && s1(i - 1) == s2(j - 2) && s1(i - 2) == s2(j - 1)) {
      d(i)(j) = d(i)(j) min (d(i - 2)(j - 2) + 1)
    }
  }

  d(n)(m)
}