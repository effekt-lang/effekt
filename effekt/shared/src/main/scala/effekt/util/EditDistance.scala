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
  val prefixLen = a.view.zip(b).takeWhile { case (a, b) => a == b }.size

  // Strip common suffix (but don't overlap with prefix)
  val suffixLen = a.view.drop(prefixLen).reverse
    .zip(b.drop(prefixLen).reverse)
    .takeWhile { case (a, b) => a == b }
    .size

  val s1 = a.substring(prefixLen, n - suffixLen)
  val s2 = b.substring(prefixLen, m - suffixLen)

  // Levenshtein
  val N = s1.length
  val M = s2.length

  // After stripping, if one is empty, distance is just the other's length
  if (s1.isEmpty) return Some(M)
  if (s2.isEmpty) return Some(N)

  val d = Array.ofDim[Int](N + 1, M + 1)
  for (i <- 0 to N) { d(i)(0) = i }
  for (j <- 0 to M) { d(0)(j) = j }

  for {
    i <- 1 to N; s1_i = s1(i - 1)
    j <- 1 to M; s2_j = s2(j - 1)
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

  val distance = d(N)(M)
  if (distance <= limit) Some(distance) else None
}