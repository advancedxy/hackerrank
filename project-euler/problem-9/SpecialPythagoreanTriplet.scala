object Solution {
  type Triplet = (Int, Int, Int)

  def findTriplet(n: Int): Option[Triplet] = {
    var c = n / 3 + 1 // suppose we have a < b < c
    while (c < n / 2) { // of course a + b > c
      // a + b = N - c; a^2 + b^2 = c^2 => 2ab = (N-c)^2 - c^2
      // a^2 - 2ab + b^2 = c^2 - (N-c)^2 + c^2 =  c^2 - N^2 + 2Nc
      // (a-b)^2 = c^2 - N^2 + 2Nc should be a square of natural number
      val squareOfABDiff = c * c - n * n + 2 * n * c
      val squareRoot = math.sqrt(squareOfABDiff).toInt
      if (squareRoot * squareRoot == squareOfABDiff) {
        val b = (n - c + squareRoot) / 2
        val a = n - c - b
        return Some((a, b, c)) // if we can find a result. Then this result will be the biggest as abc = -N(c - N/4)^2 + N*(N/4)^2
      }
      c += 1
    }
    return None
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      val tripleLet = findTriplet(n)
      if (tripleLet.isDefined) {
        val (a, b, c) = tripleLet.get
        println(a * b * c)
      } else {
        println("-1")
      }
    }
  }
}
