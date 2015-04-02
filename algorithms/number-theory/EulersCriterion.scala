object Solution {

  // see http://en.wikipedia.org/wiki/Euler%27s_criterion
  def canQuadraticResidueMod(a: Int, m: Int): Boolean = {
    if (m == 2 || a == 0) true
    else {
      BigInt(a).modPow((m - 1) / 2, m) == 1
    }
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(a, m) = readLine.split(" ").map(_.toInt)
      if (canQuadraticResidueMod(a, m)) println("YES") else println("NO")
    }
  }
}
