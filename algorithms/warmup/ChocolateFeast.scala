object Solution {

  def getNumOfCholocates(n: Int, c: Int, m: Int): Int = {
    var result = n / c
    var wrapers = result
    while (wrapers >= m) {
      val newCandys = wrapers / m
      result += newCandys
      wrapers = newCandys + wrapers % m
    }
    result
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val Array(n, c, m) = readLine.split(" ").map(_.toInt)
      println(getNumOfCholocates(n, c, m))
    }
  }
}
