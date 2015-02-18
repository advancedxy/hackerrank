object Solution {

  def theBeastOfN(n: Int): String = {
    var numOf3 = 0
    var numOf5 = n - numOf3
    while (numOf3 <= n) {
      numOf5 = n - numOf3
      if (numOf5 % 3 == 0) return "5" * numOf5 + "3" * numOf3
      numOf3 += 5
    }
    "-1"
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val n = readLine.toInt
      println(theBeastOfN(n))
    }
  }
}
