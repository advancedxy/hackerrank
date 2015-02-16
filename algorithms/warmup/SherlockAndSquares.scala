object Solution {
  
  def sumOfSquaresBetweenAAndB(a: Int, b: Int): Int = {
    val sqrtA = math.ceil(math.sqrt(a)).toInt
    val sqrtB = math.floor(math.sqrt(b)).toInt
    sqrtB - sqrtA + 1 // a <= b is guaranteed.
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val Array(a, b) = readLine.split(" ").map(_.toInt)
      println(sumOfSquaresBetweenAAndB(a, b))
    }
  }
}
