object Solution {

  def largestAnagram(x: Long): Long = {
    val digits = Array.fill(10)(0)
    var xx = x
    while (xx > 0) {
      digits((xx % 10).toInt) += 1
      xx /= 10
    }
    var result = 0L
    for (digit <- 9 to 0 by -1) {
      while (digits(digit) > 0) {
        result = result * 10 + digit
        digits(digit) -= 1
      }
    }
    result
  }

  def largestSquare(digits: Int): Long = {
    val uplimits = math.pow(10, digits).toLong
    val sqrtLimit = math.sqrt(uplimits - 1).toInt
    val squareMap = (0L to sqrtLimit).map(x => x * x)
    val anagramMap = new scala.collection.mutable.HashMap[Long, (Long, Int)] {
      override def default(x: Long) = (0L, 0)
    }
    var i = 0
    while (i <= sqrtLimit) {
      val square = squareMap(i)
      val largestNumber = largestAnagram(square)
      val (largest, number) = anagramMap(largestNumber)
      anagramMap(largestNumber) = (square max largest, number + 1)
      i += 1
    }
    val (largest, number) = anagramMap.values.maxBy(x => (x._2, x._1))
    largest
  }

  def main(args: Array[String]) {
    val n = readInt
    println(largestSquare(n))
  }
}
