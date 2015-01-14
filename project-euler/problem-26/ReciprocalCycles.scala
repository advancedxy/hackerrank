object Solution {

   def numberOfCycleDigits(n: Int, d: Int): Int = {
    val remainderMap = collection.mutable.Map.empty[Int, Int]
    var num = n
    for (i <- 1 to d) {
      val remainder = num % d
      if (remainder == 0) return 0
      if (remainderMap.contains(remainder)) {
        return remainderMap.size - remainderMap(remainder) + 1
      }
      remainderMap(remainder) = i
      num = 10 * remainder
    }
    remainderMap.size
  }

  def main(args: Array[String]) {
    val upperBound = 1e4.toInt
    val numberCycleCountMap = (1 to upperBound).map(x => (x,
      numberOfCycleDigits(1, x)))
    var longest = 0
    var index = 1
    val indexArray = Array.fill(upperBound + 1)(0)
    for ((d, count) <- numberCycleCountMap) {
      indexArray(d) = index
      if (count > longest) {
        longest = count
        index = d
      }
    }
    val t = readLine.toInt
    for (i <- 1 to t) {
      var n = readLine.toInt
      println(indexArray(n))
    }
  }
}
