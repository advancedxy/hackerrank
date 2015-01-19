object Solution {
  val moduloP = 1e9.toInt + 7
  val upperBound = 1e5.toInt
  val ways = Array.fill(upperBound + 1)(0)
  ways(0) = 1
  val coinSizes = Array(1, 2, 5, 10, 20, 50, 100, 200)
  for {i <- 0 until coinSizes.length
       j <- coinSizes(i) to upperBound} {
     ways(j) = (ways(j) + ways(j - coinsSize(i))) % moduloP
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      println(ways(n))
    }
  }
}
