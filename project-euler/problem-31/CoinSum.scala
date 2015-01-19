object Solution {
  val moduloP = 1e9.toInt + 7
  val coinSizes = Array(1, 2, 5, 10, 20, 50, 100, 200)

  def countTable(coinSizes: Array[Int], n: Int): Array[Int] = {
    val ways = Array.fill(n + 1)(0)
    ways(0) = 1
    for {i <- 0 until coinSizes.length
         j <- coinSizes(i) to n} {
      ways(j) = (ways(j) + ways(j - coinSizes(i))) % moduloP
    }
    ways
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    val testCases = (1 to t).map(x => readLine.toInt)
    val n = testCases.max
    val table = countTable(coinSizes, n)
    testCases foreach { x =>
      println(table(x))
    }
  }
}
