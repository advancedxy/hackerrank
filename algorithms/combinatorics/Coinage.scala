object Solution {

  def waysToN(n: Int, coins: Array[Int]): Int = {
    val ways = Array.fill(n + 1) (0)
    ways(0) = 1
    val coinsWithCount = Array(1, 2, 5, 10).zip(coins)
    for {
      (coin, count) <- coinsWithCount
      sum <- n to coin by -1
      c <- coin to math.min(coin * count, sum) by coin
    } ways(sum) += ways(sum - c)

    // println(ways.zipWithIndex.mkString(" "))
    ways(n)
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readLine.toInt) {
      val n = readLine.toInt
      val coins = readLine.split(" ").map(_.toInt)
      println(waysToN(n, coins))
    }
  }
}
