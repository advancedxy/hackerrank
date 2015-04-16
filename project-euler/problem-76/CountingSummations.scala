object Solution {

  def ways(limit: Int): Array[Int] = {
    val p = 1e9.toInt + 7
    val result = Array.fill(limit + 1)(0)
    result(0) = 1
    for {
      i <- 1 to limit
      j <- i to limit
    } result(j) = (result(j) + result(j - i)) % p
    // result will be all the summations include the single 1 integer.
    result
  }

  def main(args: Array[String]) {
    val t = readInt
    val tcs = (1 to t).map(x => readInt)
    val result = ways(tcs.max)
    for (t <- tcs) {
      println(result(t) - 1)
    }
  }
}
