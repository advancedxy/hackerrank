object Solution {

  // limit can be 6e4, so the time complexity is 18e8, which times out on hr
  def ways(limit: Int): Array[Int] = {
    val p = 1e9.toInt + 7
    val result = Array.fill(limit + 1)(0)
    result(0) = 1
    var i = 1
    while (i <= limit) {
      var j = i
      while (j <= limit) {
        result(j) = (result(j) + result(j - i)) % p
        j += 1
      }
      i += 1
    }
    // result will be all the summations include the single 1 integer.
    result
  }

  def partitionWays(limit: Int): Array[Int] = {
    val p = 1e9.toInt + 7
    val result = Array.fill(limit + 1)(0)
    result(0) = 1
    val pentas = (for {
      i <- 1 to math.rint(math.sqrt(2 * limit / 3 + 1)).toInt
      j <- List(i, -i)
    } yield j * (3 * j - 1) / 2).filter(_ <= limit)

    for (i <- 1 to limit) {
      var j = 0
      while(j < pentas.size && pentas(j) <= i) {
        val sign = if (j % 4 > 1) -1 else 1
        val s = sign * result(i - pentas(j))
        result(i) = (result(i) + sign * result(i - pentas(j))) % p
        j += 1
      }
      // well there's subtraction, so the result can be negative.
      if (result(i) < 0) result(i) = result(i) + p
    }
    result
  }

  def main(args: Array[String]) {
    val t = readInt
    val tcs = (1 to t).map(x => readInt)
    val result = partitionWays(tcs.max)
    for (t <- tcs) {
      println(result(t))
    }
  }
}
