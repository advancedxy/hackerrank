object Solution {

  def main(args: Array[String]) {
    val Array(n, m) = readLine.split(" ").map(_.toInt)
    val nums = (1 to n).map(_ => BigInt(readLine, 2))
    nums.combinations(2).map( { case Array(x, y) =>
      (x ^ y).bitCount
    } )
  }
}
