object Solution {

  def main(args: Array[String]) {
    val Array(n, m) = readLine.split(" ").map(_.toInt)
    val nums = (1 to n).map(_ => BigInt(readLine, 2))
    val counts = nums.combinations(2).map( { case Vector(x, y) =>
      (x | y).bitCount
    } ).toList
    val maxCount = counts.max
    val maxCountC = counts.filter(_ == maxCount).size
    println(maxCount)
    println(maxCountC)
  }
}
