object Solution {

  def doQuery(A: Array[Int], B: Array[Int], C: Array[Int]): Array[Long] = {
    val moduloP = 1e9.toInt + 7
    val n = A.size
    val m = B.size
    val result = A.map(_.toLong)
    val bWithIndex = B.zipWithIndex
    val bWithMuls = bWithIndex.groupBy(_._1)
      .mapValues(x => x.map(y => C(y._2)).foldLeft(1L) { (m, n) => m * n % moduloP } )

    for ((b, cm) <- bWithMuls if cm != 1) {
      var mul = b
      while (mul <= n) {
        result(mul - 1) = result(mul - 1) * cm % moduloP
        mul += b
      }
    }
    result
  }

  def main(args: Array[String]) {
    val _ = readLine
    val lines = (0 until 3).map(x => readLine)
    val Seq(as, bs, cs) = lines.map(l => l.split(" ").map(_.toInt))
    val result = doQuery(as, bs, cs)
    println(result.mkString(" "))
  }
}
