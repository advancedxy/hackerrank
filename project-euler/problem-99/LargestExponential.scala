object Solution {

  def main(args: Array[String]) = {
    val n = readInt
    val exps = for (i <- 1 to n) yield {
      val Array(b, e) = readLine.split(" ").map(_.toInt)
      (b, e)
    }
    val k = readInt
    val sortedExps = exps.sortBy(x => 1L * x._2 * math.log(x._1))
    val (b, e) = sortedExps(k - 1)
    println(s"$b $e")
  }
}
