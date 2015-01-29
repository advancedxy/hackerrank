object Solution {
  
  def genPentagonNumbers(n: Int): (Array[Long], Set[Long]) = {
    val result = (1l to n).map(x => x * (3 * x - 1) / 2).toArray
    (result, result.toSet)
  }

  def isPentagonNumber(x: Long): Boolean = {
    val n = (math.sqrt(24.0 * x + 1) + 1) / 6
    n == n.toLong
  }
  
  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    var i = (k + 1).toLong
    var pk,pn = 0L
    while (i < n) {
      pn = i * (3 * i - 1) / 2
      pk = (i - k) * (3 * (i - k) - 1) / 2
      if (isPentagonNumber(pn + pk) || isPentagonNumber(pn - pk)) println(pn)
      i += 1
    }
  }
}
