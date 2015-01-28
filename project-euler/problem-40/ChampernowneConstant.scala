object Solution {

  val headTailTable = ((1 to 18).foldLeft(List((0l, 0l))) { (x, y) =>
    val start = x.head._2 + 1
    val numbers = math.pow(10, y).toLong - math.pow(10, y - 1).toLong
    val end = start + y * numbers - 1
    (start, if (end > 0) end else Long.MaxValue) :: x
  }).reverse.toVector

  def digitOfIndex(idx: Long): Int = {
    val n = headTailTable.indexWhere(x => idx >= x._1 && idx <= x._2)
    val (start, end)= headTailTable(n)
    val numberStart = math.pow(10, n - 1).toLong - 1
    val number = idx - start + 1
    val (d, p) = (number / n + numberStart, number % n)
    if (p == 0) (d % 10).toInt
    else {
      var c = 0l
      var digits = d + 1
      for (i <- p to n) {c = digits % 10; digits /= 10}
      c.toInt
    }
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val digits = readLine.split(" ").map(x => digitOfIndex(x.toLong))
      println(digits.product)
    }
  }
}
