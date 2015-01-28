object Solution {

  val powerOf10 = ((1 to 18).foldLeft(List(1l)) { (x, y) =>
     10 * x.head :: x
  }).reverse.toVector
  val headTailTable = ((1 to 18).foldLeft(List((0l, 0l))) { (x, y) =>
    val start = x.head._2 + 1
    val numbers = powerOf10(y) - powerOf10(y - 1)
    val end = start + y * numbers - 1
    (start, if (end > 0) end else Long.MaxValue) :: x
  }).reverse.toVector

  def binarySearchIndex(htTable: IndexedSeq[(Long, Long)], idx: Long): Int = {
    var s = 0
    var e = htTable.size
    var m = (e - s) / 2 + s
    while (m >= s) {
      val (start, end) = htTable(m)
      if (idx >= start && idx <= end) return m
      else if (idx < start) e = m
      else s = m + 1
      m = (e - s) / 2 + s
    }
    -1
  }

  def digitOfIndex(idx: Long): Int = {
    val n = binarySearchIndex(headTailTable, idx)
    val (start, end)= headTailTable(n)
    val numberStart = powerOf10(n - 1) - 1
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
