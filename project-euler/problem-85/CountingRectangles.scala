object Solution {

  def largestAreaForTarget(target: Int, countTable: IndexedSeq[Int]): Int = {
    def bsearch(target: Int, table: IndexedSeq[Int]): Int = {
      def bsearchHelper(lo: Int, hi: Int): Int = {
        if (lo >= hi) - lo - 1
        else {
          val mid = lo + (hi - lo) / 2
          if (target > table(mid)) bsearchHelper(mid + 1, hi)
          else if (target == table(mid)) mid
          else bsearchHelper(lo, mid)
        }
      }
      bsearchHelper(0, table.length)
    }
    val yIndex = bsearch(target, countTable)
    var y = if (yIndex >= 0) yIndex else -yIndex - 1
    var x = 0
    var cloestArea = 0
    var error = Int.MaxValue
    while (y >= x) {
      val area = countTable(x) * countTable(y)
      if (error > math.abs(area - target)) {
        error = math.abs(area - target)
        cloestArea = (x + 1) * (y + 1)
      }
      else if (error == math.abs(area - target)) {
        cloestArea = math.max(cloestArea, (x + 1) * (y + 1))
      }

      if (area > target) y -= 1
      else x += 1
    }
    cloestArea
  }

  def calculateCountTable(target: Int): IndexedSeq[Int] = {
    val upLimit = math.ceil(math.sqrt(2 * target)).toInt
    (1 to upLimit).map(x => x * (x + 1) / 2)
  }

  def main(args: Array[String]) {
    val t = readInt
    val targets = (1 to t).map(x => readInt)
    val maxTarget = targets.max
    val countTable = calculateCountTable(maxTarget)
    for (i <- targets) {
      println(largestAreaForTarget(i, countTable))
    }
  }
}
