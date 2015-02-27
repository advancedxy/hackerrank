object Solution {

  def insertionSortShiftsCounter(nums: Array[Int]): Long = {
    val n = nums.size
    var numOfShifts = 0
    for (i <- 1 until n) {
      var j = i
      while (j >= 1 && nums(j) < nums(j - 1)) {
        numOfShifts += 1
        val temp = nums(j)
        nums(j) = nums(j - 1)
        nums(j - 1) = temp
        j -= 1
      }
    }
    numOfShifts
  }

  def quickSortSwapsCounter(nums: Array[Int]): Long = {
    var numOfSwaps = 0L

    def exch(i: Int, j: Int) {
      numOfSwaps += 1
      if (i == j) return;
      val temp = nums(i)
      nums(i) = nums(j)
      nums(j) = temp
    }

    def lomutoPartition(lo: Int, hi: Int) {
      if (lo >= hi) return;
      val p = nums(hi)
      var i = lo - 1
      for (j <- lo until hi if nums(j) <= p) {
          i += 1
          exch(i, j)
      }
      exch(i + 1, hi)
      lomutoPartition(lo, i)
      lomutoPartition(i + 2, hi)
    }
    
    lomutoPartition(0, nums.size - 1)
    numOfSwaps
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    val shifts = insertionSortShiftsCounter(nums.clone)
    val swaps = quickSortSwapsCounter(nums.clone)
    println(shifts - swaps)
  }
}
