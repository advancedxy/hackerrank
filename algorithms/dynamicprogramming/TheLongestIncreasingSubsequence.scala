object Solution {

  // see
  // http://www.geeksforgeeks.org/longest-monotonically-increasing-subsequence-size-n-log-n/
  // for more details.
  def largestIncSubseq(nums: Array[Int]): Int = {
    def binSearch(nums: Array[Int], lo: Int, hi: Int, k: Int): Int = {
      var r = hi
      var l = lo
      while (r - l >= 0) {
        val m = l + (r - l) / 2
        if (nums(m) > k) r = m - 1
        else if (nums(m) < k) l = m + 1
        else return m
      }
      l
    }

    val tailTable = Array.fill(nums.size)(0)
    tailTable(0) = nums(0)
    var len = 1
    for (i <- 1 until nums.size) {
      if (nums(i) < tailTable(0))
        tailTable(0) = nums(i)
      else if (nums(i) > tailTable(len - 1)) {
        tailTable(len) = nums(i)
        len += 1
      }
      else tailTable(binSearch(tailTable, 0, len - 1, nums(i))) = nums(i)
    }

    len
  }

  def main(args: Array[String]) {
    val n = readInt
    val nums = (0 until n).map(x => readInt).toArray
    println(largestIncSubseq(nums))
  }
}
