object Solution {
  
  def minUnFairness(nums: IndexedSeq[Int], k: Int): Int = {
    val numsSorted = nums.sorted
    (0 to nums.size - k).map(i => numsSorted(i + k - 1) - numsSorted(i)).min
  }

  def main(args: Array[String]) {
    val (t, k) = (readLine.toInt, readLine.toInt)
    val nums = (1 to t).map(x => readLine.toInt)
    println(minUnFairness(nums, k))
  }
}
