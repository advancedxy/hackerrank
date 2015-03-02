object Solution {

  def testSorted(nums: Array[Int]): String = {
    def isSorted(nums: Array[Int], lo: Int, hi: Int): Boolean =
      (lo + 1 to hi).forall(x => nums(x) >= nums(x - 1))

    def exch(i: Int, j: Int) {
      val temp = nums(i)
      nums(i) = nums(j)
      nums(j) = temp
    }

    if (isSorted(nums, 0, nums.size - 1)) "yes"
    else {
      val sortedNums = nums.sorted
      val leftIndex = (0 until nums.size).find(x => nums(x) != sortedNums(x)).get
      val rightIndex =
        (nums.size - 1 to 0 by -1).find(x => nums(x) != sortedNums(x)).get
      exch(leftIndex, rightIndex)
      if (isSorted(nums, leftIndex, rightIndex))
        s"yes\nswap ${leftIndex+1} ${rightIndex+1}"
      else {
        exch(leftIndex, rightIndex)
        val seg = nums.slice(leftIndex, rightIndex + 1).reverse
        if (isSorted(seg, 0, seg.size - 1))
          s"yes\nreverse ${leftIndex+1} ${rightIndex+1}"
        else "no"
      }
    }
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val nums = readLine.split(" ").map(_.toInt)
    println(testSorted(nums))
  }
}
