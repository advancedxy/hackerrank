object Solution {

  def quickSelect(nums: Array[Int], kth: Int): Int = {
    assert(kth >= 1 && kth <= nums.size)

    def exch(i: Int, j: Int) {
      val temp = nums(i)
      nums(i) = nums(j)
      nums(j) = temp
    }

    def quickSelect(lo: Int, hi: Int, kth: Int): Int = {
      println(lo, hi, kth)
      var lt = lo; var gt = hi; var i = lo + 1
      val p = nums(lo)
      while (i <= gt) {
        if (nums(i) < p) {
          exch(i, lt)
          i += 1
          lt += 1
        }
        else if (nums(i) > p) {
          exch(i, gt)
          gt -= 1
        }
        else i += 1
      }
      if (kth >= lt - lo && kth <= gt - lo) p
      else if (kth < lt - lo) quickSelect(lo, lt - 1, kth)
      else quickSelect(gt + 1, hi, kth - gt + lo - 1)
    }

    quickSelect(0, nums.size - 1, kth - 1)
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val nums = readLine.split(" ").map(_.toInt)
    val median =
      if (n % 2 == 0) (quickSelect(nums, n / 2) + quickSelect(nums, n / 2 + 1)) / 2
      else quickSelect(nums, n / 2 + 1)
    println(median)
  }
}
