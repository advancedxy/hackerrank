object Solution {

  def partition(nums: Array[Int]): Array[Int] = {
    if (nums.size <= 1) return nums
    val p = nums(0)
    val lessThan = nums.filter(_ < p)
    val equalTo = nums.filter(_ == p)
    val greaterThan = nums.filter(_ > p)
    lessThan ++ equalTo ++ greaterThan
  }

  def threeWaysPartition(nums: Array[Int]): Array[Int] = {
    def exch(i: Int, j: Int) {
      val temp = nums(i)
      nums(i) = nums(j)
      nums(j) = temp
    }

    // we don't need to partition if it has only one element
    if (nums.size <= 1) return nums
    val p = nums(0)
    var lo = 0; var hi = nums.size - 1; var i = 1;
    while (i <= hi) {
      if (nums(i) < p) {
        exch(i, lo)
        i += 1
        lo += 1
      }
      else if (p < nums(i)) {
        exch(i, hi)
        hi -= 1
      }
      else i += 1
    }
    nums
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    println(partition(nums).mkString(" "))
  }
}
