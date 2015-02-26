object Solution {

  def quickSort(nums: Array[Int]) {

    def partition(nums: Array[Int]): Array[Int] = {
      if (nums.size <= 1) return nums
      val p = nums(0)
      val lessThan = nums.filter(_ < p)
      val equalTo = nums.filter(_ == p)
      val greaterThan = nums.filter(_ > p)
      val sortedArray = partition(lessThan) ++ equalTo ++ partition(greaterThan)
      println(sortedArray.mkString(" "))
      sortedArray
    }

    partition(nums)
  }

  def quickSort3Way(nums: Array[Int]) {

    def exch(i: Int, j: Int) {
      var temp = nums(i)
      nums(i) = nums(j)
      nums(j) = temp
    }

    def threeWayPartition(lo: Int, hi: Int) {
      if (lo >= hi) return;
      val p = nums(lo)
      var lt = lo; var gt = hi; var i = lo + 1;
      while (i <= gt) {
        if (nums(i) < p) {
          exch(i, lt)
          i += 1
          lt += 1
        }
        else if (p < nums(i)) {
          exch(i, gt)
          gt -= 1
        }
        else i += 1
      }
      threeWayPartition(lo, lt - 1)
      threeWayPartition(gt + 1, hi)
      println(nums.slice(lo, hi + 1).mkString(" "))
    }

    threeWayPartition(0, nums.size - 1)
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    quickSort(nums)
  }
}
