object Solution {

  def mergeSortCounting(nums: Array[Int]): Long = {
    var count = 0L

    def merge(nums: Array[Int], aux: Array[Int], lo: Int, mid: Int, hi: Int) {

      // copy to aux
      for (i <- lo to hi) aux(i) = nums(i)
      var i = lo; var j = mid + 1

      for (k <- lo to hi) {
        if (i > mid) {
          nums(k) = aux(j)
          j += 1
        }
        else if (j > hi) {
          nums(k) = aux(i)
          i += 1
        }
        else if (aux(j) < aux(i)) {
          nums(k) = aux(j)
          count += j - k
          j += 1
        }
        else {
          nums(k) = aux(i)
          i += 1
        }
      }
    }

    def mergeSort(nums: Array[Int], aux: Array[Int], lo: Int, hi: Int) {
      if (lo >= hi) return
      val mid = lo + (hi - lo) / 2
      mergeSort(nums, aux, lo, mid)
      mergeSort(nums, aux, mid + 1, hi)
      merge(nums, aux, lo, mid, hi)
    }

    mergeSort(nums, Array.fill(nums.size)(0), 0, nums.size - 1)
    count
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val _ = readLine
      val nums = readLine.split(" ").map(_.toInt)
      println(mergeSortCounting(nums))
    }
  }
}
