object Solution {
  
  def binSearch(nums: Array[Int], v: Int): Int = {

    def binSearchAux(nums: Array[Int], lo: Int, hi: Int): Int = {
      if (lo >= hi) return -lo - 1
      val mid = lo + (hi - lo) / 2
      if (nums(mid) > v) binSearchAux(nums, lo, mid)
      else if (nums(mid) < v) binSearchAux(nums, mid + 1, hi)
      else mid
    }
    binSearchAux(nums, 0, nums.size)
  }

  def main(args: Array[String]) {
    val v = readLine.toInt
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    println(binSearch(nums, v))
  }
}
