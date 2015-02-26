object Solution {
  
  def quickSort(nums: Array[Int]) {
    
    def exch(i: Int, j: Int) {
      if (i == j) return;
      val temp = nums(i)
      nums(i) = nums(j)
      nums(j) = temp
    }

    def partition(lo: Int, hi: Int) {
      if (lo >= hi) return;
      var lt, eq, i = lo;
      val p = nums(hi)
      while(i < hi) {
        if (nums(i) < p) {
          exch(lt, i)
          exch(eq, i)
          lt += 1
          eq += 1
          i += 1
        }
        else if (p < nums(i)) {
          i += 1
        }
        else {
          exch(eq, i)
          eq += 1
          i += 1
        }
      }
      exch(lt, i) // swap the last element to the middle
      println(nums.mkString(" "))
      partition(lo, lt - 1)
      partition(eq, hi)
    }

    partition(0, nums.size - 1)
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    quickSort(nums)
  }
}
