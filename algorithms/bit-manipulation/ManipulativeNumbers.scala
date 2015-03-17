object Solution {
  import scala.collection.mutable.HashMap

  // see
  // http://stackoverflow.com/questions/11161465/existence-of-a-permutation-under-constraints-interview-street-manipulative-nu
  // the second answer(Tuxdude's)
  def manipulativeK(nums: Array[Int]): Int = {
    val n = nums.size
    val maxBits = nums.max.toBinaryString.length - 1
    for (ki <- maxBits to 0 by -1) {
      val bitCounts = new HashMap[Int, Int]() {
        override def default(x: Int) = 0
      }
      for (num <- nums) bitCounts(num >> ki) += 1
      if (bitCounts.forall(x => x._2 <= n / 2)) return ki
    }
    -1
  }

  def main(args: Array[String]) {
    val n = readInt
    val nums = readLine.split(" ").map(_.toInt)
    println(manipulativeK(nums))
  }
}
