object Solution {

  def pairs(nums: Array[Int]): Long = {
    val limit = nums.max
    val counts = Array.fill(limit + 1)(0)
    for (n <- nums) counts(n) += 1
    counts.map(x => 1L * x * (x - 1)).sum
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val _ = readLine
      val nums = readLine.split(" ").map(_.toInt)
      println(pairs(nums))
    }
  }
}
