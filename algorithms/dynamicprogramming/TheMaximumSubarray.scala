object Solution {

  def maxSumOfContiguousSubarray(nums: Array[Int]): Int = {
    var sum = 0
    var result = sum
    for (n <- nums){
      sum += n
      result = result max sum
      if (sum <= 0) sum = 0
    }
    result
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val _ = readLine
      val nums = readLine.split(" ").map(_.toInt)
      val maxSum = nums.filter(_ >= 0).sum
      if (maxSum == 0) {
        val max = nums.max
        println(max + " " + max)
      }
      else println(maxSumOfContiguousSubarray(nums) + " " + maxSum)
    }
  }
}
