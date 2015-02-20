object Solution {

  def isTheArray(nums: Array[Int]): Boolean = {
    var sum = 0
    val sumArray = for (n <- nums) yield { sum += n; sum - n }
    (0 until nums.size).find(index => sum - nums(index) == 2 * sumArray(index))
                       .isDefined
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val _ = readLine
      val nums = readLine.split(" ").map(_.toInt)
      println(if (isTheArray(nums)) "YES" else "NO")
    }
  }
}
