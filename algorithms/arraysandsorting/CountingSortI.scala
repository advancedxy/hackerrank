object Solution {

  // maybe we should assert all the number in the nums is less than 100
  def occCount(nums: Array[Int]): Array[Int] = {
    val result = Array.fill(100)(0)
    for (n <- nums) result(n) += 1
    result
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    println(occCount(nums).mkString(" "))
  }
}
