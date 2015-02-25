object Solution {

  def partition(nums: Array[Int]): Array[Int] = {
    if (nums.size <= 1) return nums
    val p = nums(0)
    val lessThan = nums.filter(_ < p)
    val equalTo = nums.filter(_ == p)
    val greaterThan = nums.filter(_ > p)
    lessThan ++ equalTo ++ greaterThan
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    println(partition(nums).mkString(" "))
  }
}
