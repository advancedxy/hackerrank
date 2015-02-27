object Solution {

  def countSum(nums: Seq[Int]): Seq[Int] = {
    val occCount = Array.fill(100)(0)
    for (num <- nums) occCount(num) += 1
    var sum = 0
    for (c <- occCount) yield {
      sum += c
      sum
    }
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val nums = (1 to n).map(x => readLine.split(" ")(0).toInt)
    println(countSum(nums).mkString(" "))
  }
}
