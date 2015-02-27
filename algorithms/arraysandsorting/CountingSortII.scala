object Solution {

  def countSort(nums: Array[Int]): Array[Int] = {
    val occCounts = Array.fill(100)(0)
    for (n <- nums) occCounts(n) += 1
    (for ((c, i) <- occCounts.zipWithIndex if c > 0) yield {
      Array.fill(c)(i)
    }).flatten
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    println(countSort(nums).mkString(" "))
  }
}
