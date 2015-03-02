object Solution {
  import scala.collection.mutable.HashMap

  def pairs(nums: Array[Int]): Long = {
    val occMap = new HashMap[Int, Int]() {
      override def default(x: Int) = 0
    }

    for (n <- nums) occMap(n) += 1
    occMap.values.filter(_ > 1).map(x => 1L * x * (x - 1)).sum
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
