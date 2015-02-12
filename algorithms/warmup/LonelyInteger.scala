object Solution {

  def main(args: Array[String]) {
    val (_, nums) = (readLine, readLine.split(" ").map(_.toInt))
    val lonelyNumber = nums.foldLeft(0) { case (x, y) => x ^ y }
    println(lonelyNumber)
  }
}
