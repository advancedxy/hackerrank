object Solution {

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val n = readInt
      val nums = readLine.split(" ").map(_.toInt)
      if (n % 2 == 0) println(0)
      else {
        val result = (0 until n by 2).map(nums(_)).reduceLeft { _ ^ _ }
        println(result)
      }
    }
  }
}
