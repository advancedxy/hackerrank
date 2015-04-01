object Solution {
  import scala.annotation.tailrec

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def generateList(nums: IndexedSeq[Int]): IndexedSeq[Int] = {
    val paddedList = 1 +: nums :+ 1
    for (i <- 1 until paddedList.size) yield {
      val (x, y) = (paddedList(i), paddedList(i - 1))
      x * y / gcd(x, y)
    }
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val n = readInt
      val nums = readLine.split(" ").map(_.toInt)
      println(generateList(nums).mkString(" "))
    }
  }
}
