object Solution {

  def main(args: Array[String]) {
    val _ = readInt
    val nums = readLine.split(" ").map(_.toInt)
    var zeros = 0
    val postZeros = (for (n <- nums.reverse) yield {
      val c = zeros
      if (n == 0) zeros += 1
      else zeros = 0
      c
    }).reverse
    for (_ <- 1 to readInt) {
      val Array(a, b) = readLine.split(" ").map(_.toInt)
      val zs = (b - a) min postZeros(a - 1)
      if (zs % 2 != 0 || nums(a - 1) % 2 != 0) println("Odd")
      else println("Even")
    }
  }
}
