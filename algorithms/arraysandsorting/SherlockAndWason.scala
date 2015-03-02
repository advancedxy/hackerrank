object Solution {

  def main(args: Array[String]) {
    val Array(n, k, q) = readLine.split(" ").map(_.toInt)
    val nums = readLine.split(" ").map(_.toInt)
    for (_ <- 1 to q) {
      val idx = readLine.toInt
      val rIdx = (idx - k) % n
      val revIdx = if (rIdx < 0) rIdx + n else rIdx
      println(nums(revIdx))
    }
  }
}
