object Solution {

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(n, k) = readLine.split(" ").map(_.toInt)
      val nums = readLine.split(" ").map(_.toInt)
      if (nums.filter(_ <= 0).size < k) println("YES")
      else println("NO")
    }
  }
}
