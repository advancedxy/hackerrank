object Solution {

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(x, y) = readLine.split(" ").map(_.toInt)
      val Array(w, b, z) = readLine.split(" ").map(_.toInt)
      val cost1 = 1L * x * w + 1L * y * b
      val cost2 =
        if (w > b) 1L * x * (b + z) + 1L * y * b
        else 1L * x * w + 1L * y * (w + z)
      println(cost1 min cost2)
    }
  }
}
