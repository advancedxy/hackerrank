object Solution {
  
  def main(args: Array[String]) {
    val Array(_, t) = readLine.split(" ").map(_.toInt)
    val lanes = readLine.split(" ").map(_.toInt)
    for (_ <- 1 to t) {
      val Array(i, j) = readLine.split(" ").map(_.toInt)
      println(lanes.slice(i, j + 1).min)
    }
  }
}
