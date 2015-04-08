object Solution {

  def main(args: Array[String]) {
    val Array(r, c) = readLine.split(" ").map(_.toInt)
    println(10L * ((r - 1) / 2) + ((r - 1) & 1) + 2 * (c - 1))
  }
}
