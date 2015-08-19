object Solution {
  def main(args: Array[String]) = {
    val _ = readInt
    val ns = readLine.split(" ").map(_.toInt)
    println(ns.sum)
  }
}
