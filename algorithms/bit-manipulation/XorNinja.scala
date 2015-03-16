object Solution {

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val n = BigInt(2).pow(readInt - 1)
      val num = readLine.split(" ").map(_.toInt).foldLeft(0) { _ | _ }
      println(n * num % (1e9.toInt + 7))
    }
  }
}
