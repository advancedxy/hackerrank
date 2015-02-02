object Solution {

  def main(args: Array[String]) {
    val n = readLine.toInt
    val biggest = (0 until n).flatMap(x =>
                  (0 until n).map(BigInt(x).pow(_).toString.map(_.asDigit).sum))
                  .max
    println(biggest)
  }
}
