import scala.io.Source.stdin
object Solution {
  def main(args: Array[String]) {
    val num = stdin.getLines.toList.tail.map(BigInt(_))
    println(num.sum.toString.take(10))
  }
}
