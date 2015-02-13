object Solution {

  def numberOfDeletion(s: String): Int = {
    var previousC = ' '
    val ns = for (c <- s if c != previousC) yield { previousC = c; c }
    s.size - ns.size
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val s = readLine
      println(numberOfDeletion(s))
    }
  }
}
