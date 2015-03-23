object Solution {

  def main(args: Array[String]) {
    val strings = (1 to readInt).map(x => readLine.toSet))
    val intersect = strings.reduceLeft { _ & _ }
    println(intersect.size)
  }
}
