object Solution {

  def main(args: Array[String]) {
    val t = readInt
    for (_ <- 1 to t) {
      val _ = readLine
      val s = readLine.distinct.sorted.toList
      val result = s.foldRight(List[List[Char]]()) { (x, y) =>
        List(x) :: y.map(x :: _) ::: y
      }
      println(result.map(_.mkString).mkString("\n"))
    }
  }
}
