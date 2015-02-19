object Solution {

  def getCavityMap(m: Seq[Seq[Int]]): Seq[Seq[Char]] = {
    val n = m.size
    val fourDirections = Vector((0, 1), (0, -1), (-1, 0), (1, 0))
    val points = scala.collection.mutable.Set.empty[(Int, Int)]
    for {
      i <- 1 until n - 1
      j <- 1 until n - 1
    } {
      if (fourDirections.forall(x => m(i)(j) > m(i + x._1)(j + x._2)))
        points.add((i, j))
    }
    for {
      i <- 0 until n
    } yield {
      (0 until n).map(j => if (points((i, j))) 'X' else (m(i)(j) + '0').toChar)
    }
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val map = (1 to n).map(x => readLine.map(_.asDigit))
    println(getCavityMap(map).map(_.mkString).mkString("\n"))
  }
}
