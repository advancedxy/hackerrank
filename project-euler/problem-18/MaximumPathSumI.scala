object Solution {
  type Triangle = Vector[Vector[Int]]
  
  def maximumPathSum(t: Triangle): Int = {
    t reduce { (x, y) =>
      y.zipWithIndex map {
        case (v, i) if i == 0 => v + x(i)
        case (v, i) if i >= x.length => v + x(i - 1)
        case (v, i) => v + math.max(x(i - 1), x(i))
      }
    } max
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      val triangle = (1 to n).toVector.map(x => readLine.split(" ").map(_.toInt).toVector)
      println(maximumPathSum(triangle))
    }
  }
}
