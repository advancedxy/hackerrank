object Solution {
  type Grid = Vector[Vector[Int]]
  type Point= (Int, Int)

  def largestProduct(grid: Grid): Int = {
    
    def isInGrid(point: Point): Boolean = {
      val (x, y) = point
      x >= 0 && x < grid.length && y >= 0 && y < grid(x).length
    }

    def isValid(point: Point, dir: String, length: Int = 4): Boolean =
      isInGrid(point) && (dir match {
        case "Right" => isInGrid(point._1, point._2 + length - 1)
        case "Down" => isInGrid(point._1 + length - 1, point._2)
        case "Diag" => isInGrid(point._1 + length -1, point._2 + length - 1)
        case "ADiag" => isInGrid(point._1 - length + 1, point._2 + length - 1)
        case _ => false
      })
    val length = 4

    val products = for {
      m <- 0 until grid.length
      n <- 0 until grid(m).length
      dir <- List("Right", "Down", "Diag", "ADiag")
      if isValid((m, n), dir, length)
      } yield {
        (0 until length) map { x =>
          dir match {
            case "Right" => grid(m)(n + x)
            case "Down" => grid(m + x)(n)
            case "Diag" => grid(m + x)(n + x)
            case "ADiag" => grid(m - x)(n + x)
          }
        } product
      }

    products.max
  }
   
  def main(args: Array[String])  {
    import scala.io.Source.stdin
    val grid = stdin.getLines.toVector.map(_.split(" ").map(_.toInt).toVector)
    println(largestProduct(grid))
  }
}
