object Solution {
  import scala.collection.mutable
  
  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = mutable.Map.empty[A, B]
    def apply(x: A) = cache getOrElseUpdate (x, f(x))
  }
  
  val modulo = 1e9.toInt + 7
  val paths: Memo[(Int, Int), Int] = Memo {
    case (x, y) if x < 0 || y < 0 => 0
    case (0, 0) => 0
    case (0, y) => 1
    case (x, 0) => 1
    case (x, y) => (paths(x - 1, y) + paths(x, y - 1)) % modulo
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val Array(m, n) = readLine.trim.split(" ").map(_.toInt)
      println(paths(m, n))
    }
  }
}
