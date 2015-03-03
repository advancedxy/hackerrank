object Solution {

  def canProtectAll(r: Int, k: Int): Boolean = {
    val sr = math.sqrt(r).toInt
    val squares = (1 to sr).map(x => x * x).toSet
    val count = squares.filter(x => squares(r - x)).size
    val totalCount = if (sr * sr == r) 4 + 4 * count else 4 * count
    totalCount <= k
  }

  def main(args: Array[String]) {
    val t = readInt
    for (_ <- 1 to t) {
      val Array(r, k) = readLine.split(" ").map(_.toInt)
      if (canProtectAll(r, k)) println("possible") else println("impossible")
    }
  }
}

