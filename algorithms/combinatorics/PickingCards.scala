object Solution {

  def ways(cards: Array[Int]): Long = {
    val n = cards.size
    val occCounts = Array.fill(n + 1)(0)
    for (c <- cards) occCounts(c) += 1
    var c = 0
    val leCounts = for (occ <- occCounts) yield {
      c += occ
      c
    }
    val p = 1e9.toInt + 7
    (0 until n).foldLeft(1L) { (x, y) =>
      x * (leCounts(y) - y) % p
    }
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val _ = readInt
      val cards = readLine.split(" ").map(_.toInt)
      println(ways(cards))
    }
  }
}
