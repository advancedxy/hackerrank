object Solution {

  def numberOfAnagrams(s: String): Int = {
    val occMap = new scala.collection.mutable.HashMap[Map[Char, Int], Int]() {
      override def default(x: Map[Char, Int]): Int = 0
    }
    val n = s.size
    for {
      i <- 0 until n
      j <- i + 1 to n
    } {
      val occ = new scala.collection.mutable.HashMap[Char, Int] () {
        override def default(x: Char): Int = 0
      }
      for (idx <- i until j) occ(s(idx)) += 1
      occMap(occ.toMap) += 1
    }

    occMap.map(_._2).filter(_ >= 2).map(x => x * (x - 1) / 2).sum
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val s = readLine
      println(numberOfAnagrams(s))
    }
  }
}
