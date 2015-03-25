object Solution {

  def binSearch(arr: IndexedSeq[Char], s: Char): Int = {
    var lo = 0; var hi = arr.size
    while (hi > lo) {
      val mid = lo + (hi - lo) / 2
      if (arr(mid) > s) hi = mid
      else if (arr(mid) < s) lo = mid + 1
      else return mid
    }
    lo
  }

  def nextString(s: String): Option[String] = {
    val sv = s.toVector
    var n = sv.size - 1
    while (n >= 1 && sv(n) <= sv(n - 1)) {
      n -= 1
    }
    if (n == 0) None
    else {
      val (left, right) = sv.splitAt(n)
      val remain = left.slice(0, n - 1)
      val toBeInserted = left.last
      val toBeSearched = right.reverse
      var lo = binSearch(toBeSearched, toBeInserted)
      while (lo < toBeSearched.size && toBeSearched(lo) == toBeInserted) lo += 1
      val toBeLeader = toBeSearched(lo)
      val sorted = toBeSearched.updated(lo, toBeInserted)
      Option(((remain :+ toBeLeader) ++ sorted).mkString)
    }
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val s = readLine
      nextString(s) match {
        case Some(x) => println(x)
        case None => println("no answer")
      }
    }
  }
}
