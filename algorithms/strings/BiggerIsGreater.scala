object Solution {

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
      var lo = 0; var hi = toBeSearched.size
      var mid = 0
      // binary search the insert index
      while (hi > lo) {
        mid = lo + (hi - lo) / 2
        if (toBeSearched(mid) > toBeInserted) {
          hi = mid
        }
        else if (toBeSearched(mid) < toBeInserted) {
          lo = mid + 1
        }
        else {
          // we find the number, we should break from the loop and set lo to be
          // the index that we can insert into.
          lo = mid
          hi = -1
        }
      }
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
