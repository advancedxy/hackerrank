object Solution {

  def canFitInString(ss: String, bs: String): Boolean = {
    var i,j = 0
    while (i < ss.size && j < bs.size) {
      while(j < bs.size && ss(i) != bs(j)) j += 1
      if (j == bs.size) return false
      i += 1
      j += 1
    }
    if (i == ss.size) true else false
  }

  def lexSmallestString(s: String): String = {
    val occ = Array.fill(26)(0)
    for (c <- s) occ(c - 'a') += 1
    val sOcc = occ.map(_ / 2)
    val rOcc = sOcc.clone
    val n = s.size
    val result = new StringBuilder(n / 2)
    var firstC = ('a' to 'z').find(x => sOcc(x - 'a') > 0)
    var accStack = Map.empty[Char, (Int, List[Int])].withDefaultValue(0, Nil) // Char -> (Count, Index)
    for (i <- n - 1 to 0 by -1) {
      //println(sOcc.mkString(" "), rOcc.mkString(" "), firstC, i, s(i))
      //println(result, accStack)
      firstC match {
        case Some(c) if c == s(i) =>
          if (sOcc(c - 'a') == 1) {
            sOcc(c - 'a') = 0
            result += c
            firstC = ((c + 1).toChar to 'z').find(x => sOcc(x - 'a') > 0)
            accStack = Map.empty[Char, (Int, List[Int])].withDefaultValue(0, Nil)
          }
          else if (sOcc(c - 'a') == 0) {
            rOcc(c - 'a') -= 1
          } else {
            sOcc(c - 'a') -= 1
            result += c
            accStack = Map.empty[Char, (Int, List[Int])].withDefaultValue(0, Nil)
          }
        case Some(c) if c != s(i) =>
          if (rOcc(s(i) - 'a') > 0) {
            rOcc(s(i) - 'a') -= 1
            val (c, idxs) = accStack(s(i))
            accStack = accStack.updated(s(i), (c + 1, i :: idxs))
          }
          else {
            val acc = accStack.filterKeys(x=> x > c && x < s(i)).toList.sortBy(_._1)
            var preIdx = Int.MaxValue
            for {
              (char, (count, idxs)) <- acc
              val cc = idxs.takeWhile(_ < preIdx).size
              if idxs.head <= preIdx && sOcc(char - 'a') >= cc
            } {
              val cc = idxs.takeWhile(_ < preIdx).size
              sOcc(char - 'a') -= cc
              rOcc(char - 'a') += cc
              preIdx = idxs.head min preIdx
              result ++= char.toString * cc
            }
            sOcc(s(i) - 'a') -= 1
            result += s(i)
            val (_, idxs) = accStack(s(i))
            val newIdxs = (i :: idxs).takeWhile(_ < preIdx).reverse.tail.reverse
            accStack = Map.empty[Char, (Int, List[Int])].withDefaultValue(0, Nil)
            if (newIdxs.size > 0)
              accStack = accStack.updated(s(i), (newIdxs.size, newIdxs))
          }
        case None => return result.result
      }
    }
    result.result
  }

  def main(args: Array[String]) {
    val s = readLine
    println(lexSmallestString(s))
  }
}
