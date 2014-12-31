import scala.collection.mutable

object Solution {
  val upperBound = 5e6.toInt
  val sequencesCounter = Array.fill(upperBound + 1)(-1)
  val largestNumberIndexer = Array.fill(upperBound + 1)(0)
  var alreadyCaculatedIndex = 1
  largestNumberIndexer(1) = 1

  // we cannot cache all the intermediate result produced by sequences(n),
  // however caching all the input should be suffice. When the intput n exceed
  // upperBound, just compute it, it may have hundreds of intermediate results,
  // take 1001063 for example. And the input n will exceeds Int.MaxValue, 
  // use Long instead.
  def sequences(n: Long): Int = n match {
    case x if x <= 0 => throw new Exception("n <= 0, this should never happen.")
    case 1 => 
      sequencesCounter(1) = 0
      0
    case x if x > 1 && x <= upperBound =>
      if (sequencesCounter(x.toInt) == -1)  // not in the cache
        sequencesCounter(x.toInt) = if(x % 2 == 0) 1 + sequences(n / 2) 
                                    else 1 + sequences(3 * n + 1)
      sequencesCounter(x.toInt)
    case x => if (x % 2 == 0) 1 + sequences(n / 2) else 1 + sequences(3 * n + 1)
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      var maxIndex = largestNumberIndexer(alreadyCaculatedIndex)
      for (j <- alreadyCaculatedIndex + 1 to n) {
        val num = sequences(j)
        if (num >= sequences(maxIndex)) {
          largestNumberIndexer(j) = j
          maxIndex = j
        }
        else largestNumberIndexer(j) = maxIndex
      }
      if (n > alreadyCaculatedIndex) alreadyCaculatedIndex = n
      println(largestNumberIndexer(n))
    }
  }
}
