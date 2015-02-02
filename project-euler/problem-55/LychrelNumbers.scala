object Solution {
  import scala.collection.mutable
  
  def genConvergeNums(n: Int): Array[BigInt] = {
    val cache = mutable.Map.empty[BigInt, BigInt]
    
    def recursiveCalculateConverge(x: BigInt, c: Int): BigInt = {
      require(x >= 0)
      if (x < 10) return x
      if (cache.contains(x)) cache(x)
      else {
        if (c >= 60) {
          cache(x) = BigInt(-1)
          cache(x)
        }
        else {
          val xs = x.toString
          val rxs = xs.reverse
          if (xs == rxs) {
            cache(x) = x
            x
          }
          else {
            val rx = BigInt(rxs)
            val convergeNum = recursiveCalculateConverge(x + rx, c + 1)
            cache(x) = convergeNum
            // The leading zero check would be sufficient to guarantee x and rx
            // converge to same number
            if (rxs(0) != '0') cache(rx) = convergeNum
            convergeNum
          }
        }
      }
    }

    (0 to n).toArray.map(recursiveCalculateConverge(_, 0))
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val converges = genConvergeNums(n)
    val occ = new mutable.HashMap[BigInt, Int] { override def default(k: BigInt) = 0 }
    for (x <- converges if x != -1) occ(x) += 1
    val (k, c)= occ.maxBy(_._2)
    println(k + " " + c)
  }
}
