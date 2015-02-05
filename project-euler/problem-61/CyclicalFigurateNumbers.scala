object Solution {
  
  def isPerfectSquare(n: Long) = {
    // http://stackoverflow.com/questions/295579/fastest-way-to-determine-if-an-integers-square-root-is-an-integer
    if (n <= 0) false else n & 0x3F match {
      case 0x00 | 0x01 | 0x04 | 0x09 | 0x10 | 0x11
        | 0x19 | 0x21 | 0x24 | 0x29 | 0x31 | 0x39 =>
        math.sqrt(n) % 1 == 0
      case _ => false
    }
  }
  def isPerfectSquare(n: Int): Boolean = isPerfectSquare(n.toLong)

  def isNgonal(s: Int, n: Int) = {
    val x = (math.sqrt(n * (8 * s - 16) +
             math.pow(s - 4, 2)) + s - 4) / (2 * s - 4)
    x == Math.rint(x)
  }

  def genCyclics(comb: Array[Int]) = {
    
    def isPoly(n: Int) = comb.exists(s => isNgonal(s, n))
    def slides(n: Int) = comb collect { case s if isNgonal(s, n) => s }

    val nums = comb.toList.map({ n =>
      (1000 to 9999).filter(isNgonal(n, _)).toList
    })

    def genCombines(ls: List[List[Int]]): List[List[Int]] = ls match {
      case Nil => Nil
      case head :: Nil => head.map(List(_))
      case head :: tail =>
        for {
          h <- head
          ls <- genCombines(tail) if h % 100 == ls.head / 100
        } yield h :: ls
    }

    def isCyclics(x: Array[Int]) = {
      val last = x(x.size - 1)
      (x(0) / 100 == last % 100) && x.zip(x.slice(1, x.size)).forall { y =>
        y._1 % 100 == y._2 / 100
      }
    }

  }

  def main(args: Array[String]) {
    val _ = readLine
    val comb = readLine.split(" ").map(_.toInt)
  }
}
