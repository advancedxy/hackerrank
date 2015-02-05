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
    import scala.collection.mutable.LinkedHashSet

    def isPoly(n: Int) = comb.exists(s => isNgonal(s, n))
    def slides(n: Int) = comb collect { case s if isNgonal(s, n) => s }

    val nums = comb.toList.map({ n =>
      (1000 to 9999).filter(isNgonal(n, _)).toList
    })
    
    val resultSet = LinkedHashSet.empty[List[Int]]
    
    def searchNums(result: LinkedHashSet[Int], vistedCategory: Set[Int]) {
      for {
        j <- 0 until comb.size if !vistedCategory(j)
        ele <- nums(j) if ele / 100 == result.last % 100 && !result(ele)
      } {
        val newResult = result + ele
        if (newResult.size == comb.size) {
          if (newResult.head / 100 == newResult.last % 100)
            resultSet.add(newResult.toList)
        }
        else {
          searchNums(newResult, vistedCategory + j)
        }
      }
    }

    for (ele <- nums(0)) searchNums(LinkedHashSet(ele), Set(0))

    resultSet
  }

  def main(args: Array[String]) {
    val _ = readLine
    val comb = readLine.split(" ").map(_.toInt)
    val result = genCyclics(comb).map(_.sum).toList.sorted
    result foreach println
  }
}
