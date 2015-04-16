object Solution {
  import scala.collection.mutable.{HashMap, LinkedHashSet}

  val factorialOfDigits = (1 to 9).foldLeft(Vector(1)) { (x, y) =>
    x :+ y * x.last
  }
  val limit = 1e6.toInt
  val chainCache = Array.fill(limit + 1)(0)
  val chainLengthCache = HashMap[Int, Int](
    169 -> 3, 363601 -> 3, 1454 -> 3, 871 -> 2,
    45361 -> 2, 872 -> 2, 45362 -> 2).withDefaultValue(0)

  def nextNumber(x: Int): Int = {

    def nextNumberAux(x: Int): Int = {
      var xx = x
      var result = 0
      while (xx >= 10) {
        result += factorialOfDigits(xx % 10)
        xx /= 10
      }
      // deal with x = 0
      result + factorialOfDigits(xx)
    }
    if (x <= limit) {
      if (chainCache(x) == 0) chainCache(x) = nextNumberAux(x)
      chainCache(x)
    }
    else nextNumberAux(x)
  }

  def chainLength(x: Int): Int = {

    def chainLenthSearch(x: Int, visted: LinkedHashSet[Int]) {
      // we find result in the cache
      if (chainLengthCache(x) != 0) {
        val cLen = chainLengthCache(x)
        var len = visted.size + cLen
        for (i <- visted) {
          if (i <= limit) {
            chainLengthCache(i) = len
          }
          len -= 1
        }
      }
      // we hit a cycle.
      else if (visted(x)) {
        val nums = visted.toVector
        val index = nums.indexOf(x)
        val (left, right) = nums.splitAt(index)
        var len = visted.size
        for (i <- left) {
          if (i <= limit) {
            chainLengthCache(i) = len
          }
          len -= 1
        }
        for (i <- right) chainLengthCache(i) = len
      }
      else {
        chainLenthSearch(nextNumber(x), visted + x)
      }
    }

    if (chainLengthCache(x) != 0) chainLengthCache(x)
    else {
      chainLenthSearch(x, LinkedHashSet.empty[Int])
      chainLengthCache(x)
    }
  }

  def numbersOfChainLength(n: Int, l: Int): Array[Int] = {
    (0 to n).filter(x => chainLength(x) == l).toArray
  }

  def main(args: Array[String]) {
    for (i <- 1 to readInt) {
      val Array(n, l) = readLine.split(" ").map(_.toInt)
      val numbers = numbersOfChainLength(n, l)
      if (numbers.size == 0) println(-1)
      else println(numbers.mkString(" "))
    }
  }
}
