object Solution {

  // n: the first n square number.
  // m: number of cubes
  def countingPairs(n: Int, m: Int) = {
    val factorials =
      ((1 to 9).foldLeft(List(1)) { (x, y) => x.head * y :: x }).reverse.toVector
    var acc = Set[List[Set[Int]]](List.fill(m)(Set[Int]()))
    var visted = Set[Map[Int, Int]]()
    for (i <- 1 to n) {
      val square = (i * i).toString
      val digits = ("0" * (m - square.length) + square).toList.map(_.asDigit)
      val sig = digits.groupBy(x => x).mapValues(_.size)
      if (visted(sig)) {

      } else {
        acc = (for {
            ds <- digits.permutations
            seq <- acc
               } yield ds.zip(seq).map(x => if (x._1 == 9) x._2 + 6 else x._2 + x._1).sortBy(x => (x.size, x.sum))
        ).toSet.filter(x => x.forall(_.size <= 6))
      }
      visted  += sig
    }

    def combineNumber(x: Int, y: Int): Int = {
      if(x >= y && x <= 9 && y >= 0)
        factorials(x) / factorials(x - y) / factorials(y) 
      else 0
    }

    def countingSize(x: Set[Int]): Long = {
      val s = x.size
      if (x(6))
        2L * combineNumber(10 - s, 6 - s) - combineNumber(10 - s - 1, 6 - s - 1)
      else 1L * combineNumber(10 - s, 6 - s)
    }

    (acc, acc.map(x => x.map(countingSize(_)).product).sum)
  }

  def bruteForce(n: Int, m: Int): Int = {
    val cubes = (0 to 9)
      .combinations(6)
      .map( x => x.foldLeft(0) { (x, y) => x + (1 << y) } ).toVector

    def numberToDigits(n: Int, c: Int): IndexedSeq[Int] = {
      val s = n.toString
      ("0" * (c - s.length) + s).map(_.asDigit)
    }
    val squares = (1 to n).map(x => x * x).map(numberToDigits(_, m).toArray).toArray

    def testBit(qube: Int, digit: Int): Boolean = ((qube >>> digit) & 1) != 0

    def validQube1(x: Int): Boolean = {
      val xx =
        if (testBit(x, 6) || testBit(x, 9)) x | (1 << 6) | (1 << 9)
        else x

      for (sqr <- squares) {
        if (! testBit(xx, sqr(0))) return false
      }
      return true
    }

    def validQube2(x1: Int, x2: Int): Boolean = {
      val xx1 =
        if (testBit(x1, 6) || testBit(x1, 9)) x1 | (1 << 6) | (1 << 9)
        else x1
      val xx2 =
        if (testBit(x2, 6) || testBit(x2, 9)) x2 | (1 << 6) | (1 << 9)
        else x2

      for (sqr <- squares) {
        if (! (testBit(xx1, sqr(0)) && testBit(xx2, sqr(1)) || testBit(xx2, sqr(0)) && testBit(xx1, sqr(1))))
              return false
      }
      return true

    }

    def validQube3(x1: Int, x2: Int, x3: Int): Boolean = {
    val xx1 =
        if (testBit(x1, 6) || testBit(x1, 9)) x1 | (1 << 6) | (1 << 9)
        else x1
    val xx2 =
        if (testBit(x2, 6) || testBit(x2, 9)) x2 | (1 << 6) | (1 << 9)
        else x2

    val xx3 =
        if (testBit(x3, 6) || testBit(x3, 9)) x3 | (1 << 6) | (1 << 9)
        else x3

    for (sqr <- squares) {
      if(! (
           testBit(xx1, sqr(0)) && testBit(xx2, sqr(1)) && testBit(xx3, sqr(2)) || 
           testBit(xx1, sqr(0)) && testBit(xx3, sqr(1)) && testBit(xx2, sqr(2)) || 
           testBit(xx2, sqr(0)) && testBit(xx1, sqr(1)) && testBit(xx3, sqr(2)) || 
           testBit(xx2, sqr(0)) && testBit(xx3, sqr(1)) && testBit(xx1, sqr(2)) || 
           testBit(xx3, sqr(0)) && testBit(xx1, sqr(1)) && testBit(xx2, sqr(2)) || 
           testBit(xx3, sqr(0)) && testBit(xx2, sqr(1)) && testBit(xx1, sqr(2)) 
         ))
        return false
    }
    return true

    }

    m match {
      case 1 => cubes.filter(cube => validQube1(cube)).size
      case 2 =>
        var count = 0
        var i = 0
        while (i < cubes.size) {
          var j = i
          while (j < cubes.size) {
            if (validQube2(cubes(i), cubes(j))) count += 1

            j += 1
          }
          i += 1
        }
        count
      case 3 =>
        var count = 0
        var i = 0
        while (i < cubes.size) {
          var j = i
          while (j < cubes.size) {
            var k = j
            while (k < cubes.size) {
              if (validQube3(cubes(i), cubes(j), cubes(k))) count += 1
              k += 1
            }
            j += 1
          }
          i += 1
        }
        count
      case _ => 0
    }
  }

  def main(args: Array[String]) {
    val Array(n, m) = readLine.split(" ").map(_.toInt)
    println(bruteForce(n, m))
  }
}
