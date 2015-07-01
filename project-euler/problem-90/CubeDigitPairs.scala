object Solution {

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
