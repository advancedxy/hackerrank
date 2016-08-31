import scala.annotation.tailrec
object Solution {

  val PQ_LIMIT = 1e7.toInt;
  val LIMIT = BigInt(Long.MaxValue);

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b.signum == 0) a else gcd(b, a.mod(b))

  def isLess(numerator: BigInt, denominator: BigInt,
             p: BigInt, q: BigInt): Boolean =
     numerator * numerator * q < denominator * denominator * p

  def isSquare(n: Long): Boolean = {
    if (n < 0 || (n & 2) != 0 || (n & 7) == 5 || (n & 11) == 8) return false
    if (n == 0) return true
    val x = math.sqrt(n).toLong
    x * x == n
  }

  def sievePrimeGenerator(n: Int): (Array[Int], Array[Boolean]) = {
    val primeTable = Array.fill(n + 1)(true)
    primeTable(0) = false
    primeTable(1) = false
    val primes = for (i <- (2 to n).toIterator if primeTable(i)) yield {
      var j = 2
      while (i * j <= n) {
        primeTable(i * j) = false
        j += 1
      }
      i
    }
    (primes.toArray, primeTable)
  }

  val (primes, primeTable) = sievePrimeGenerator(1e4.toInt)

  def primeFactorAndCounts(number: Int): Map[Int, Int] = {
    var num = number
    var result = Map.empty[Int, Int]
    var index = 0
    while (primes(index) <= num / primes(index) ) {
      var count = 0
      while (num % primes(index) == 0) {
        count += 1
        num /= primes(index)
      }
      if (count > 0) {
        result = result.updated(primes(index), count)
      }
      index += 1
    }
    if (num > 1) {
      result.updated(num, 1)
    } else result
  }

  // todo: speed up divisor generator
  def divisors(n: Int): Vector[Int] = {
    val primeMap = primeFactorAndCounts(n)
    primeMap.foldLeft(Vector(1)) { (x, y) =>
      val (p, c) = y
      var result: Vector[Int] = x
      for (i <- 1 to c) {
        result = result ++ x.map(_ * math.pow(p, i).toInt)
      }
      result
    }
  }

  def solve_with_limit(P: Int, Q: Int, D: Long): (BigInt, BigInt) = {
    if (isSquare(1L * P * Q)) return solvePQSquareCase(P, Q, D)
    val bigP = BigInt(P)
    val bigQ = BigInt(Q)
    val bigD = BigInt(D)

    var result = (BigInt(-1), BigInt(Long.MaxValue))

    for (diff <- divisors(Q - P)) {
      var leftNumerator = BigInt(0)
      var leftDenominator = BigInt(diff)
      var rightNumerator = BigInt(1)
      var rightDenominator = BigInt(1)

      var blueAndTotal = (BigInt(-1), BigInt(-1))
      while (leftDenominator <= LIMIT && rightDenominator <= LIMIT && blueAndTotal._2 <= bigD) {
        var nextNumerator = leftNumerator + rightNumerator
        var nextDenominator = leftDenominator + rightDenominator

        val less = isLess(nextNumerator, nextDenominator, bigP, bigQ)

        if (less) {
          // Speed up left update
          val leftStep = computeLeftStep(bigP, bigQ, leftNumerator, leftDenominator,
                                         rightNumerator, rightDenominator)
          nextNumerator = leftNumerator + rightNumerator * leftStep
          nextDenominator = leftDenominator + rightDenominator * leftStep
          leftNumerator = nextNumerator
          leftDenominator = nextDenominator
        } else {
          rightNumerator = nextNumerator
          rightDenominator = nextDenominator
        }

        var pairedNumerator = bigP * nextDenominator
        var pairedDenominator = bigQ * nextNumerator
        val common = gcd(pairedNumerator, pairedDenominator)
        pairedNumerator /= common
        pairedDenominator /= common

        blueAndTotal = if (less)
          findBlueAndTotal(nextNumerator, nextDenominator, pairedNumerator, pairedDenominator)
                       else
                         findBlueAndTotal(pairedNumerator, pairedDenominator, nextNumerator, nextDenominator)
      }
      if (blueAndTotal._1 >= 0 && blueAndTotal._2 <= result._2) {
        result = blueAndTotal
      }
    }
    result
  }

  def findBlueAndTotal(ln: BigInt, ld: BigInt, rn: BigInt, rd: BigInt): (BigInt, BigInt) = {
    val d = ld * rn - ln * rd
    if ((ln * (rd - rn)).mod(d).signum != 0 ||
          (ld * (rd - rn)).mod(d).signum != 0 ||
          (rn * (ld - ln)).mod(d).signum != 0 ||
          (rd * (ld - ln)).mod(d).signum != 0) (BigInt(-1), BigInt(-1))
    else
      ((rn * (ld - ln)) / d, (rd * (ld - ln))/ d)
  }

  def computeLeftStep(p: BigInt, q: BigInt, ln: BigInt, ld: BigInt, rn: BigInt, rd: BigInt): BigInt = {
    var lower = 1
    var upper = PQ_LIMIT
    var leftStep = 1

    while (lower <= upper) {
      val middle = (lower + upper) >>> 1
      val nn = ln + rn * middle
      val nd = ld + rd * middle

      if (isLess(nn, nd, p, q)) {
        leftStep = math.max(leftStep, middle)
        lower = middle + 1
      } else {
        upper = middle - 1
      }
    }
    BigInt(leftStep)
  }

  def solvePQSquareCase(P: Int, Q: Int, D: Long): (BigInt, BigInt) = {
    val W: Long = math.round(math.sqrt(1L * P * Q))
    val product: Long = 1L * Q * (Q - P)

    var smallFactor = 1L;
    var result = (BigInt(-1), BigInt(Long.MaxValue))
    while(smallFactor * smallFactor < product) {
      if (product % smallFactor == 0) {
        val bigFactor = product / smallFactor

        val Q2s = smallFactor + bigFactor
        val s = Q2s / (Q * 2)
        val W2t = bigFactor - smallFactor
        val t = W2t / (W * 2)

        val x = (s + 1) / 2
        val y = (t + 1) / 2
        if (Q2s % (Q * 2) != 0 || W2t % (W * 2) != 0 || s % 2 == 0 || t % 2 == 0) {

        } else if (y > D && y < result._2) {
          result = (BigInt(x), BigInt(y))
        }
      }
      smallFactor += 1
    }
    result
  }

  def main(args: Array[String]) = {
    val t = readInt
    for (i <- 1 to t) {
      val Array(p, q, limit) = readLine.split(" ").map(_.toLong)
      val g = gcd(p, q)
      val (pp, pq) = (p / g, q / g)
      val (blue, total) = solve_with_limit(pp.toInt, pq.toInt, limit)
      if (blue < 0) println("No solution") else println(s"$blue $total")
    }
  }
}
