object Solution {

  def sievePrimeGenerator(n: Int): (Seq[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false
    nums(1) = false
    val primes = for (i <- (2 to n) if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    (primes, nums)
  }

  def isPerm(x: Int, y: Int): Boolean = {
    val occ = Array.fill(10)(0)
    var xx = x
    while (xx > 0) {
      occ(xx % 10) += 1
      xx /= 10
    }

    var yy = y
    while (yy > 0) {
      occ(yy % 10) -= 1
      yy /= 10
    }

    occ.forall(_ == 0)
  }

  def bruteForceTotient(n: Int): Int = {
    val totients = Array.fill(n + 1)(0)
    val resultSet = scala.collection.mutable.LinkedHashSet.empty[(Int, Int)]
    totients(1) = 1
    val (primes, primeTable) = sievePrimeGenerator(n)
    // prime
    for (p <- primes) totients(p) = p - 1
    // prime power
    for (p <- primes.takeWhile(_ <= math.sqrt(n).toInt)) {
      var pPower = p
      while (1L * pPower * p <= n) {
        totients(pPower * p) = pPower * totients(p)
        pPower *= p
      }
    }

    // fill other numbers
    var i = 0
    while (i < primes.length) {
      val p = primes(i)
      var j = p + 1
      while (1L * p * j <= n) {
        val num = p * j
        if (totients(num) == 0) {
          if (j % p != 0)
            totients(num) = totients(p) * totients(j)
          else {
            var jj = j
            var pp = p
            while (jj % p == 0) {
              jj /= p
              pp *= p
            }
            totients(num) = totients(pp) * totients(jj)
          }
          if (isPerm(num, totients(num)))
            resultSet.add((num, totients(num)))
        }
        j += 1
      }
      i += 1
    }
    assert(totients.tail.forall(x => x > 0))
    resultSet.filter(x => x._1 < n).toVector
             .minBy(x => x._1.toDouble / x._2)._1
  }

  def minPermutationTotient(n: Int): Int = {
    val (primes, _) = sievePrimeGenerator(n / 2)

    var i = 0
    var minRatio = Double.MaxValue
    var result = 0
    val sqrtn = math.sqrt(n).toInt
    while (i < primes.length && primes(i) <= sqrtn) {
      var j = i + 1
      val pi = primes(i)
      while (j < primes.length && primes(j) * pi < n) {
        val pj = primes(j)
        val num = pi * pj
        val totient = num - pi - pj + 1
        if (isPerm(num, totient) && num.toDouble / totient <= minRatio) {
          result = num
          minRatio = num.toDouble / totient
        }
        j += 1
      }
      i += 1
    }
    result
  }

  def main(args: Array[String]) {
    val n = readInt
    println(bruteForceTotient(n))
  }
}
