object Solution {
  val upperBound = 1e6.toInt

  // see http://en.wikipedia.org/wiki/Circular_buffer and
  // http://stackoverflow.com/questions/8876769/best-practice-for-shifting-a-sequence-in-a-circular-manner
  // for more details
  class RingBuffer[A](val index: Int, val data: IndexedSeq[A]) extends IndexedSeq[A] {
    def shiftLeft = new RingBuffer((index + 1) % data.size, data)
    def shiftRight = new RingBuffer((index + data.size - 1) % data.size, data)
    def length = data.length
    def apply(i: Int) = data((index + i) % data.size)
  }

  def sievePrimeGenerator(n: Int): (Vector[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    val primes = for (i <- (2 to n).toIterator if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    (primes.toVector, nums)
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val bound = math.min(upperBound, math.pow(10, n.toString.length) - 1).toInt
    val (primes, primeTable) = sievePrimeGenerator(bound)
    val circularPrimeTable = Array.fill(bound + 1)(false)
    val circularPrimes = primes.filter { prime =>
      if (prime >= n) false
      else if (circularPrimeTable(prime)) true
      else {
        val digits = prime.toString.map(_.asDigit)
        val rotations = (0 until digits.length).map(new RingBuffer(_, digits)).map {
          x => x.reduceLeft { _ * 10 + _ }
        }
        if (rotations.forall(primeTable(_))) {
          rotations foreach { x => circularPrimeTable(x) = true }
          true
        }
        else false
      }
    }
    println(circularPrimes.sum)
  }
}
