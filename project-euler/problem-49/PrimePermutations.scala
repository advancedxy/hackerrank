object Solution {
  import scala.annotation.tailrec
  import scala.collection.mutable

  @tailrec
  def toDigits(x: Int, acc: List[Int]): List[Int] =
    if (x == 0) acc else toDigits(x / 10, x % 10 :: acc)

  @tailrec
  def toNum(ls: List[Int], acc: Int): Int = ls match {
    case h :: t => toNum(t, acc * 10 + h)
    case Nil => acc
  }

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = mutable.Map.empty[A, B]
    def apply(x: A) = cache getOrElseUpdate (x, f(x))
  }

  val primePermutationNumbers: Memo[List[Int], List[Int]] = Memo {
    case ls: List[Int] => ls.permutations.map(toNum(_, 0)).toList
  }

  def sievePrimeGenerator(n: Int): (Array[Int], Array[Boolean]) = {
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
    (primes.toArray, nums)
  }

  def isPermutations(m: Int, n: Int): Boolean = {
    if (m == n) return true
    val digits = Array.fill(10)(0)
    var temp = m
    while(temp > 0) {
      digits(temp % 10) += 1
      temp /= 10
    }
    temp =n
    while(temp > 0) {
      digits(temp % 10) -=1
      temp /= 10
    }
    for (d <- digits) {
      if (d != 0) return false
    }
    true
  }

  def genPrimePermutations(n: Int, k: Int): Seq[Seq[Int]] = {
    val digits = n.toString.size
    val limit = math.pow(10, digits min 6).toInt
    val (primes, primeTable) = sievePrimeGenerator(limit)
    val primesToBeSearched = primes.filter(x => x > 1000 && x < n)
    val result = Array.fill(2000)(List.empty[Int])
    val slots = (0 until k).toList
    var i = 0
    var j = 0
    while (i < primesToBeSearched.size) {
      val prime = primesToBeSearched(i)
      val candidates = primePermutationNumbers(toDigits(prime, Nil).sorted)
                      .filter(x => x > prime && primeTable(x))
      for (c <- candidates) {
        val diff = c - prime
        val primes = slots.map(prime + _ * diff)
        if (primes.drop(2).forall(x => x < limit && primeTable(x) &&
          isPermutations(prime, x))) {
            result(j) = primes
            j += 1
          }
      }
      i += 1
    }
    result.takeWhile(_.size > 0)
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    for (primes <- genPrimePermutations(n, k)) {
      println(primes mkString "")
    }
  }
}
