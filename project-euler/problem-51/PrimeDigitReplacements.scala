object Solution {
  import scala.collection.immutable.BitSet

  def sievePrimeGenerator(n: Int): (Array[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false
    nums(1)= false
    val primes = for (i <- (2 to n).toArray if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    (primes, nums)
  }

  def getDigits(n: Int, b: BitSet) : BitSet =
    if (n == 0) b else getDigits(n / 10, b + (n % 10))

  // Generate "offsets": an offset is a base 10 "binary-literal" _mask_ in the form of 101, 1001, 11101, etc.
  // Given any number, we take its digits (Int) and map it the possible masks (Int) based on how it repeats
  // __For example:__ 120133 generates { 0 -> [00]1000, 1 -> 100100, 2 -> [0]10000, 3 -> [0000]11 },
  //    where [0...0] marks leading zeroes
  // By adding these masks, we can generate the solution by "replacing part of the number"
  // There are other cases th
  def getOffsets(x: Int): Map[Int, List[Int]] = {
    val digits = getDigits(x, BitSet())
    def inner(rem: Int, count: Int,
              offsets: Map[Int, List[Int]]) : Map[Int, List[Int]] = {
      val currentDigit = rem % 10
      rem match {
        case 0 => offsets                                                               // Terminate
        case _ if digits contains currentDigit =>
          val newValue = math.pow(10, count).toInt
          val oldList = offsets.getOrElse(currentDigit, List.empty[Int])
          inner(rem / 10, count + 1,                                                     // Update map
            offsets +  ((currentDigit, newValue :: oldList.map(_ + newValue) ::: oldList)))
      }
    }
    inner(x, 0, Map[Int, List[Int]]())
  }

  def numsToDigits(x: Int, acc: List[Int]): List[Int] =
    if (x == 0) acc else numsToDigits(x / 10, (if (x % 10 == 0) 0 else 1) :: acc)

  def getOffsetsWithKReplaceMents(x: Int, k: Int): List[(Int, Int)] =
    getOffsets(x).toList.flatMap {
      case (x, y) => y.filter(numsToDigits(_, Nil).sum == k).map((x, _))
    }
  
  def genPrimeReplaceSet(n: Int, k: Int, l: Int): Seq[Int] = {

    val primeStart = math.pow(10, n - 1).toInt
    val limit = primeStart * 10
    val (pp, primeTable) = sievePrimeGenerator(limit - 1)
    val primes = pp.toList.dropWhile(_ <= primeStart)
    def isPrime(x: Int) = primeTable(x)

    // Go through each mask for each prime, tackling three cases:
    // 1. Subtraction -- iteratively subtract the mask and check for primes
    // 2. Addition -- iteratively add the mask and check for primes
    // 3. The number itself -- is it prime?
    def search (plst: List[Int], l: Int) : List[(Int, Int, Int)] = {
      require(l > 1)
      plst match {
        case Nil => Nil
        case ph::pt => {
          // For the given prime, map over the offsets
          val candidates = getOffsetsWithKReplaceMents(ph, k).map {
            case (digit, offset) => {
              def oneIfPrime(n: Int) = if (n > primeStart && isPrime(n)) 1 else 0
              def primeCount (op: Int => Int, count: Int, num: Int, iter: Int, limit: Int) : Int = {
                val delta = offset * (iter + 1)
                val result = op(delta)

                if (iter >= limit) count
                else primeCount(op, count + oneIfPrime(result), result, iter + 1, limit)
              }
              if (primeCount(ph + _, 0, ph, 0, 9 - digit) + primeCount(ph - _, 0, ph, 0, digit - 1) + oneIfPrime(ph) >= l)
                Some((ph, digit, offset))
              else None
            }
          }

          // Search the results for our answer
          val result = candidates.filter(_.isDefined).toList
          if (result.isEmpty) search(pt, l) else result.map(_.get)
        }
      }
    }

    if (l == 1) primes.take(1)
    else {
      val candidates = search(primes, l)
      candidates.map({
        case (x, digit, offset) =>
          (0 to 9).map(d => x + (d - digit) * offset).filter({
            num => num > primeStart && num < limit && isPrime(num)
          }).take(l)
      }).minBy(x => x.toString)
    }
  }

  def main(args: Array[String]) {
    val Array(n, k, l) = readLine.split(" ").map(_.toInt)
    println(genPrimeReplaceSet(n, k, l).mkString(" "))
  }
}
