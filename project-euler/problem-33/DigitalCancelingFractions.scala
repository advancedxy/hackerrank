object Solution {
  import scala.annotation.tailrec

  def combinations[T](ls: List[T], k: Int): Stream[List[T]] = {
    if (k > ls.size) Stream()
    else ls match {
      case _ :: _ if k == 1 => ls.map(List(_)).toStream
      case hd :: tl =>
        combinations(tl, k - 1).map(hd :: _) #::: combinations(tl, k)
      case _ => Stream()
    }
  }

  val numberDigits = (0 to 1e4.toInt).map(x => x.toString.map(_.asDigit).toList)
  val digitNumbers = numberDigits.zipWithIndex.toMap

  def sievePrimeGenerator(n: Int): (List[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false; nums(1) = false
    val primes = for (i <- (2 to n).toList if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    (primes, nums)
  }

  
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def prepadTo(ls: List[Int], k: Int, n: Int): List[Int] = {
    if (ls.size < k) List.fill(k - ls.size)(n) ::: ls
    else ls
  }

  def genRN(denominator: Int, rd: Int, n: Int, k: Int): Seq[(Int, Int)] = {
    val sep = rd / gcd(denominator, rd)
    val nk = n - k
    val rdList = prepadTo(numberDigits(rd), nk, 0)
    val diff = numberDigits(denominator).diff(rdList).sorted
    if (diff.contains(0)) Seq.empty[(Int, Int)]
    else {
      //val rnStart = if (start % sep == 0) start / sep else start / sep + 1
      (sep until rd by sep).map({ rn =>
        (rn, denominator * rn / rd)}).filter {
          case (rn, numerator) =>
            val rnList = prepadTo(numberDigits(rn), nk, 0)
            numberDigits(numerator).diff(rnList).sorted == diff &&
            combinations(numberDigits(numerator), nk).contains(rnList)
        }
    }
  }

  @tailrec
  def digitsToNumbers(ls: List[Int]): Int = ls match {
    case Nil => 0
    case 0 :: tail => digitsToNumbers(tail)
    case _ => digitNumbers(ls)
  }

  def genFractions(n: Int, k: Int): Seq[(Int, Int)] = {
    val start = math.pow(10, n - 1).toInt
    val end = start * 10
    val rnStart = math.pow(10, n - k - 1).toInt
    val (_, primeTable) = sievePrimeGenerator(end)
    (for {
      denominator <- start + 1 until end if !primeTable(denominator)
      rd <- combinations(numberDigits(denominator), n - k) 
      if digitsToNumbers(rd) >= 1
      (rn, numerator) <- genRN(denominator, digitsToNumbers(rd), n, k)
    } yield {
      (numerator, denominator)
    }).distinct
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val fractions = genFractions(n, k)
    val (numerator, denominator) = fractions.foldLeft((0, 0)) { (x, y) =>
      (x._1 + y._1, x._2 + y._2)
    }

    println(numerator + " " + denominator)
  }
}

