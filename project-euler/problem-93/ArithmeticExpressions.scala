object Solution {
  import scala.annotation.tailrec
  import scala.collection.mutable

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = mutable.Map.empty[A, B]
    def apply(x: A) = cache getOrElseUpdate (x, f(x))
  }

  class Rational(val n: Int, val d: Int) {
    require(d != 0)
    @tailrec
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    private val g = gcd(math.abs(n), math.abs(d))
    val sn = if (d < 0) - n / g else n / g
    val sd = if (d < 0) - d / g else d / g

    def this(n: Int) = this(n, 1)

    def +(that: Rational): Rational =
      Rational(sn * that.sd + that.sn * sd, sd * that.sd)

    def -(that: Rational): Rational =
      Rational(sn * that.sd - that.sn * sd, sd * that.sd)

    def unary_- : Rational = Rational(-sn, sd)

    def *(that: Rational): Rational =
      Rational(sn * that.sn, sd * that.sd)

    def /(that: Rational): Rational =
      Rational(sn * that.sd, sd * that.sn)

    override def toString = s"$sn/$sd"

    override def hashCode = (sn, sd).hashCode

    override def equals(other: Any) = other match {
      case that: Rational =>
        this.sn == that.sn && this.sd == that.sd
      case _ => false
    }
  }

  object Rational {
    def apply(n: Int, d: Int) = new Rational(n, d)
    def applly(n: Int) = new Rational(n)
    def unapply(that: Rational): Option[(Int, Int)] = Some((that.sn, that.sd))
    implicit def int2Rational(i: Int) = new Rational(i)
  }

  def testBit(x: Int, y: Int): Boolean = ((x >>> y) & 1) != 0
  def extractDigitsFromNumber(x: Int): Seq[Int] = (0 to 9).filter(testBit(x, _))

  def foldDigits(digits: Seq[Int]): Int =
    digits.foldLeft(0) { (x, y) => x | (1 << y) }

  def generateRational(m: Rational, n: Rational) = (m, n) match {
    case (Rational(0, _), Rational(0, _)) => Set(m)
    case (Rational(0, _), _) => Set(m, n, -n)
    case (_, Rational(0, _)) => Set(n, m, -m)
    case (_, _) =>     Set(m + n, m * n, m / n, n / m, m - n, n - m)
  }

  // There are only 0-9 ten digits there, so I think we can use a number under
  // 1 << 10 to represent all the combinations.
  val arithmeticExp: Memo[Int, Set[Rational]] = Memo {
    case 0 => Set.empty[Rational]
    case x =>
      val digits = extractDigitsFromNumber(x)
      val size = digits.size
      if (size == 1) Set(Rational(digits.head, 1))
      else {
        for {
          i: Int <- (1 to size / 2).toSet
          selectDigits <- digits.combinations(i)
          val a = foldDigits(selectDigits)
          m <- arithmeticExp(a)
          n <- arithmeticExp(a ^ x)
          rational <- generateRational(m, n)
        } yield rational
      }
  }

  def main(args: Array[String]) {
    val n = readInt
    val digits = readLine.split(" ").map(_.toInt)
    val rationals = arithmeticExp(foldDigits(digits))
    val numbers = rationals.filter(x => x.sd == 1 && x.sn >= 0).map(_.sn)
    var r = 0
    while (numbers(r + 1)) { r += 1 }
    println(r)
  }
}
