import java.util.Arrays

object Solution extends App {
  val pow10 = Array(1, 10, 100, 1000, 10000, 100000, 1000000)
  val key = (0 until pow10.last).map(_.toString.sorted)
  val input = readLine().split(" ").map(_.toInt)
  val N = input(0)
  val K = input(1)
  val maskArr = (0 until (1 << N)).filter(i => {
    def count(x: Int): Int = if (x == 0) 0
                             else if (x % 2 == 1) 1 + count(x / 2)
                             else count(x / 2)
    count(i) == K
  })
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def canRemove(a: Int, mask: Int): Boolean = {
    var aa = a
    for (i <- 0 until N) {
      if (((mask & (1 << i)) > 0) && aa % 10 == 0) return false
      aa /= 10
    }
    return true
  }

  def remove(a: Int, mask: Int) = {
    var res = 0
    var drop = 0
    var aa = a
    var resPtr = 0
    for (i <- 0 until N) {
      if ((mask & (1 << i)) > 0) {
        drop = drop * 10 + aa % 10
      } else {
        res += aa % 10 * pow10(resPtr)
        resPtr += 1
      }
      aa /= 10
    }
    (res, drop)
  }

  class Fraction(n: Int, d: Int) {
    require(d != 0)
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(n, d)
    val a = n / g
    val b = d / g
    override def equals(that: Any) = that match {
      case r: Fraction => a.toLong * r.b == b.toLong * r.a
      case _           => false
    }
    override def hashCode = a * 31 + b
    override def toString = a + "/" + b
  }
  object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d)
  }

  val cb = for {
    b <- pow10(N - 1) until pow10(N)
    mask <- maskArr
    if (canRemove(b, mask))
    (bb, drop) = remove(b, mask)
    if (bb > 0)
  } yield (Fraction(bb, b), key(drop), b)

  val ratioGroup = cb.groupBy(x => x._1)
  val candList = ratioGroup.map(x => x._2.groupBy(_._2))
  val res = for {
    mapList <- candList
    list <- mapList.values
    sortedList = list.unzip3._3.distinct.toArray
    i <- 0 until sortedList.length
    j <- i + 1 until sortedList.length
  } yield
  if (sortedList(i) < sortedList(j)) (sortedList(i), sortedList(j))
  else (sortedList(j), sortedList(i))

  val (a, b) = res.toSet.toArray.unzip
  println(a.map(_.toLong).sum + " " + b.map(_.toLong).sum)
}
