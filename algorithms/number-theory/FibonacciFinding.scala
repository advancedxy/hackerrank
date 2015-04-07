object Solution {

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.HashMap.empty[A, B]
    def apply(x: A): B = cache getOrElseUpdate (x, f(x))
  }

  // see http://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form
  // one thing should note: for f0, f1 as input, the nth fib is
  // f1 * f_01(n) + f0 * f_01(n -1) where f_01 denotes the normal
  // 0, 1 input fibonacci number sequences
  // this is a O(logn) algorithm, which is much better than the O(n) algorithm.
  def nthFib(a:Int, b:Int, n: Int): Long = {
    val p = 1e9.toInt + 7
    lazy val nthFibAux: Memo[Int, Int] = Memo {
      case 0 => 0
      case 1 => 1
      case n if n % 2 == 0 =>
        val x = nthFibAux(n / 2 - 1)
        val y = nthFibAux(n / 2)
        ((2L * x % p + y % p) * y % p).toInt
      case n if n % 2 == 1 =>
        val x = nthFibAux(n / 2 + 1)
        val y = nthFibAux(n / 2)
        ((1L * x * x % p + 1L * y * y % p) % p).toInt
    }

    (1L * b * nthFibAux(n) % p + 1L * a * nthFibAux(n - 1) % p) % p
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(a, b, n) = readLine.split(" ").map(_.toInt)
      println(nthFib(a, b, n))
    }
  }
}
