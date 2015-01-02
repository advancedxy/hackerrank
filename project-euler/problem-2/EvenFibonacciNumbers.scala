object Solution {
  case class Memo[A, B](f: A => B) {
    import scala.collection.mutable.Map
    private val cache = Map.empty[A, B]
    def apply(x: A) = cache getOrElseUpdate (x, f(x))
  }
  
  val fib: Memo[Long, Long] = Memo {
    case 1l => 1l
    case 2l => 2l
    case n => fib(n - 1) + fib(n - 2)
  }
  
  def sumOfEvenFib(n: Long) = {
    var sum = 0l
    var i = 2
    while(fib(i) <= n) {
      if (fib(i) % 2 == 0) sum += fib(i)
      i += 1
    }
    sum
  }
  
  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution */
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toLong
      println(sumOfEvenFib(n))
    }
  }
}
