object Solution {
  import scala.collection.mutable.HashMap

  case class Memo[I <% K, K, O](f: I => O) extends (I => O) {
    val cache = HashMap.empty[K, O]
    def apply(x: I) = cache getOrElseUpdate (x, f(x))
  }

  type I = (Int, Int)
  type K = I
  type O = Int
  type Choices = Memo[I, K, O]
  val p = 1e9.toInt

  lazy val nCr: Choices = Memo {
    case (_, k) if k < 0 => 0
    case (_, 0) => 1
    case (n, k) if k > n / 2 => nCr(n, n - k)
    case (n, k) => (nCr(n - 1, k) + nCr(n - 1, k - 1)) % p
  }

  def choicesOfCandy(n: Int, k: Int) = nCr(n + k - 1, k)

  def main(args: Array[String]) {
    for (_ <- 1 to readLine.toInt) {
      val n = readInt
      val k = readInt
      println(choicesOfCandy(n: Int, k: Int))
    }
  }
}
