object Solution {

  def numberOfDivisors(n: Int): Int = n match {
    case x if x <= 0 => 0
    case 1 => 1
    case x =>
      var i = 2
      while (i * i <= x && x % i != 0) {
        i += 1
      }
      if (i * i <= x) {
        var j = 1
        var num = n
        while (num % i == 0) {
          num /= i
          j += 1
        }
        j * numberOfDivisors(num)
      }
      else 2
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val n = readInt
      if (n % 2 == 0) println(numberOfDivisors(n / 2))
      else println(0)
    }
  }
}
