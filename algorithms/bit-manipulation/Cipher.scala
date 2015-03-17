object Solution {

  def decode(s: String, n: Int, k: Int): Array[Int] = {
    val result = Array.fill(n)(0)
    var i = 1
    result(0) = s(0).asDigit
    while (i < k && i < n) {
      result(i) = s(i - 1).asDigit ^ s(i).asDigit
      i += 1
    }
    while (i < n) {
      result(i) = s(i - 1).asDigit ^ s(i).asDigit ^ result(i - k)
      i += 1
    }
    result
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val s = readLine
    println(decode(s, n, k).mkString)
  }
}
