object Solution {

  // perfect digit invariant
  // A PDI is a number equals to the sum of power p of its digits
  def isPDI(n: Int, p: Int): Boolean = {
    var num = n
    var powerSum = 0
    while (num != 0) {
      powerSum += math.pow(num % 10, p).toInt
      num /= 10
    }
    powerSum == n
  }

  def main(args: Array[String]) {
    val p = readLine.toInt
    val limit = (p + 1) * math.pow(9, p).toInt
    val PDIs = for (i <- 2 to limit if isPDI(i, p)) yield i
    println(PDIs.foldLeft(0l) { _ + _ })
  }
}
