object Solution {

  def generateToientNumbers(x: Int): Array[Long] = {
    val totients = (0 to x).toArray
    totients(1) = 0
    for {
      i <- 2 to x if totients(i) == i
      j <- i to x by i
    } totients(j) = totients(j) / i * (i - 1)
    var sum = 0L
    val result = Array.fill(x + 1)(0L)
    for (i <- 2 to x) {
      sum += totients(i)
      result(i) = sum
    }
    result
  }

  def main(args: Array[String]) {
    val t = readInt
    val cases = (1 to t).map(x => readInt)
    val totientNumbers = generateToientNumbers(cases.max)
    cases.foreach(x => println(totientNumbers(x)))
  }
}
