object Solution {
  val factorialTable = ((1 to 9).foldLeft(List(1)) { (x, y) =>
    y * x.head :: x 
  }).reverse.toVector
  
  def canBeDigitalFactorials(x: Int): Boolean = {
    x.toString.map(d => factorialTable(d.asDigit)).sum % x == 0
  }

  def main(args: Array[String]) {
    val numbers = (10 to 1e5.toInt).filter(canBeDigitalFactorials)
    val n = readLine.toInt
    println(numbers.filter(_ < n).sum)
  }
}
