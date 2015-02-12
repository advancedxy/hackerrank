def f(coefficients: List[Int], powers: List[Int], x: Double): Double =
  coefficients.zip(powers).map(y => y._1 * math.pow(x, y._2)).sum

def area(coefficients: List[Int], powers: List[Int], x: Double): Double =
  math.Pi * math.pow(f(coefficients, powers, x), 2)

def summation(func: (List[Int],List[Int],Double) => Double,
             upperLimit: Int,
             lowerLimit: Int,
             coefficients: List[Int],
             powers:List[Int]): Double = {
  val sep = 0.001D
  val (start, end) = (lowerLimit.toDouble, upperLimit.toDouble)
  (start to end by sep).map(func(coefficients, powers, _) * sep).sum
}
