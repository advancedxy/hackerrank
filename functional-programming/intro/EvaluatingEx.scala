def f(x: Double): Double= {
  ((1 to 9).foldLeft(List(1D)) { (ls, index) =>
    ls.head * x / index :: ls
  }).sum
}
