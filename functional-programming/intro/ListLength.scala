def f(arr: List[Int]): Int = {
  def lengthAux(arr: List[Int], acc: Int = 0): Int = arr match {
    case Nil => acc
    case head :: tail => lengthAux(tail, acc + 1)
  }

  lengthAux(arr)
}
