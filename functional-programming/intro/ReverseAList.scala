def f(arr: List[Int]): List[Int] = {
  def reverseAux(arr: List[Int], acc: List[Int]): List[Int] = arr match {
    case Nil => acc
    case head :: tail => reverseAux(tail, head :: acc)
  }

  reverseAux(arr, Nil)
}
