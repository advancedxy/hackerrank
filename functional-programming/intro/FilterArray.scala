def f(delim: Int,arr: List[Int]): List[Int] = {
    def filter[T](p: T => Boolean, arr: List[T]): List[T] = arr match {
      case Nil => Nil
      case head :: tail =>
        if (p(head)) head :: filter(p, tail) else filter(p, tail)
    }
    filter[Int](_ < delim, arr)
}
