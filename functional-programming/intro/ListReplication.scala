def f(num: Int, arr: List[Int]): List[Int] = arr.flatMap(List.fill(num)(_))
