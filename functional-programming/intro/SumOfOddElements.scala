// don't deal with int overflow.
def f(arr: List[Int]): Int = arr.filter(_ % 2 != 0).sum
