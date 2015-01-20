object Solution {

  // This function is used to generate the last digits for every complement 
  // in m * n = p
  def genTails(n: Int) = {
    for { i <- 1 to n
          j <- 1 to n if j != i
          k <- j + 1 to n if k != i && (k * j - i) % 10 == 0} yield (i, j, k)
  }
  
  def genPandigitalProduct(n: Int, tails: (Int, Int, Int)): Vector[Int] = {
    val set = (1 to n).toSet
    val (a, b, c) = tails

    // The number of product digits will be n / 2, and we already have 1 digit
    // choosen.
    val pSize = n / 2 - 1
    val choices = set - a - b -c
    val result = scala.collection.mutable.Set.empty[Int]
    var pitr = choices.toVector.permutations
    while(pitr.hasNext) {
      val permutation = pitr.next
      val (p, remain) = permutation.splitAt(pSize)
      val product = (p.foldLeft(0) { case (x, y) => x * 10 + y }) * 10 + a
      val hasProduct =
        (0 to math.min(pSize, n - 3 -pSize)).map(remain.splitAt(_)).map {
          case (x, y) =>
            List(x :+ b, y :+ c).map(z => z.reduceLeft { 10 * _ + _ }).product
        } exists { _ == product }
      if (hasProduct) result.add(product)
    }
    result.toVector
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val tails = genTails(n)
    val sum = tails.flatMap(genPandigitalProduct(n, _)).toSet.sum
    println(sum)
  }
}
