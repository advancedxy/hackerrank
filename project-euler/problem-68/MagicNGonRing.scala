object Solution {
  import scala.collection.mutable.Set

  def generateSolution(n: Int, total: Int): Seq[String] = {
    val result = Set.empty[Vector[Int]]
    val maxSum = 4 * n - 1
    val pairSum = Array.fill(maxSum + 1)(Set.empty[(Int, Int)])
    for {
      i <- 1 to 2 * n
      j <- i + 1 to 2 * n
    } pairSum(i + j).add((i, j))

    def search(arr: Vector[Int], idx: Int, visted: Set[Int]) {
      if (arr(0) == 0) { // initial state, try all the possible
        for {
          i <- 1 to n + 1 if total - i >= 0 && total - i <= maxSum
          (l, r) <- pairSum(total - i) if l != i && r != i
        } {
          val newVisted = visted + i + l + r
          val newArray = arr.updated(0, i)
          search(newArray.updated(n, l).updated(n + 1, r), 1, newVisted)
          search(newArray.updated(n, r).updated(n + 1, l), 1, newVisted)
        }
      } else {
        if (idx == n - 1) { // the final choice. Check if it can be a magic ring.
          val last = total - arr(2 * n - 1) - arr(n)
          if (!visted(last) && last >= arr(0) && last <= 2 * n)
            result.add(arr.updated(idx, last))
          else return // not a valid solution
        } else {
          val known = arr(n + idx)
          if (total - known < 0 || total - known > maxSum) return
          for {
            (l, r) <- pairSum(total - known)
            if !visted(l) && !visted(r) && r > arr(0)
          } {
            val newVisted = visted + l + r
            if (l > arr(0)) {
              val newArray = arr.updated(n + idx + 1, r).updated(idx, l)
              search(newArray, idx + 1, newVisted)
            }
            val newArray = arr.updated(n + idx + 1, l).updated(idx, r)
            search(newArray, idx + 1, newVisted)
          }
        }
      }
    }

    def vectorToString(arr: Vector[Int]): String = {
      (for (i <- 0 until n) yield "" + arr(i) + arr(n + i) + arr(n + (i + 1) % n)).mkString
    }

    search(Vector.fill(2 * n)(0), 0, Set.empty[Int])
    result.toList.map(vectorToString).sorted
  }

  def main(args: Array[String]) {
    val Array(n, total) = readLine.split(" ").map(_.toInt)
    println(generateSolution(n, total).mkString("\n"))
  }
}
