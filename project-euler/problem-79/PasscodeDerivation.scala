object Solution {
  import scala.collection.mutable.{HashSet, PriorityQueue}

  // It's a topological sort with lexicographically.
  // See http://en.wikipedia.org/wiki/Topological_sorting
  def solve(xs: Seq[String]): String = {
    val ord = new Ordering[Int] { def compare(x: Int, y: Int) = y.compare(x) }
    val minQueue = PriorityQueue[Int]()(ord)
    val previous = Array.fill(128)(HashSet.empty[Int])
    val next = Array.fill(128)(HashSet.empty[Int])
    val chars = HashSet.empty[Int]
    for (x <- xs) {
      for (c <- x) chars.add(c)

      x.zip(x.tail).foreach { case (x, y) =>
        next(x).add(y)
        previous(y).add(x)
      }
    }
    minQueue ++= chars.filter(x => previous(x).size == 0)
    var result = List.empty[Char]
    while (minQueue.size > 0) {
      val node = minQueue.dequeue
      result = node.toChar :: result
      for (i <- next(node)) {
        previous(i).remove(node)
        if (previous(i).size == 0) minQueue.enqueue(i)
      }
    }
    val s = result.mkString
    if (s.size == chars.size) s.reverse else "SMTH WRONG"
  }

  def main(args: Array[String]) {
    val t = readInt
    val logins = (1 to t).map(x => readLine)
    println(solve(logins))
  }
}
