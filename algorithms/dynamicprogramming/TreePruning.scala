object Solution {
  import scala.annotation.tailrec
  import scala.collection.mutable.HashSet

  trait Node
  case object Empty extends Node
  sealed case class TreeNode(val weight: Int, val id: Int) extends Node {
    var parent: Node = Empty
    val children: HashSet[Int] = new HashSet
  }

  def pruneTree(nodes: IndexedSeq[TreeNode], k: Int): Long = {
    val n = nodes.size
    var dfsOrder: List[Int] = Nil
    val sizeOfSubTree = Array.fill(n)(1)

    def dfs(node: TreeNode) {
      dfsOrder = node.id :: dfsOrder
      for (child <- node.children) {
        dfs(nodes(child))
        sizeOfSubTree(node.id) += sizeOfSubTree(child)
      }
    }
    dfs(nodes(0))
    dfsOrder = dfsOrder.reverse

    var w = 0L
    val weights = (for (id <- dfsOrder) yield {
      w += nodes(id).weight
      w
    }).toArray
    for (_ <- 1 to k) {
      for (j <- n - 1 until 0 by -1) {
        val pos = j + sizeOfSubTree(dfsOrder(j)) - 1
        weights(pos) = math.max(weights(pos), weights(j - 1))
      }

      for (j <- 1 until n)
        weights(j) = math.max(weights(j), weights(j - 1) + nodes(dfsOrder(j)).weight)

    }

    weights(n - 1)
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val nodes = readLine.split(" ").map(_.toInt)
                        .zipWithIndex.map(x => TreeNode(x._1, x._2))

    for (_ <- 1 until n) {
      val Array(u, v) = readLine.split(" ").map(_.toInt - 1)
      val parent = u min v
      val child = u - parent + v
      nodes(child).parent = nodes(parent)
      nodes(parent).children.add(child)
    }
    println(pruneTree(nodes, k))
  }
}
