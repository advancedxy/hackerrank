object Solution {
  
  def ctOnlyDecrypt(ct: Array[Int], kSize: Int): String = {
    val splitCt = (0 until kSize).map(x => (x until ct.size by kSize).map(ct(_)))
    splitCt.map(x =>
                x.filter(c => c >= 'A' && c <= 'Z')
                .groupBy(identity).maxBy(_._2.size)._1).map(_ ^ ' ')
           .map(_.toChar).mkString
  }

  def main(args: Array[String]) {
    val _ = readLine
    val ct = readLine.split(" ").map(_.toInt)
    println(ctOnlyDecrypt(ct, 3))
  }
}
