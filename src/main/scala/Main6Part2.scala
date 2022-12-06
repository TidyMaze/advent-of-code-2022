import Helpers._

object Main6Part2 {

  def main(args: Array[String]): Unit = {
    println(
      loadLines("input6.txt")
        .head
        .split("")
        .sliding(14, 1)
        .toList
        .zipWithIndex
        .find { case (seq, index) =>
          seq.distinct.length == 14
        }
        .map(_._2)
        .get + 14
    )
  }
}
