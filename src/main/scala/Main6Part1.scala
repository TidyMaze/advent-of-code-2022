import Helpers._

object Main6Part1 {

  def main(args: Array[String]): Unit = {
    println(
      loadLines("input6.txt")
        .head
        .split("")
        .sliding(4, 1)
        .toList
        .zipWithIndex
        .find { case (seq, index) =>
          seq.distinct.length == 4
        }
        .map(_._2)
        .get + 4
    )
  }
}
