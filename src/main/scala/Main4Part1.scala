import Helpers._

object Main4Part1 {

  def main(args: Array[String]): Unit =
    println(
      loadLines("input4.txt")
        .map(line =>
          line.split(",").toList.map(_.split("-").toList.map(_.toInt))
        )
        .count(l => fullyOverlaps((l(0)(0), l(0)(1)), (l(1)(0), l(1)(1))))
    )

  def fullyOverlaps(rangeA: (Int, Int), rangeB: (Int, Int)): Boolean =
    rangeA._1 <= rangeB._1 && rangeA._2 >= rangeB._2 || rangeB._1 <= rangeA._1 && rangeB._2 >= rangeA._2
}
