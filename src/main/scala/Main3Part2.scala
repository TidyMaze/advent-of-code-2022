import Helpers._

object Main3Part2 {

  private def intersectAll[A](sets: List[Set[A]]): Set[A] = {
    sets match {
      case Nil          => Set.empty
      case head :: Nil  => head
      case head :: tail => head.intersect(intersectAll(tail))
    }
  }

  def main(args: Array[String]): Unit =
    println(
      loadLines("input3.txt")
        .grouped(3)
        .toList
        .map(_.map(_.toSet))
        .map(intersectAll)
        .map(_.map(scoreItem).sum)
        .sum
    )

  /** To help prioritize item rearrangement, every item type can be converted to a priority:
    *
    * Lowercase item types a through z have priorities 1 through 26.
    * Uppercase item types A through Z have priorities 27 through 52.
    */
  def scoreItem(char: Char): Int = {
    if (char.isLower) {
      char.toInt - 96
    } else {
      char.toInt - 64 + 26
    }
  }
}
