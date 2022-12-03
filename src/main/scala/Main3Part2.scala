import Helpers._

object Main3Part2 {

  def main(args: Array[String]): Unit =
    println(
      loadLines("input3.txt")
        .grouped(3)
        .toList
        .map(group => group(0).toSet.intersect(group(1).toSet).intersect(group(2).toSet))
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
