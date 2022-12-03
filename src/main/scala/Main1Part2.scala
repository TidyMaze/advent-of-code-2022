import Helpers._

object Main1Part2 {
  def main(args: Array[String]): Unit = {
    println(splitWhen(loadLines("input1.txt"), (item: String) => item.isEmpty).map(_.map(_.toInt)).map(_.sum).sorted.reverse.take(3).sum)
  }
}