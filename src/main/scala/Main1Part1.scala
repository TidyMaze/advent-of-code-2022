import Helpers._

import scala.io.Source

object Main1Part1 {
  def main(args: Array[String]): Unit = {
    println(splitWhen(loadLines("input1.txt"), (item: String) => item.isEmpty).map(_.map(_.toInt)).map(_.sum).max)
  }
}