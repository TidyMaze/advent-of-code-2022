import Helpers._

object Main5Part1 {

  def main(args: Array[String]): Unit = {
    val (stacksRaw, other) = loadLines("input5.txt").splitAt(8)
    val stacks = stacksRaw
      .map(line =>
        line
          .grouped(4)
          .map {
            case e if e.startsWith("[") && e.length == 3 =>
              Some(e.drop(1).dropRight(1))
            case e if e.startsWith("[") => Some(e.drop(1).dropRight(2))
            case _                      => None
          }
          .toList
      )
      .transpose
      .map(_.collect { case Some(e) => e })
      .map(_.reverse)
    val actions =
      other
        .drop(2)
        .map(_.split(" ").toList)
        .map(l => (l(1).toInt, l(3).toInt, l(5).toInt))

    println(stacks, actions)

    val res = actions.foldLeft(stacks)((acc, curr) => move(acc, curr))

    println(res.map(_.last).mkString(""))
  }

  def move(
      stacks: List[List[String]],
      action: (Int, Int, Int)
  ): List[List[String]] = action match {
    case (count, from, to) => {
      val (fromStack, toStack) = (stacks(from - 1), stacks(to - 1))
      val (fromStackNew, toStackNew) = (fromStack.dropRight(count), toStack ++ fromStack.takeRight(count).reverse)
      val res = stacks.updated(from - 1, fromStackNew).updated(to - 1, toStackNew)
      println(res)
      res
    }
  }
}
