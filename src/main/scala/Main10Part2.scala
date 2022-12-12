import Helpers._

object Main10Part2 {

  def main(args: Array[String]): Unit = {
    val lines = loadLines("input10.txt")
    println(lines)

    val measuresAt = List(20, 60, 100, 140, 180, 220)

    var res = Array.fill(6, 40)(".")

    val (endCycle, endRegisterValue) =
      lines.foldLeft((0, 1)) {
        case ((currentCycle, currentRegisterValue), instruction)
            if instruction.startsWith("addx ") =>
          val addxValue = instruction.substring(5).toInt
          val newCycle = currentCycle + 2

          for (subCycle <- currentCycle to newCycle) {
            val x = currentRegisterValue
            val pixelX = subCycle % 40
            val isInSprite =
              x - 1 <= pixelX && pixelX <= x + 1
            res(subCycle / 40)(pixelX) = if (isInSprite) "█" else " "
          }

          val newRegisterValue = currentRegisterValue + addxValue

//          println(
//            s"addx $addxValue: cycle $currentCycle -> $newCycle, register value $currentRegisterValue -> $newRegisterValue"
//          )

          (newCycle, newRegisterValue)
        case ((currentCycle, currentRegisterValue), "noop") =>
          val newCycle = currentCycle + 1

          val x = currentRegisterValue
          val pixelX = currentCycle % 40

          val isInSprite = x - 1 <= pixelX && pixelX <= x + 1
          res(currentCycle / 40)(pixelX) = if (isInSprite) "█" else " "

//          println(
//            s"noop: cycle $currentCycle -> $newCycle, register value $currentRegisterValue"
//          )
          (newCycle, currentRegisterValue)
      }

    println("endCycle: " + endCycle)
    println("endRegisterValue: " + endRegisterValue)

    res foreach { row =>
      println(row.mkString)
    }
  }
}
