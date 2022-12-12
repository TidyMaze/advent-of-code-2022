import Helpers._

import scala.collection.mutable.ListBuffer

object Main10Part1 {

  def main(args: Array[String]): Unit = {
    val lines = loadLines("input10.txt")
    println(lines)

    val measuresAt = List(20, 60, 100, 140, 180, 220)

    val (endCycle, endTotalScore, endRegisterValue) =
      lines.foldLeft((0, 0, 1)) {
        case ((currentCycle, totalScore, currentRegisterValue), instruction)
            if instruction.startsWith("addx ") =>
          val addxValue = instruction.substring(5).toInt
          val newCycle = currentCycle + 2
          val containedMeasure: Option[Int] = findContainedMeasure(
            measuresAt,
            currentCycle,
            currentRegisterValue,
            newCycle
          )
          val newTotalScore = containedMeasure match {
            case Some(measure) => totalScore + currentRegisterValue * measure
            case None          => totalScore
          }

          val newRegisterValue = currentRegisterValue + addxValue

          println(
            s"addx $addxValue: cycle $currentCycle -> $newCycle, register value $currentRegisterValue -> $newRegisterValue, total score $totalScore -> $newTotalScore"
          )

          (newCycle, newTotalScore, newRegisterValue)
        case ((currentCycle, totalScore, currentRegisterValue), "noop") =>
          val newCycle = currentCycle + 1
          val containedMeasure: Option[Int] = findContainedMeasure(
            measuresAt,
            currentCycle,
            currentRegisterValue,
            newCycle
          )

          val newTotalScore = containedMeasure match {
            case Some(measure) => totalScore + currentRegisterValue * measure
            case None          => totalScore
          }

          println(
            s"noop: cycle $currentCycle -> $newCycle, register value $currentRegisterValue, total score $totalScore"
          )
          (newCycle, newTotalScore, currentRegisterValue)
      }

    println("endCycle: " + endCycle)
    println("endTotalScore: " + endTotalScore)
    println("endRegisterValue: " + endRegisterValue)
  }

  private def findContainedMeasure(
      measuresAt: List[Int],
      currentCycle: Int,
      currentRegisterValue: Int,
      newCycle: Int
  ): Option[Int] = {
    val containedMeasure =
      measuresAt.find(measure => currentCycle < measure && measure <= newCycle)
    containedMeasure match {
      case Some(value) =>
        println(
          s"=== During cycle $value, register has value $currentRegisterValue, signal strength is $value * $currentRegisterValue = ${value * currentRegisterValue} ==="
        )
      case None => ()
    }
    containedMeasure
  }
}
