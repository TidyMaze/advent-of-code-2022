import Helpers._

import scala.collection.mutable.ListBuffer

object Main9Part2 {

  // neighbors in 8 directions
  def getNeighbors(coord: (Int, Int)): List[(Int, Int)] = {
    val (x, y) = coord
    List(
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    )
  }

  def showVisited(
      tailVisited: Set[(Int, Int)],
      knots: List[(Int, Int)]
  ): String = {
    val minX = (tailVisited ++ knots).map(_._1).min
    val maxX = (tailVisited ++ knots).map(_._1).max
    val minY = (tailVisited ++ knots).map(_._2).min
    val maxY = (tailVisited ++ knots).map(_._2).max

    val grid = (minY to maxY)
      .map { y =>
        (minX to maxX)
          .map { x =>
            val knotsIndex = knots.indexOf((x, y))

            if (knotsIndex == 0) {
              "H"
            } else if (knotsIndex > 0) {
              knotsIndex.toString
            } else if (tailVisited.contains((x, y))) {
              "#"
            } else {
              "."
            }
          }
          .mkString("")
      }
      .mkString("\n")

    grid
  }

  def main(args: Array[String]): Unit = {
    val lines = loadLines("input9.txt")
    println(lines)

    val instructions = lines.map(_.split(" ")).map(e => (e(0), e(1).toInt))

    val KnotsNumber = 10

    val initialKnots = List.fill(10)((0, 0))
    val initialTailVisited = Set(initialKnots.last)
    val finalState = instructions
      .flatMap { case (dir, steps) => List.fill(steps)((dir, 1)) }
      .foldLeft((initialKnots, initialTailVisited)) {
        case ((knots, tailVisited), (direction, distance)) => {
          val headCoord = knots.head

          val newHeadCoord = direction match {
            case "U" => (headCoord._1, headCoord._2 - distance)
            case "D" => (headCoord._1, headCoord._2 + distance)
            case "L" => (headCoord._1 - distance, headCoord._2)
            case "R" => (headCoord._1 + distance, headCoord._2)
          }

          val newKnots = ListBuffer(knots: _*)
          newKnots(0) = newHeadCoord

          for (knotNumber <- 1 to KnotsNumber - 1) {
            val currentKnotCoord = newKnots(knotNumber)
            val neighborsCurrentKnot = getNeighbors(currentKnotCoord)

            val leaderKnotCoord = newKnots(knotNumber - 1)
            // is too far (leader is not in neighbors of follower)
            val isTooFar =
              leaderKnotCoord != currentKnotCoord && !neighborsCurrentKnot
                .contains(leaderKnotCoord)

            // find the coord around the follower knot in 8 directions that is closest to the new leader coord
            val newKnotCoord = if (isTooFar) {
              neighborsCurrentKnot.minBy(n =>
                Math.abs(n._1 - leaderKnotCoord._1) + Math
                  .abs(n._2 - leaderKnotCoord._2)
              )
            } else {
              currentKnotCoord
            }

            newKnots(knotNumber) = newKnotCoord
          }

          val newTailVisited = tailVisited + newKnots.last

          println(
            s"After instruction $direction $distance, new head coord is $newHeadCoord, new tail coord is ${newKnots.last}, new tail visited is $newTailVisited"
          )
          val newKnotsList = newKnots.toList
          println(showVisited(newTailVisited, newKnotsList))
          (newKnotsList, newTailVisited)
        }
      }
    println(finalState._2.size)

    //    println(showVisited(finalState._3))
  }
}
