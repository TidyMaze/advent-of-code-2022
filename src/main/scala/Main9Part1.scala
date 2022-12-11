import Helpers._

object Main9Part1 {

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

  def showVisited(_3: Set[(Int, Int)]): String = {
    val minX = _3.map(_._1).min
    val maxX = _3.map(_._1).max
    val minY = _3.map(_._2).min
    val maxY = _3.map(_._2).max

    val grid = (minY to maxY)
      .map { y =>
        (minX to maxX)
          .map { x =>
            if (_3.contains((x, y))) {
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

    val initialHeadCoord = (0, 0)
    val initialTailCoord = (0, 0)
    val initialTailVisited = Set(initialTailCoord)

    val finalState = instructions
      .flatMap { case (dir, steps) =>
        List.fill(steps)((dir, 1))
      }
      .foldLeft(
        (initialHeadCoord, initialTailCoord, initialTailVisited)
      ) {
        case ((headCoord, tailCoord, tailVisited), (direction, distance)) => {
          val newHeadCoord = direction match {
            case "U" => (headCoord._1, headCoord._2 - distance)
            case "D" => (headCoord._1, headCoord._2 + distance)
            case "L" => (headCoord._1 - distance, headCoord._2)
            case "R" => (headCoord._1 + distance, headCoord._2)
          }

          val neighbors = getNeighbors(tailCoord)

          // is too far (tail is not in neighbors of newHeadCoord)
          val isTooFar = newHeadCoord != tailCoord && !neighbors.contains(newHeadCoord)

          // find the coord around the tail in 8 directions that is closest to the new head coord
          val newTailCoord = if (isTooFar) {
            neighbors.minBy(n =>
              Math.abs(n._1 - newHeadCoord._1) + Math
                .abs(n._2 - newHeadCoord._2)
            )
          } else {
            tailCoord
          }

          val newTailVisited = tailVisited + newTailCoord

          println(
            s"After instruction $direction $distance, new head coord is $newHeadCoord, new tail coord is $newTailCoord, new tail visited is $newTailVisited"
          )
//          println(showVisited(newTailVisited))
          (newHeadCoord, newTailCoord, newTailVisited)
        }
      }
    println(finalState._3.size)

//    println(showVisited(finalState._3))
  }
}
