import Helpers._

object Main8Part2 {

  def main(args: Array[String]): Unit = {
    val grid = loadLines("input8.txt").map(_.split("").toList.map(_.toInt))
    println(grid)

    val height = grid.size
    val width = grid.head.size

    computeScenicScore(grid, 2, 3)
    
    println("")

    val gridScore = (0 until height).map { y =>
      (0 until width).map { x =>
        computeScenicScore(grid, x, y)
      }
    }

    println("gridScore: " + gridScore)
  }

  def computeScenicScore(grid: List[List[Int]], x: Int, y: Int) = {
    // compute view length from tree at (x, y), looking up
    val viewUp = (y to 0 by -1).foldLeft((-1, 0)) {
      case ((highest, viewLength), y) => {
        val current = grid(y)(x)
        if (current > highest) {
          (current, viewLength + 1)
        } else {
          (highest, viewLength)
        }
      }
    }

    // compute view length from tree at (x, y), looking down
    val viewDown = (y until grid.size).foldLeft((-1, 0)) {
      case ((highest, viewLength), y) => {
        val current = grid(y)(x)
        if (current > highest) {
          (current, viewLength + 1)
        } else {
          (highest, viewLength)
        }
      }
    }

    // compute view length from tree at (x, y), looking left
    val viewLeft = (x to 0 by -1).foldLeft((-1, 0)) {
      case ((highest, viewLength), x) => {
        val current = grid(y)(x)
        if (current > highest) {
          (current, viewLength + 1)
        } else {
          (highest, viewLength)
        }
      }
    }

    // compute view length from tree at (x, y), looking right
    val viewRight = (x until grid.head.size).foldLeft((-1, 0)) {
      case ((highest, viewLength), x) => {
        val current = grid(y)(x)
        if (current > highest) {
          (current, viewLength + 1)
        } else {
          (highest, viewLength)
        }
      }
    }

    val scenicScore = viewUp._2 * viewDown._2 * viewLeft._2 * viewRight._2
    println(
      s"scenicScore($x, $y): $scenicScore (viewUp: ${viewUp._2}, viewDown: ${viewDown._2}, viewLeft: ${viewLeft._2}, viewRight: ${viewRight._2})"
    )

    scenicScore
  }
}
