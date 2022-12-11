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
    
    val biggestScore = gridScore.flatten.max
    println("biggestScore: " + biggestScore)
  }

  def computeScenicScore(grid: List[List[Int]], x: Int, y: Int) = {
    val currentTreeHeight = grid(y)(x)

    // compute view length from tree at (x, y), looking up
    val viewUp = (y - 1 to 0 by -1)
      .foldLeft((false, 0)) {
        case ((true, count), _) => (true, count)
        case ((false, count), cy) => {
          val current = grid(cy)(x)
          if (current < currentTreeHeight) {
            (false, count + 1)
          } else {
            (true, count + 1)
          }
        }
      }

    // compute view length from tree at (x, y), looking down
    val viewDown = (y + 1 until grid.size)
      .foldLeft((false, 0)) {
        case ((true, count), _) => (true, count)
        case ((false, count), cy) => {
          val current = grid(cy)(x)
          if (current < currentTreeHeight) {
            (false, count + 1)
          } else {
            (true, count + 1)
          }
        }
      }

    // compute view length from tree at (x, y), looking left
    val viewLeft = (x - 1 to 0 by -1)
      .foldLeft((false, 0)) {
        case ((true, count), _) => (true, count)
        case ((false, count), cx) => {
          val current = grid(y)(cx)
          if (current < currentTreeHeight) {
            (false, count + 1)
          } else {
            (true, count + 1)
          }
        }
      }

    // compute view length from tree at (x, y), looking right
    val viewRight = (x + 1 until grid.head.size)
      .foldLeft((false, 0)) {
        case ((true, count), _) => (true, count)
        case ((false, count), cx) => {
          val current = grid(y)(cx)
          if (current < currentTreeHeight) {
            (false, count + 1)
          } else {
            (true, count + 1)
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
