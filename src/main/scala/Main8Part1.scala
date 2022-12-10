import Helpers._

import scala.collection.mutable

object Main8Part1 {

  def main(args: Array[String]): Unit = {
    val lines = loadLines("input8.txt").map(_.split("").toList.map(_.toInt))
    println(lines)

    val height = lines.size
    val width = lines.head.size

    val visibleAtEdgeCount = width * 2 + (height - 2) * 2

    val visibleInInteriorFromTop =
      (0 until width).flatMap { x =>
        (0 until height)
          .foldLeft((-1, Seq.empty[(Int, Int)])) {
            case ((highest, visibleTrees), y) => {
              val current = lines(y)(x)
              if (current > highest) {
                (current, visibleTrees :+ (x, y))
              } else {
                (highest, visibleTrees)
              }
            }
          }
          ._2
      }

    println("visibleInInteriorFromTop: " + visibleInInteriorFromTop)

    val visibleInInteriorFromBottom =
      (0 until width).flatMap { x =>
        (height - 1 to 0 by -1)
          .foldLeft((-1, Seq.empty[(Int, Int)])) {
            case ((highest, visibleTrees), y) => {
              val current = lines(y)(x)
              if (current > highest) {
                (current, visibleTrees :+ (x, y))
              } else {
                (highest, visibleTrees)
              }
            }
          }
          ._2
      }

    println("visibleInInteriorFromBottom: " + visibleInInteriorFromBottom)

    val visibleInInteriorFromLeft =
      (0 until height).flatMap { y =>
        (0 until width)
          .foldLeft((-1, Seq.empty[(Int, Int)])) {
            case ((highest, visibleTrees), x) => {
              val current = lines(y)(x)
              if (current > highest) {
                (current, visibleTrees :+ (x, y))
              } else {
                (highest, visibleTrees)
              }
            }
          }
          ._2
      }

    println("visibleInInteriorFromLeft: " + visibleInInteriorFromLeft)

    val visibleInInteriorFromRight =
      (0 until height).flatMap { y =>
        (width - 1 to 0 by -1)
          .foldLeft((-1, Seq.empty[(Int, Int)])) {
            case ((highest, visibleTrees), x) => {
              val current = lines(y)(x)
              if (current > highest) {
                (current, visibleTrees :+ (x, y))
              } else {
                (highest, visibleTrees)
              }
            }
          }
          ._2
      }

    println("visibleInInteriorFromRight: " + visibleInInteriorFromRight)
    
    val visibleTrees = (visibleInInteriorFromTop ++ visibleInInteriorFromBottom ++ visibleInInteriorFromLeft ++ visibleInInteriorFromRight).distinct.sorted
    
    println("visibleTrees: " + visibleTrees)
    
    val visibleCount = visibleTrees.size
    
    println("visibleCount: " + visibleCount)
  }
}
