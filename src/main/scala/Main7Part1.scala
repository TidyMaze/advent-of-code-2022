import Helpers._

import scala.collection.mutable

object Main7Part1 {

  sealed trait History extends Product

  sealed trait Command extends History
  case class Cd(path: String) extends Command
  case object Ls extends Command

  sealed trait Output extends History
  case class FileSizeOutput(size: Long, name: String) extends Output
  case class DirOutput(name: String) extends Output

  def main(args: Array[String]): Unit = {
    val lines = loadLines("input7.txt")
    val parsed = lines
      .map {
        case str if str.startsWith("$ cd ") => Cd(str.drop(5))
        case str if str == "$ ls"           => Ls
        case str if str.startsWith("dir ")  => DirOutput(str.drop(4))
        case other =>
          FileSizeOutput(
            other.takeWhile(_.isDigit).toLong,
            other.dropWhile(_.isDigit).drop(1)
          )
      }
    println(parsed)
    val allSizes = findAllDirectorySizes(parsed)
    println(allSizes)

    val smallSizes = allSizes.filter(_._2 <= 100000)
    println(smallSizes.toList.sortBy(_._2).reverse)

    val smallSizesSum = smallSizes.map(_._2).sum
    println(smallSizesSum)

    // not 1075948
  }

  /** Find all directory sizes from history, a file contributes to the size of all its parent directories. */
  def findAllDirectorySizes(history: List[History]): Map[String, Long] = {
    val directorySizes = mutable.Map.empty[String, Long]
    val currentDirectory = mutable.ListBuffer.empty[String]

    history.foreach {
      case Cd("..") =>
        currentDirectory.remove(currentDirectory.size - 1)
      case Cd(path) =>
        currentDirectory += path
      case Ls =>
      // do nothing
      case FileSizeOutput(size, name) =>
        // list of all parent directory paths (ex: List("a", "b", "c") -> List("a", "a/b", "a/b/c"))
        val allParentDirectoriesPaths = currentDirectory
          .scanLeft("")((acc, next) =>
            acc match {
              case ""  => next
              case "/" => s"/$next"
              case _   => acc + "/" + next
            }
          )
        allParentDirectoriesPaths.foreach { dir =>
          directorySizes(dir) = directorySizes.getOrElse(dir, 0L) + size
        }
      case DirOutput(name) =>
      // do nothing
    }

    directorySizes.toMap
  }
}
