import Helpers._

object Main7Part1 {

  sealed trait History extends Product

  sealed trait Command extends History
  case class Cd(path: String) extends Command
  case object Ls extends Command

  sealed trait Output extends History
  case class FileSizeOutput(size: Long, name: String) extends Output
  case class DirOutput(name: String) extends Output

  sealed trait Tree

  case class File(name: String, size: Long) extends Tree {
    override def toString: String = {
      s"$name ($size)"
    }
  }

  case class Directory(name: String, files: List[File], dirs: List[Directory])
      extends Tree {
    override def toString: String = {
      val filesStr =
        files.map(f => s"${f.toString}")
      val dirsStr =
        dirs.map(d => s"${d.toString}")
      val mainStr = s"$name (dir count: ${dirs.size}, file count: ${files.size})"
      
        (mainStr :: filesStr ::: dirsStr).map(l => s"  $l").mkString("\n")
    }
  }

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
    val builtTree = parsed
      .foldLeft(
        (Directory("/", List.empty, List.empty), List.empty[String])
      ) {
        case (acc, Ls) => acc
        case ((tree, currentPath), cd: Cd) if cd.path == ".." =>
          (tree, currentPath.dropRight(1))
        case ((tree, currentPath), cd: Cd) => (tree, currentPath :+ cd.path)
        case ((tree, currentPath), fileSizeOutput: FileSizeOutput) =>
          (updated(tree, currentPath, fileSizeOutput), currentPath)
        case ((tree, currentPath), dirOutput: DirOutput) =>
          (updated(tree, currentPath, dirOutput), currentPath)
      }
    println(parsed)

    println(builtTree._1)
  }

  def updated(
      directory: Directory,
      path: List[String],
      output: Output
  ): Directory = {
    (path, output) match {
      case (Nil, _) =>
        output match {
          case FileSizeOutput(size, name) =>
            directory.copy(files = directory.files :+ File(name, size))
          case DirOutput(name) =>
            directory.copy(dirs =
              directory.dirs :+ Directory(name, List.empty, List.empty)
            )
        }
      case (head :: tail, _) if directory.dirs.exists(_.name == head) =>
        val newDirs = directory.dirs.map {
          case dir if dir.name == head => updated(dir, tail, output)
          case other                   => other
        }
        directory.copy(dirs = newDirs)
      case (head :: tail, _) =>
        val newDirs = directory.dirs :+ updated(
          Directory(head, List.empty, List.empty),
          tail,
          output
        )
        directory.copy(dirs = newDirs)
    }
  }
}
