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
      // tree as indented string
      val filesStr =
        if (files.isEmpty) ""
        else files.map(_.toString).map("  " + _).mkString("\n") + "\n"
      val dirsStr =
        if (dirs.isEmpty) ""
        else
          dirs
            .map(_.toString.split("\n").map("  " + _).mkString("\n"))
            .mkString("\n")
      s"$name\n$filesStr$dirsStr"
    }
  }

  def totalFileSize(tree: Tree): Long = {
    tree match {
      case File(_, size) => size
      case Directory(_, files, dirs) =>
        files.map(_.size).sum + dirs.map(totalFileSize).sum
    }
  }

  def findDirectorySizes(directory: Directory): Map[String, Long] = {
    directory.dirs
      .map(findDirectorySizes)
      .flatten
      .toMap + (directory.name -> totalFileSize(directory))
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
      .drop(1)
    println(parsed)

    val builtTree = parsed
      .foldLeft(
        (Directory("/", List.empty, List.empty), List.empty[String])
      )((acc, next) => {
        val stepRes = (acc, next) match {
          case (acc, Ls) => acc
          case ((tree, currentPath), cd: Cd) if cd.path == ".." =>
            (tree, currentPath.dropRight(1))
          case ((tree, currentPath), cd: Cd) => (tree, currentPath :+ cd.path)
          case ((tree, currentPath), fileSizeOutput: FileSizeOutput) =>
            (updated(tree, currentPath, fileSizeOutput), currentPath)
          case ((tree, currentPath), dirOutput: DirOutput) =>
            (updated(tree, currentPath, dirOutput), currentPath)
        }
//        println(
//          s"at ${acc._2} with ${next} got path ${stepRes._2} and tree\n${stepRes._1}"
//          s"at ${acc._2} with ${next} got path ${stepRes._2}"
//        )
        stepRes
      })

    println("result:\n" + builtTree._1)

    val directorySizes = findDirectorySizes(builtTree._1)

    println("directory sizes:\n" + directorySizes)

    val smallDirs = directorySizes.filter(_._2 <= 100000)

    println("small dirs:\n" + smallDirs)
    println(smallDirs.values.sum)
  }

  def updated(
      directory: Directory,
      path: List[String],
      output: Output
  ): Directory = {
//    println(s"updating ${directory.name} with $output at $path")

    (path, output) match {
      case (Nil, _) =>
        output match {
          case FileSizeOutput(size, name) =>
            println(s"updating leaf with file $name")
            directory.copy(files = directory.files :+ File(name, size))
          case DirOutput(name) =>
            println(s"updating leaf with dir $name")
            directory.copy(dirs =
              directory.dirs :+ Directory(name, List.empty, List.empty)
            )
        }
      case (head :: tail, _) =>
        directory.dirs.find(_.name == head) match {
          case Some(dir) =>
            directory.copy(
              dirs = directory.dirs.map(d =>
                if (d.name == head) updated(d, tail, output) else d
              )
            )
          case None =>
            directory.copy(
              dirs = directory.dirs :+ updated(
                Directory(head, List.empty, List.empty),
                tail,
                output
              )
            )
        }
    }
  }
}
