import Helpers._



object Main2Part1 {
  sealed trait RPS

  case object Rock extends RPS

  case object Paper extends RPS

  case object Scissors extends RPS

  def main(args: Array[String]): Unit = {
    println(loadLines("input2.txt").map(line => line.split(" ").toList).map(l => (l(0), l(1))).map {
      case (op, me) => (parse(op), parse(me))
    }.map(turn => outcome(turn._1, turn._2) + score(turn._2)).sum)
  }

  def parse(raw: String): RPS = raw match {
    case "X" => Rock
    case "Y" => Paper
    case "Z" => Scissors
    case "A" => Rock
    case "B" => Paper
    case "C" => Scissors
  }

  def outcome(op: RPS, me: RPS): Int = (op, me) match {
    case (Rock, Rock) => 3
    case (Rock, Paper) => 6
    case (Rock, Scissors) => 0
    case (Paper, Rock) => 0
    case (Paper, Paper) => 3
    case (Paper, Scissors) => 6
    case (Scissors, Rock) => 6
    case (Scissors, Paper) => 0
    case (Scissors, Scissors) => 3
  }

  def score(rps: RPS): Int  = rps match {
    case Rock => 1
    case Paper => 2
    case Scissors => 3
  }
}