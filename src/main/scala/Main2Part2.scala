import Helpers._

object Main2Part2 {

  sealed trait RPS

  case object Rock extends RPS

  case object Paper extends RPS

  case object Scissors extends RPS

  private sealed trait Outcome

  private case object Lose extends Outcome

  private case object Draw extends Outcome

  private case object Win extends Outcome

  def main(args: Array[String]): Unit = {
    println(
      loadLines("input2.txt")
        .map(line => line.split(" ").toList)
        .map(l => (l(0), l(1)))
        .map { case (op, me) =>
          (parse(op), parseOutcome(me))
        }
        .map(turn => {
          val action = actionToPlay(turn._1, turn._2)
          outcome(turn._1, action) + score(action)
        })
        .sum
    )
  }

  def parse(raw: String): RPS = raw match {
    case "A" => Rock
    case "B" => Paper
    case "C" => Scissors
  }

  private def parseOutcome(raw: String): Outcome = raw match {
    case "X" => Lose
    case "Y" => Draw
    case "Z" => Win
  }

  private def actionToPlay(op: RPS, outcome: Outcome) = {
    (op, outcome) match {
      case (Rock, Lose)     => Scissors
      case (Rock, Draw)     => Rock
      case (Rock, Win)      => Paper
      case (Paper, Lose)    => Rock
      case (Paper, Draw)    => Paper
      case (Paper, Win)     => Scissors
      case (Scissors, Lose) => Paper
      case (Scissors, Draw) => Scissors
      case (Scissors, Win)  => Rock
    }
  }

  def outcome(op: RPS, me: RPS): Int = (op, me) match {
    case (Rock, Rock)         => 3
    case (Rock, Paper)        => 6
    case (Rock, Scissors)     => 0
    case (Paper, Rock)        => 0
    case (Paper, Paper)       => 3
    case (Paper, Scissors)    => 6
    case (Scissors, Rock)     => 6
    case (Scissors, Paper)    => 0
    case (Scissors, Scissors) => 3
  }

  def score(rps: RPS): Int = rps match {
    case Rock     => 1
    case Paper    => 2
    case Scissors => 3
  }
}
