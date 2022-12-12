import Helpers._

case class Monkey(
    id: Int,
    items: List[Int],
    operation: Int => Int,
    test: Int => Boolean,
    throwTo: (Int, Int)
)

object Main11Part1 {

  /** Monkey 0:
    * Starting items: 99, 67, 92, 61, 83, 64, 98
    * Operation: new = old * 17
    * Test: divisible by 3
    * If true: throw to monkey 4
    * If false: throw to monkey 2
    *
    * Monkey 1:
    * Starting items: 78, 74, 88, 89, 50
    * Operation: new = old * 11
    * Test: divisible by 5
    * If true: throw to monkey 3
    * If false: throw to monkey 5
    *
    * Monkey 2:
    * Starting items: 98, 91
    * Operation: new = old + 4
    * Test: divisible by 2
    * If true: throw to monkey 6
    * If false: throw to monkey 4
    *
    * Monkey 3:
    * Starting items: 59, 72, 94, 91, 79, 88, 94, 51
    * Operation: new = old * old
    * Test: divisible by 13
    * If true: throw to monkey 0
    * If false: throw to monkey 5
    *
    * Monkey 4:
    * Starting items: 95, 72, 78
    * Operation: new = old + 7
    * Test: divisible by 11
    * If true: throw to monkey 7
    * If false: throw to monkey 6
    *
    * Monkey 5:
    * Starting items: 76
    * Operation: new = old + 8
    * Test: divisible by 17
    * If true: throw to monkey 0
    * If false: throw to monkey 2
    *
    * Monkey 6:
    * Starting items: 69, 60, 53, 89, 71, 88
    * Operation: new = old + 5
    * Test: divisible by 19
    * If true: throw to monkey 7
    * If false: throw to monkey 1
    *
    * Monkey 7:
    * Starting items: 72, 54, 63, 80
    * Operation: new = old + 3
    * Test: divisible by 7
    * If true: throw to monkey 1
    * If false: throw to monkey 3
    */
  def main(args: Array[String]): Unit = {
    val lines = loadLines("input11.txt")
    println(lines)

    var monkeys = List(
      Monkey(0, List(99, 67, 92, 61, 83, 64, 98), _ * 17, _ % 3 == 0, (4, 2)),
      Monkey(1, List(78, 74, 88, 89, 50), _ * 11, _ % 5 == 0, (3, 5)),
      Monkey(2, List(98, 91), _ + 4, _ % 2 == 0, (6, 4)),
      Monkey(
        3,
        List(59, 72, 94, 91, 79, 88, 94, 51),
        x => x * x,
        _ % 13 == 0,
        (0, 5)
      ),
      Monkey(4, List(95, 72, 78), _ + 7, _ % 11 == 0, (7, 6)),
      Monkey(5, List(76), _ + 8, _ % 17 == 0, (0, 2)),
      Monkey(6, List(69, 60, 53, 89, 71, 88), _ + 5, _ % 19 == 0, (7, 1)),
      Monkey(7, List(72, 54, 63, 80), _ + 3, _ % 7 == 0, (1, 3))
    )

    var monkeysInspectedCount = List.fill(monkeys.size)(0)

    (1 to 20).foreach { roundNumber =>
      println("=== Round " + roundNumber + " ===")
      monkeys.indices.foreach { monkeyNumber =>
        val monkey = monkeys(monkeyNumber)
//        println(s"Current monkey is ${monkey.id} with items ${monkey.items}")
        monkey.items.foreach { item =>
          val worryLevel = monkey.operation(item) / 3

          monkeysInspectedCount = monkeysInspectedCount.updated(
            monkeyNumber,
            monkeysInspectedCount(monkeyNumber) + 1
          )

          val (throwToTrue, throwToFalse) = monkey.throwTo

          val destinationMonkeyId =
            if (monkey.test(worryLevel)) throwToTrue else throwToFalse
          val destinationMonkey = monkeys(destinationMonkeyId)

          val newMonkey = monkey.copy(items = monkeys(monkey.id).items.tail)
          val newDestinationMonkey =
            destinationMonkey.copy(items =
              destinationMonkey.items :+ worryLevel
            )

          monkeys = monkeys
            .updated(monkeyNumber, newMonkey)
            .updated(destinationMonkeyId, newDestinationMonkey)

          println(
            s"Round $roundNumber => Monkey ${monkey.id} inspected item $item and threw it to monkey ${destinationMonkey.id} with new worry level $worryLevel. New from monkey: ${newMonkey.items}, new to monkey: ${newDestinationMonkey.items}"
          )
        }
      }

      println("Monkeys after round " + roundNumber + ":")
      monkeys.foreach { monkey =>
        println(s"Monkey ${monkey.id}: ${monkey.items}")
      }
    }

    println("Monkeys inspected count: " + monkeysInspectedCount)

    val top2MonkeysScore =
      monkeysInspectedCount.zipWithIndex.sortBy(-_._1).take(2).map(_._1).product
    println("Top 2 monkeys score: " + top2MonkeysScore)
  }
}
