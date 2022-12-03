import scala.io.Source

private object Helpers {
  def loadLines(str: String): List[String] = {
    val source = Source.fromResource(str)
    val lines = source.getLines().toList
    source.close()
    lines
  }


  /**
   * Returns a list of chunks separating by predicate function, ignoring items that match the predicate.
   * Ex: splitWhen(List(1,2,3,4,5,6,7,8,9,10), _ % 3 == 0) = List(List(1,2), List(4,5), List(7,8), List(10))
   */
  def splitWhen[A](list: List[A], predicate: A => Boolean): List[List[A]] =
    list.span(!predicate(_)) match {
      case (Nil, Nil) => Nil
      case (Nil, _) => splitWhen(list.tail, predicate)
      case (chunk, Nil) => List(chunk)
      case (chunk, _) => chunk :: splitWhen(list.drop(chunk.size + 1), predicate)
    }
}
