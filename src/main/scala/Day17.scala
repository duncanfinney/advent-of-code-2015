object Day17 extends App {

  val containers = scala.io.Source.fromFile("src/input/Day17.txt").getLines.toList.map(c => c.toInt).sorted

  def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] = {
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }
  }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {
        sl.head :: _
      }
    }
  }

  val part1 = (1 to containers.size).toList.map(combinations(_, containers).count(_.sum == 150)).sum
  println("part1", part1)

  val part2 = (1 to containers.size).toList.find(combinations(_, containers).count(_.sum == 150) > 0)
  println("part2", part2)

}