object Day13 extends App {

  val input = scala.io.Source.fromFile("src/input/Day13.txt").mkString.split("\n")


  val regex = raw"(.*) would (gain|lose) (.*) happiness units by sitting next to (.*).".r
  val parsedInput :Map[(String,String), Int] = input.map {
    x => {
      val regex(who, gainOrLose, pointsStr, sittingNextTo) = x
      val points =
        if (gainOrLose == "gain")
          pointsStr.toInt
        else
          -pointsStr.toInt
      (who, sittingNextTo) -> points
    }
  }
    .toMap

  val allUsers = input.map(_.split(" ")(0)).toSet.toList

  def totalHappiness(seatingArr: List[String]) = {
    val allPairs = seatingArr.sliding(2).toList ++ List(List(seatingArr.head, seatingArr.last))
    allPairs
      .map { case List(p1: String, p2: String) => parsedInput.getOrElse((p1, p2), 0) + parsedInput.getOrElse((p2, p1), 0) }
      .sum
  }

  val part1 = allUsers.permutations.toList.map(totalHappiness).max
  println("part1", part1)

  //partTwo

  val partTwoInput = parsedInput ++ allUsers.map(x => (x, "Duncan") -> 0) ++ allUsers.map(x => ("Duncan", x) -> 0)
  val part2 = (allUsers ++ List("Duncan")).permutations.toList.map(totalHappiness).max
  println("part2", part2)

}