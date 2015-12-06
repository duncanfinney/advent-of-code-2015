object Day3 extends App {

  val input = scala.io.Source.fromFile("src/input/Day3.txt", "utf8").mkString


  def nextLocation(loc: (Int, Int), dir: Char) = dir match {
    case '<' => (loc._1 - 1, loc._2)
    case '>' => (loc._1 + 1, loc._2)
    case '^' => (loc._1, loc._2 + 1)
    case 'v' => (loc._1, loc._2 - 1)
    case _ => throw new Exception("Not found")
  }


  //part one
  case class PartOneState(visited: Map[(Int, Int), Int], currentLocation: (Int, Int))

  def doPartOneMove(currentState: PartOneState, direction: Char): PartOneState = {
    val PartOneState(visited, currentLocation) = currentState

    val newLocation = nextLocation(currentLocation, direction)
    val visitedCount = visited.getOrElse(newLocation, 0)

    PartOneState(visited + (newLocation -> (visitedCount + 1)), newLocation)
  }

  def partOne = {
    val initialState = PartOneState(Map(), (0,0))
    val finalState = input.foldLeft(initialState)(doPartOneMove)
    finalState.visited.size
  }

  //part two
  //TODO: Finish me


  println("part1", partOne)
}
