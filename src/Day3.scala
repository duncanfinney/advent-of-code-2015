object Day3 extends App {

  val input = scala.io.Source.fromFile("src/input/Day3.txt", "utf8").mkString

  type Direction = Char

  case class Point(x: Int, y: Int) {
    def +(p :(Int,Int)) = Point(x + p._1, y + p._2)
  }

  def nextLocation(loc: Point, dir: Direction) = dir match {
    case '<' => loc + (-1, 0)
    case '>' => loc + (1, 0)
    case '^' => loc + (0, 1)
    case 'v' => loc + (0, -1)
    case _ => throw new Exception("Not found")
  }


  //part one
  case class PartOneState(visited: Map[Point, Int], currentLocation: Point)

  def doPartOneMove(currentState: PartOneState, direction: Char): PartOneState = {
    val PartOneState(visited, currentLocation) = currentState

    val newLocation = nextLocation(currentLocation, direction)
    val visitedCount = visited.getOrElse(newLocation, 0)

    PartOneState(visited + (newLocation -> (visitedCount + 1)), newLocation)
  }

  def partOne = {
    val initialState = PartOneState(Map(), Point(0,0))
    val finalState = input.foldLeft(initialState)(doPartOneMove)
    finalState.visited.size
  }

  //part two
  //TODO: Finish me


  println("part1", partOne)
}
