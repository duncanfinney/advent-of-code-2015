object Day3 extends App {

  val input = scala.io.Source.fromFile("src/input/Day3.txt", "utf8").mkString

  type Direction = Char

  case class Point(x: Int, y: Int) {
    def +(p: (Int, Int)) = Point(x + p._1, y + p._2)
  }

  def nextLocation(loc: Point, dir: Direction) = dir match {
    case '<' => loc +(-1, 0)
    case '>' => loc +(1, 0)
    case '^' => loc +(0, 1)
    case 'v' => loc +(0, -1)
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
    val initialState = PartOneState(Map(), Point(0, 0))
    val finalState = input.foldLeft(initialState)(doPartOneMove)
    finalState.visited.size
  }

  //part two
  object SantaType extends Enumeration {
    type SantaType = Value
    val Santa, RobotSanta = Value
  }

  import SantaType._
  case class PartTwoState(visited: Set[Point], santaLocation: Point, robotLocation: Point, nextMove: SantaType) {

    def doMove(direction: Direction): PartTwoState = if (nextMove == SantaType.Santa) {
      val newSantaLocation = nextLocation(santaLocation, direction)
      PartTwoState(visited + newSantaLocation, newSantaLocation, robotLocation, SantaType.RobotSanta)
    } else {
      val newRobotLocation = nextLocation(robotLocation, direction)
      PartTwoState(visited + newRobotLocation, santaLocation, newRobotLocation, SantaType.Santa)
    }

  }

  def partTwo = {
    val initialState = PartTwoState(Set(), Point(0,0), Point(0,0), SantaType.Santa)
    val finalState = input.foldLeft(initialState)((state, move) => state.doMove(move))
    finalState.visited.size
  }

  println("part1", partOne)
  println("part2", partTwo)
}
