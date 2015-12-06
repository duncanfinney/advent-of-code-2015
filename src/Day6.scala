object Day6 extends App {

  case class Point(x: Int, y: Int)

  object Point {
    implicit def PairWrapper(p: (Int, Int)): Point = Point(p._1, p._2)
  }

  def range(p1: Point, p2: Point): List[Point] = {
    {
      for (
        x <- p1.x to p2.x;
        y <- p1.y to p2.y
      ) yield Point(x, y)
    }.toList
  }

  //types of moves
  sealed trait Move

  case class TurnOn(p1: Point, p2: Point) extends Move

  case class TurnOff(p1: Point, p2: Point) extends Move

  case class Toggle(p1: Point, p2: Point) extends Move


  val input = scala.io.Source.fromFile("src/input/Day6.txt", "utf8").mkString
  val inputPattern = "(.*) ([0-9]*),([0-9]*) through ([0-9]*),([0-9]*)".r
  val allMoves = input
    .split("\n")
    .map(_.trim)
    .map {
      line => {
        val inputPattern(cmd, fromX, fromY, toX, toY) = line
        val fromPt = (fromX.toInt, fromY.toInt)
        val toPt = (toX.toInt, toY.toInt)
        cmd match {
          case "turn on" => TurnOn(fromPt, toPt)
          case "turn off" => TurnOff(fromPt, toPt)
          case "toggle" => Toggle(fromPt, toPt)
        }
      }
    }

  type HouseState = Map[Point, Boolean]

  //part1
  def applyPartOneMove(currentState: HouseState, move: Move): HouseState = {
    move match {
      case TurnOn(p1, p2) => range(p1,p2).foldLeft(currentState)((state, point) => state updated (point, true))
      case TurnOff(p1, p2) => range(p1,p2).foldLeft(currentState)((state, point) => state updated (point, false))
      case Toggle(p1, p2) => range(p1,p2).foldLeft(currentState)((state, point) => state updated (point, !currentState.getOrElse(point, false)))
    }
  }

  val part1 = allMoves
    .foldLeft(Map[Point, Boolean]())(applyPartOneMove)
    .count{
      case (day, isOn) => isOn
    }

  println("part1", part1)


  //partTwo
  type HouseStatePartTwo = Map[Point, Int]
  def applyPartTwoMove(currentState: HouseStatePartTwo, move: Move) = move match {
    case TurnOn(p1, p2) => range(p1,p2).foldLeft(currentState)((state, point) => state updated (point, currentState.getOrElse(point, 0) + 1))
    case TurnOff(p1, p2) => range(p1,p2).foldLeft(currentState)((state, point) => state updated (point, Math.max(currentState.getOrElse(point, 0) - 1, 0)))
    case Toggle(p1, p2) => range(p1,p2).foldLeft(currentState)((state, point) => state updated (point, currentState.getOrElse(point, 0) + 2))

  }

  val part2 = allMoves
    .foldLeft(Map[Point, Int]())(applyPartTwoMove)
    .foldLeft(0){
      case (acc, (pt, brightness)) => acc + brightness
    }

  println("part2", part2)
}