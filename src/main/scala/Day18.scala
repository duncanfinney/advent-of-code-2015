object Day18 extends App {

  case class LightState(state: Map[(Int, Int), Boolean])

  val input = scala.io.Source.fromFile("src/input/Day18.txt").getLines.toList

  def isLightOn(c: Char) = c == '#'

  val initialState: Map[(Int, Int), Boolean] = (for {
    y <- input.indices
    x <- 0 to input(y).length - 1
  } yield (x, y) -> isLightOn(input(x).charAt(y))).toMap


  def getNeighbors(point: (Int, Int)): List[(Int, Int)] = {

    def isValidPoint(point: (Int, Int)) = point match {
      case (x, y) => x >= 0 && x <= 99 && y >= 0 && y <= 99
    }

    point match {
      case (x, y) => List(
        (x, y - 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1),
        (x, y + 1),
        (x - 1, y + 1),
        (x - 1, y),
        (x - 1, y + 1)
      ).filter(isValidPoint)
    }
  }

  def shouldLightBeOn(currentState: Map[(Int, Int), Boolean])(point: (Int, Int)): Boolean = {
    val neighbors = getNeighbors(point)
    if (currentState(point)) {
      //handle on
      neighbors.count(currentState) == 3 || neighbors.count(currentState) == 2
    } else {
      //handle off
      neighbors.count(currentState) == 3
    }
  }

  def getNextState(currentState: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] =
    for (point <- currentState)
      yield point._1 -> shouldLightBeOn(currentState)(point._1)


  //part1
  val part1 = 
    (1 to 100)
    .foldLeft(initialState){ case (curr, _) => getNextState(curr) }
    .count{ case (point, isOn) => isOn }
  
  println("part1", part1)
}