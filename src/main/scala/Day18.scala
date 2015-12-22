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

    def add(point: (Int, Int), delta: (Int, Int)) = (point._1 + delta._1, point._2 + delta._2)

    List(
      add(point, (0, -1)),
      add(point, (1, -1)),
      add(point, (1, 0)),
      add(point, (1, 1)),
      add(point, (0, 1)),
      add(point, (-1, 1)),
      add(point, (-1, 0)),
      add(point, (-1, -1))
    ).filter(isValidPoint)
  }

  val PART_2_ALWAYS_ON_POINTS = List((0, 0), (99, 0), (99, 99), (0, 99))

  def shouldLightBeOn(currentState: Map[(Int, Int), Boolean], part2: Boolean)(point: (Int, Int)): Boolean = {

    if (part2 && PART_2_ALWAYS_ON_POINTS.contains(point)) {
      return true
    }

    val neighbors = getNeighbors(point)
    if (currentState(point)) {
      //handle on
      neighbors.count(currentState) == 3 || neighbors.count(currentState) == 2
    } else {
      //handle off
      neighbors.count(currentState) == 3
    }
  }

  def getNextState(currentState: Map[(Int, Int), Boolean], part2: Boolean = false): Map[(Int, Int), Boolean] =
    for (point <- currentState)
      yield point._1 -> shouldLightBeOn(currentState, part2)(point._1)


  //part1
  val part1 =
    (1 to 100)
      .foldLeft(initialState) { case (curr, _) => getNextState(curr) }
      .count { case (_, isOn) => isOn }

  println("part1", part1)

  //part2
  val part2 =
    (1 to 100)
      .foldLeft(initialState) { case (curr, _) => getNextState(curr, true) } //part2 = true
      .count { case (_, isOn) => isOn }

  println("part2", part2)
}