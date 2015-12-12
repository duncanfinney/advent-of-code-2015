package main.scala

object Day2 extends App {

  case class Box(l: Int, w: Int, h: Int) {

    val sideAreas = List(l * w, w * h, h * l)
    val sideDiameters = List(2 * l + 2 * w, 2 * w + 2 * h, 2 * h + 2 * l)

    def neededWrappingPaper = 2 * sideAreas.sum + sideAreas.min

    def neededRibbon = sideDiameters.min + (w * l * h)
  }

  assert(Box(2, 3, 4).neededWrappingPaper == 58)
  assert(Box(2, 3, 4).neededRibbon == 34)


  val input = scala.io.Source.fromFile("src/input/Day2.txt", "utf8").mkString

  val inputAsBoxes = input
    .split("\n")
    .map(line => {
      val Array(l, w, h) = line.split('x')
      Box(l.toInt, w.toInt, h.toInt)
    })

  val partOne = inputAsBoxes
    .map(_.neededWrappingPaper)
    .sum

  val partTwo = inputAsBoxes
    .map(_.neededRibbon)
    .sum

  println("part1", partOne)
  println("part2", partTwo)

}
