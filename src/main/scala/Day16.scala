object Day16 extends App {
  //  val input = scala.io.Source.fromFile("src/input/Day16.txt").mkString

  /**
    * @param traitName - children, cats, etc.
    * @return the number of them
    */
  def getTrait(traitName: String)(inputLine: String): Option[Int] = {
    val regex = s"$traitName: ([^,]*)".r
    regex.findFirstMatchIn(inputLine).flatMap(regexMatch => Some(regexMatch.group(1).toInt))
  }

  def passesTrait(traitName: String, num: Int)(inputLine: String): Boolean =
    getTrait(traitName)(inputLine).getOrElse(num) == num //None = still a possibility

  val part1 = scala.io.Source
    .fromFile("src/input/Day16.txt")
    .mkString
    .split("\n")
    .filter(passesTrait("children", 3))
    .filter(passesTrait("cats", 7))
    .filter(passesTrait("samoyeds", 2))
    .filter(passesTrait("pomeranians", 3))
    .filter(passesTrait("akitas", 0))
    .filter(passesTrait("vizslas", 0))
    .filter(passesTrait("goldfish", 5))
    .filter(passesTrait("trees", 3))
    .filter(passesTrait("cars", 2))
    .filter(passesTrait("perfumes", 1))
    .toList

  println("part1", part1)

  //part2

  def traitIsGreaterThan(traitName: String, num: Int)(inputLine: String) = getTrait(traitName)(inputLine) match {
    case Some(parsedNum) => parsedNum > num
    case None => true
  }

  def traitIsFewerThan(traitName: String, num: Int)(inputLine: String) = getTrait(traitName)(inputLine) match {
    case Some(parsedNum) => parsedNum < num
    case None => true
  }

  val part2 = scala.io.Source
    .fromFile("src/input/Day16.txt")
    .mkString
    .split("\n")
    .filter(passesTrait("children", 3))
    .filter(traitIsGreaterThan("cats", 7))
    .filter(passesTrait("samoyeds", 2))
    .filter(traitIsFewerThan("pomeranians", 3))
    .filter(passesTrait("akitas", 0))
    .filter(passesTrait("vizslas", 0))
    .filter(traitIsFewerThan("goldfish", 5))
    .filter(traitIsGreaterThan("trees", 3))
    .filter(passesTrait("cars", 2))
    .filter(passesTrait("perfumes", 1))
    .toList

  println("part2", part2)
}