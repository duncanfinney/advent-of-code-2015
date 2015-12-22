import scala.annotation.tailrec

object Day19 extends App {
  val input = scala.io.Source.fromFile("src/input/Day19.txt").mkString.split("\n")
  val initialMolecule = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"

  val parseRegex = "(.*) => (.*)".r

  def getCombinationsForLine(inString: String, replacementInput: String): List[String] = {
    val parseRegex(in, out) = replacementInput
    val replaceRegex = in.r
    (for (m <- replaceRegex.findAllMatchIn(inString))
      yield inString.substring(0, m.start) ++ out ++ inString.substring(m.end, inString.length))
      .toList

  }

  //part1
  val part1 = input
    .flatMap(getCombinationsForLine(initialMolecule, _))
    .distinct
    .length
  println("part1", part1)


  //part2
  case class Replacement(from: String, to: String) {
    def getNeighbors(str: String): List[String] = {
      val replaceRegex = from.r
      (for (m <- replaceRegex.findAllMatchIn(str)) yield str.substring(0, m.start) ++ to ++ str.substring(m.end, str.length)).toList
    }
  }

  val inputReplacements = input.map(line => {
    val parseRegex(from, to) = line
    Replacement(from, to)
  })

  def getAllNeighbors(str: String): Stream[String] = {
    def neighborsOf(str: String) = inputReplacements.toStream.flatMap(_.getNeighbors(str))
    lazy val neighbors: Stream[String] = str #:: neighbors.flatMap(neighborsOf)
    neighbors
  }

  println(getAllNeighbors("e").find(_ == "HCaF"))

  //  println(from(10).take(10).toList)

}