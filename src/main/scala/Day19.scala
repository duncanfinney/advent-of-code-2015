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

  val backgwardsReplacement = input.map(line => {
    val parseRegex(from, to) = line
    Replacement(to, from) //backwards replacement for part two
  })

  def getReverseNeighbors(str: String): Stream[(String, Int)] = {
    def neighborsOf(pair: (String, Int)) = {
        backgwardsReplacement
          .flatMap(_.getNeighbors(pair._1))
          .sortBy(_.length)
          .map(s => (s, pair._2 + 1))
          .toStream
    }

    lazy val neighbors: Stream[(String, Int)] = (str, 0) #:: neighbors.flatMap(neighborsOf)
    neighbors
  }

    getReverseNeighbors(initialMolecule).find(_._1 == "e")

}