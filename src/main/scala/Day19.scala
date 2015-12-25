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

  //  part1
  val part1 = input
    .flatMap(getCombinationsForLine(initialMolecule, _))
    .distinct
    .length
  println("part1", part1)


  //part2
  case class Replacement(from: String, to: String) {

    def inverse = Replacement(to, from)

    def applyTo(s: String) = {
      (for (m <- from.r.findAllMatchIn(s))
        yield s.substring(0, m.start) ++ to ++ s.substring(m.end, s.length)).toList
    }

  }

  val replacements = input.map(_.split("=>")).map { case Array(lhs, rhs) => Replacement(lhs.trim, rhs.trim) }.toList
  val inverseReplacements = replacements.map(_.inverse)

  case class Node(str: String, distance: Int = 0) {
    lazy val neighbors: Set[Node] =
      inverseReplacements
        .flatMap(_.applyTo(str))
        .sortBy(_.length)
        .map(Node(_, distance + 1))
        .toSet
  }


  def findNode(str: String, startNode: Node): Option[Node] = {

    @tailrec
    def findNodeInner(currNode: Node, visitedNodes: Set[Node]): Option[Node] = {
      val allNeighbors = visitedNodes.flatMap(_.neighbors).filter(!visitedNodes.contains(_)).toList
      if (currNode.str == str) Some(currNode)
      else if (allNeighbors.isEmpty) None
      else {
        allNeighbors.sortBy(_.str.length).headOption match {
          case Some(n) => findNodeInner(n, visitedNodes + currNode)
          case None => None
        }
      }
    }

    findNodeInner(startNode, Set(startNode))
  }

  val startNode = Node(initialMolecule)
  println("part2", findNode("e", startNode))

}