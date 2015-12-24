object Day24 extends App {

  val input = scala.io.Source.fromFile("src/input/Day24.txt").getLines.map(_.toInt).toList

  //find the num of digits in our solution
  def numPresents(compartmentSize: Int) : Int = (1 to input.length).find(x => input.slice(input.length - x, input.length).sum > compartmentSize).get + 1

  def minQuantumEntanglement(compartmentSize: Int) =
    (for {
      com <- input.combinations(numPresents(compartmentSize))
      if com.sum == compartmentSize
    } yield com)
      .map(ints => ints.map(BigInt(_)))
      .map(_.product)
      .min

  //part1
  val part1 = minQuantumEntanglement(input.sum / 3)
  println("part1", part1)

  val part2 = minQuantumEntanglement(input.sum / 4)
  println("part2", part2)

}