object Day25 extends App {

  //part1
  def indexOf(row: Int, col: Int) = {
    val n = row + col - 1
    val topRight = n * (n+1) / 2
    topRight - row + 1
  }

  val indexOfAnswer = indexOf(3010, 3019)
  val part1 = Stream.iterate(BigInt(20151125))(_ * 252533 % 33554393).drop(indexOfAnswer - 1).head
  println("part1", part1)

}