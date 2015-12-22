object Day20 extends App {

  val (max1, numPresents1) = (800000, 34000000 / 10)
  val part1 =
    (for {
      num <- 1 to max1
      i <- num to max1 by num
    } yield {
      (i, num)
    }).groupBy(_._1).toList.sortBy(_._1).find(_._2.map(_._2).sum >= numPresents1).map(_._1)

  println("part1", part1)


  val (max2, numPresents2) = (1000000, 34000000 / 11)
  val part2 =
    (for {
      num <- 1 to max2
      i <- num to max2 by num take 50
    } yield {
      (i, num)
    }).groupBy(_._1).toList.sortBy(_._1).find(_._2.map(_._2).sum > numPresents2).map(_._1)
  println("part2", part2)

}