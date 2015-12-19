object Day14 extends App {

  val input = scala.io.Source.fromFile("src/input/Day14.txt").mkString.split("\n")

  case class Reindeer(name: String, speed: Int, runTime: Int, restTime: Int) {
    def fullCycleTime = runTime + restTime
  }

  val regex = "(.*) can fly (.*) km/s for (.*) seconds, but then must rest for (.*) seconds.".r
  implicit def stringToReindeer(s : String) : Reindeer = s match {
    case regex(name, speed, runTime, restTime) => Reindeer(name, speed.toInt, runTime.toInt, restTime.toInt)
  }


  def distance(r: Reindeer, time: Int) = {
    val numPeriods : Int = time / r.fullCycleTime
    val runTimeOfLastPeriod : Int = time % r.fullCycleTime
    val totalFlyTime = numPeriods * r.runTime + Math.min(runTimeOfLastPeriod, r.runTime)
    totalFlyTime * r.speed
  }

  val part1 = input.map(stringToReindeer).map(distance(_, 2503)).max

  println("part1", part1)


  //part two
  val reindeer = input.map(stringToReindeer)
  val winnersEachSecond : List[(String, Int)] = (for (sec <- 1 to 2503) yield reindeer.map(r => (r.name, distance(r, sec))).maxBy(_._2)).toList

  val part2 = winnersEachSecond
    .groupBy{ case (name, dist) => name }
    .map(_._2.length)
    .max

  println("part2", part2)

}