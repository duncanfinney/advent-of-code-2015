package main.scala

object Day9 extends App {

  val input = scala.io.Source.fromFile("src/input/Day9.txt").mkString

  val regex = """(.*) to (.*) = (\d*)""".r
  val distances = input
    .split("\n")
    .map { case regex(from, to, num) => (from, to) -> num.toInt }
    .toMap


  var keyset = distances.keySet.toList
  val allCities = (keyset.map(_._2) ++ keyset.map(_._1)).distinct

  //part 1
  def distance(places: (String, String)): Int = distances.getOrElse(places, distances(places.swap))
  def sumRoute(route: List[String]) = route
    .sliding(2)
    .map(x => (x.head, x(1)))
    .map(distance)
    .sum

  println("part1", allCities.permutations.map(sumRoute).min)
  println("part2", allCities.permutations.map(sumRoute).max)
}