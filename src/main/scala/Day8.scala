package main.scala

//WIP
object Day8 extends App {

  val input = scala.io.Source.fromFile("src/input/Day8.txt")
    .mkString
    .split("\n")

  val regex = raw"(\\+)(x[^\\]{2}|.)".r

  val part1 = input.map { line =>
    regex.findAllMatchIn(line).map { m =>
      val backslashes = m.group(1).size
      val evenNumber = backslashes % 2 == 0
      backslashes / 2 + (if (evenNumber) 0 else m.group(2).size)
    }.sum + 2
  }.sum


  val part2 = input.map(_.count(Seq('\\', '"').contains) + 2).sum

  println("part1", part1)
  println("part2", part2)
}