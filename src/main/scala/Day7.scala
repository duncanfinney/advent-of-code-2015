package main.scala

object Day7 extends App {

  val input = scala.io.Source.fromFile("src/input/Day7.txt").mkString.split("\n")

  val wires = input.map { line =>
    val Array(ex, v) = line.split(" -> ")
    v -> ex
  }.toMap

  var memo = Map.empty[String, Int]

  def get(v: String): Int = memo.getOrElse(v, {
    val res = if (v.head.isDigit)
      v.toInt
    else wires(v).split(" ") match {
      case Array(x) => get(x)
      case Array(a, "AND", b) => get(a) & get(b)
      case Array(a, "OR", b) => get(a) | get(b)
      case Array("NOT", a) => ~get(a)
      case Array(a, "RSHIFT", b) => get(a) >> get(b)
      case Array(a, "LSHIFT", b) => get(a) << get(b)
    }
    memo += v -> res
    res
  })

  println("part1", get("a"))

  memo = Map("b" -> get("a"))
  println("part2", get("a"))


}