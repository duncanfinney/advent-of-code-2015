package main.scala

import play.api.libs.json._

object Day12 extends App {
  val input = scala.io.Source.fromFile("src/input/Day12.txt").mkString

  val regex = """-?\d+""".r

  println("part1", regex.findAllIn(input).map(_.toInt).sum)

  def totalPartTwo(json: JsValue): Int = json match {
    case JsNumber(x) => x.toInt
    case JsArray(x) => x.map(totalPartTwo).sum
    case JsObject(x) if !(x exists (y => y._2 == JsString("red"))) => {
      x.map(_._2).map(totalPartTwo).sum
    }
    case _ => 0
  }

  println("part2", totalPartTwo(Json.parse(input)))
}