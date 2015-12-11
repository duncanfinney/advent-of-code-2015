//WIP
object Day8 extends App {

  val input =
    scala.io.Source.fromFile("src/input/Day8.txt")
    .mkString
    .split("\n")
//  val input =
//    """
//      |""
//      |"abc"
//      |"aaa\"aaa"
//      |"\x27"
//    """.stripMargin
//       .split("\n")
//       .filter(_.length > 0)

  //part one
  val totalCharactersOfCode = input.map(_.length).sum
  val totalCharactersOfStr = input.map(x => {
    val replaced = x.replaceAll("(^\"|\"$)", "")
      .replaceAll("(\\\\|\\\")", "|")
      .replaceAll("(\\\\x[0-9a-f]+)", "|")

    println(s"$x (${x.length}) -> $replaced (${replaced.length})")
    replaced.length

  }).sum
  val partOne = totalCharactersOfCode - totalCharactersOfStr
  println("partOne", partOne)
}