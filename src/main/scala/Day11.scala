package main.scala

object Day11 extends App {

  //requirement 1
  def hasIncreasingLetter(word: String): Boolean = {
    for (x <- word.sliding(3)) {
      val chars = x.toList
      if (chars.head + 1 == chars(1).toInt && chars(1) + 1 == chars(2).toInt)
        return true
    }
    false
  }

  //requirement 2
  val badLetterRegex =
    """(i|o|l)""".r

  def hasBadLetter(word: String): Boolean = badLetterRegex.findFirstMatchIn(word).isDefined;

  //requirement 3
  val regex =
    """(.)\1""".r

  def containsPairs(input: String): Boolean = regex.findAllIn(input).length >= 2


  val passwordIncrementor: String => String = (input: String) => (input.last + 1).toChar match {
    case 123 if input.length > 1 => passwordIncrementor(input.dropRight(1)) + "a"
    case 123 => "aa"
    case c => input.dropRight(1) + c.toChar
  }


  def isValid(x: String) = hasIncreasingLetter(x) && !hasBadLetter(x) && containsPairs(x)

  def nextPassword(input: String): String = Iterator
    .iterate(input)(passwordIncrementor)
    .drop(1)
    .find(isValid)
    .get

  val partOne = nextPassword("vzbxkghb")
  val partTwo = nextPassword(nextPassword("vzbxkghb"))

  println("partOne", partOne)
  println("partTwo", partTwo)


}