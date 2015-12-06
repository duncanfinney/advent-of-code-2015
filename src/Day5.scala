import com.sun.tools.javac.jvm.Items

import scala.util.matching.Regex

object Day5 extends App {

  val VOWELS = "aeiou".toArray.toSet

  def removeDupes[T](items: Seq[T]): Seq[T] = items match {
    case Nil => items
    case a :: b :: rest if a == b => Seq(a) ++ removeDupes(rest)
    case a :: rest => Seq(a) ++ removeDupes(rest)
  }

  case class Word(str: String) {
    //part 1
    def vowelCount = str.count(VOWELS.contains)

    def containsDoubleLetter = (0 until str.length - 1).exists(x => str(x) == str(x + 1))

    def containsBadChars = ("(ab|cd|pq|xy)".r findFirstIn str).isDefined

    def isPart1Nice = vowelCount >= 3 && containsDoubleLetter && !containsBadChars

    //part 2
    def allPairs = str.iterator.sliding(2).map(chars => (chars.head, chars(1))).toList

    def hasDuplicatePair =
      removeDupes(allPairs)
        .groupBy(x => x)
        .mapValues(_.size)
        .exists {
          case (pair, cnt) => cnt > 1
        }

    def isPartTwoNice = {

      def hasRepeatedLetterWithLetterInBetween(str: List[Char]): Boolean = str.toList match {
        case Nil => false
        case c1 :: c2 :: c3 :: rest if c1 == c3 => true
        case _ :: xs => hasRepeatedLetterWithLetterInBetween(xs)
      }

      hasDuplicatePair && hasRepeatedLetterWithLetterInBetween(str.toList)
    }
  }


  assert(Word("hello").vowelCount == 2)

  assert(Word("ttest").containsDoubleLetter)
  assert(!Word("test").containsDoubleLetter)

  assert(Word("abcd").containsBadChars)
  assert(!Word("Duncan").containsBadChars)

  assert(Word("ugknbfddgicrmopn").isPart1Nice)
  assert(Word("aaa").isPart1Nice)

  assert(!Word("haegwjzuvuyypxyu").isPart1Nice)

  val input = scala.io.Source.fromFile("src/input/Day5.txt", "utf8").mkString

  val part1 = input
    .split("\n")
    .map(line => Word(line))
    .count(_.isPart1Nice)

  println("part1", part1)

  //part2
  assert(Word("abc").allPairs == List(('a', 'b'), ('b', 'c')))

  assert(!Word("aaa").hasDuplicatePair)
  assert(Word("aaaa").hasDuplicatePair)

  val part2 = input
    .split("\n")
    .map(Word)
    .count(_.isPartTwoNice)

  println("part2", part2)


}