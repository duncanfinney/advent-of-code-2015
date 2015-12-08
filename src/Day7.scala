//TODO: Finish me
object Day7 extends App {

  trait Move {
    def toRegister: String
  }
  case class SetOp(value: Int, toRegister: String) extends Move
  case class And(r1: String, r2: String, toRegister: String) extends Move
  case class Or(r1: String, r2: String, toRegister: String) extends Move
  case class LShift(r1: String, numBits: Int, toRegister: String) extends Move
  case class RShift(r1: String, numBits: Int, toRegister: String) extends Move
  case class Not(r1: String, toRegister: String) extends Move

  object Move {
    private val digit = "(\\d*){1}"
    private val register = "([a-z]*){1}"

    private val setPattern = s"^$digit -> $register".r
    private val andPattern = s"^$register AND $register -> $register".r
    private val orPattern = s"^$register OR $register -> $register".r
    private val lShiftPattern = s"^$register LSHIFT $digit -> $register".r
    private val rShiftPattern = s"^$register RSHIFT $digit -> $register".r
    private val notPattern = s"^NOT $register -> $register".r

    def toMove(str: String): Move = str match {
      case setPattern(value, toRegister) => SetOp(value.toInt, toRegister)
      case andPattern(r1, r2, toRegister) => And(r1, r2, toRegister)
      case orPattern(r1, r2, toRegister) => Or(r1, r2, toRegister)
      case lShiftPattern(r1, numBits, toRegister) => LShift(r1, numBits.toInt, toRegister)
      case rShiftPattern(r1, numBits, toRegister) => RShift(r1, numBits.toInt, toRegister)
      case notPattern(r1, toRegister) => Not(r1, toRegister)
    }
  }

  val input = scala.io.Source.fromFile("src/input/Day7.txt", "utf8").mkString
  val allMoves = input.split("\n").map(_.trim).map(Move.toMove).toList


  println(allMoves.filter(move => move.toRegister == "a"))

  //PART 1
  type RegisterState = Map[String, Int]

//  def applyPartOneMove(currentState: RegisterState, move: Move): RegisterState = {
//    move match {
//      case SetOp(value, toReg) => currentState updated (toReg, value) //skip the setOpts because that is found int eh first pass
//      case And(r1, r2, toReg) => currentState updated(toReg, currentState(r1) & currentState(r2))
//      case Or(r1, r2, toReg) => currentState updated(toReg, currentState(r1) | currentState(r2))
//      case LShift(r1, numBits, toReg) => currentState updated(toReg, currentState(r1) << numBits)
//      case RShift(r1, numBits, toReg) => currentState updated(toReg, currentState(r1) >> numBits)
//      case Not(r1, toReg) => currentState updated (toReg, ~currentState(r1))
//    }
//  }

//  val initialState = allMoves.collect { case x : SetOp => x }.foldLeft(Map[String, Int]())(applyPartOneMove)
//  val part1 = allMoves.foldLeft(initialState)(applyPartOneMove).toList.sortBy(_._1)
//  println("part1", part1)
}