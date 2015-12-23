object Day23 extends App {

  type Register = String

  trait Instruction {
    def apply(state: State): State
  }

  case class HLF(r: Register) extends Instruction {
    def apply(state: State) = state.changeRegister(r, _ / 2).moveInstructionPointer()
  }

  case class TPL(r: Register) extends Instruction {
    def apply(state: State) = state.changeRegister(r, _ * 3).moveInstructionPointer()
  }

  case class INC(r: Register) extends Instruction {
    def apply(state: State) = state.changeRegister(r, _ + 1).moveInstructionPointer()
  }

  case class JMP(offset: Int) extends Instruction {
    def apply(state: State) = state.moveInstructionPointer(offset)
  }

  case class JIE(r: Register, offset: Int) extends Instruction {

    def apply(state: State) = {
      if (state.getValue(r) % 2 == 0)
        state.moveInstructionPointer(offset)
      else
        state.moveInstructionPointer(1)
    }

  }

  case class JIO(r: Register, offset: Int) extends Instruction {

    def apply(state: State) = {
      if (state.getValue(r) == 1)
        state.moveInstructionPointer(offset)
      else
        state.moveInstructionPointer(1)
    }

  }

  //parse the input
  val instructions = scala.io.Source
    .fromFile("src/input/Day23.txt")
    .getLines
    .map(_.replaceAll(",", "").split(" ").toSeq)
    .map {
      case Seq("hlf", r) => HLF(r)
      case Seq("tpl", r) => TPL(r)
      case Seq("inc", r) => INC(r)
      case Seq("jmp", offset) => JMP(offset.toInt)
      case Seq("jie", r, offset) => JIE(r, offset.toInt)
      case Seq("jio", r, offset) => JIO(r, offset.toInt)
    }
    .toList


  case class State(registers: Map[Register, Int], instructionPointer: Int) {
    def changeRegister(r: Register, mappingFunc: Int => Int): State = copy(registers = registers ++ Map(r -> mappingFunc(registers.getOrElse(r, 0))))

    def moveInstructionPointer(offset: Int = 1): State = copy(instructionPointer = instructionPointer + offset)

    def getValue(r: Register): Int = registers.getOrElse(r, 0)

    lazy val nextInstruction = instructions.lift(instructionPointer)
  }

  val initialState = State(Map("a" -> 0, "b" -> 0), 0)

  def findFinalState(s: State): State = s.nextInstruction match {
    case Some(ins) => findFinalState(ins.apply(s))
    case None => s
  }

  val part1 = findFinalState(initialState)
  val part2 = findFinalState(initialState.changeRegister("a", _ => 1))

  println("part1", part1)
  println("part2", part2)

}