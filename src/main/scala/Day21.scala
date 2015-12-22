import scala.annotation.tailrec

object Day21 extends App {

  trait Item {
    def name: String
    def cost: Int
    def armor: Int
    def damage: Int
  }

  case class Weapon(name: String, cost: Int, armor: Int, damage: Int) extends Item
  case class Armor(name: String, cost: Int, armor: Int, damage: Int) extends Item
  case class Ring(name: String, cost: Int, armor: Int, damage: Int) extends Item

  val allItems = List(
    Weapon("Dagger", 8, 4, 0),
    Weapon("Shortsword", 10, 5, 0),
    Weapon("Warhammer", 25, 6, 0),
    Weapon("Longsword", 40, 7, 0),
    Weapon("Greataxe", 74, 8, 0),

    Armor("Leather", 13, 0, 1),
    Armor("Chainmail", 31, 0, 2),
    Armor("Splintmail", 53, 0, 3),
    Armor("Bandedmail", 75, 0, 4),
    Armor("Platemail", 102, 0, 5),

    Ring("Damage +1", 25, 1, 0),
    Ring("Damage +2", 50, 2, 0),
    Ring("Damage +3", 100, 3, 0),
    Ring("Defense +1", 20, 0, 1),
    Ring("Defense +2", 40, 0, 2),
    Ring("Defense +3", 80, 0, 3)
  )


  object Turn extends Enumeration {
    type Turn = Value
    val Boss, Player = Value
  }

  import Turn._

  case class Player(items: List[Item], hitPoints: Int) {

    val cost = items.map(_.cost).sum
    val armor = items.map(_.armor).sum
    val damage = items.map(_.damage).sum

    def doDamage(amt: Int) = Player(items, hitPoints - amt)
  }

  case class Boss(hitPoints: Int, damage: Int, armor: Int) {
    def doDamage(amt: Int) = Boss(hitPoints - amt, damage, armor)
  }


  case class GameState(player: Player, boss: Boss, nextToAttack: Turn) {

    val isOver = player.hitPoints <= 0 || boss.hitPoints <= 0

    lazy val nextState =
      if (nextToAttack == Turn.Player)
        GameState(player, boss.doDamage(Math.max(player.damage - boss.armor, 1)), Turn.Boss)
      else
        GameState(player.doDamage(Math.max(boss.damage - player.armor, 1)), boss, Turn.Player)

    @tailrec
    final def winner: Turn =
      if (player.hitPoints <= 0)
        Turn.Boss
      else if (boss.hitPoints <= 0)
        Turn.Player
      else
        nextState.winner

  }

  //part1
  def isValidCombination(items: List[Item]) = {
    val weaponsCount = items.count(_.isInstanceOf[Weapon])
    val armorCount = items.count(_.isInstanceOf[Armor])
    val ringCount = items.count(_.isInstanceOf[Ring])
    weaponsCount == 1 && armorCount <= 1 && ringCount <= 2
  }

  val allPossibleCombinations = (allItems.combinations(4) ++ allItems.combinations(3) ++ allItems.combinations(2) ++ allItems.combinations(1)) filter isValidCombination

  val part1 = allPossibleCombinations
    .map(items => GameState(Player(items, 100), Boss(104, 8, 1), Turn.Player))
    .filter(_.winner == Turn.Player) //winner = player
    .map { case GameState(Player(items, _), _, _) => items.map(_.cost).sum }
    .toList
    .min

  println("Part1", part1)

}