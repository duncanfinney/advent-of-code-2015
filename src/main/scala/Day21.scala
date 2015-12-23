object Day21 extends App {

  case class Item(name: String, cost: Int, damage: Int, armor: Int)

  val weapons = Set(
    Item("Dagger", 8, 4, 0),
    Item("Shortsword", 10, 5, 0),
    Item("Warhammer", 25, 6, 0),
    Item("Longsword", 40, 7, 0),
    Item("Greataxe", 74, 8, 0))

  val armor = Set(
    Item("No Armor", 0, 0, 0),
    Item("Leather", 13, 0, 1),
    Item("Chainmail", 31, 0, 2),
    Item("Splintmail", 53, 0, 3),
    Item("Bandedmail", 75, 0, 4),
    Item("Platemail", 102, 0, 5))

  val rings = Set(
    Item("None 1", 0, 0, 0),
    Item("None 2", 0, 0, 0),
    Item("Damage +1", 25, 1, 0),
    Item("Damage +2", 50, 2, 0),
    Item("Damage +3", 100, 3, 0),
    Item("Defense +1", 20, 0, 1),
    Item("Defense +2", 40, 0, 2),
    Item("Defense +3", 80, 0, 3)
  )

  case class Player(name: String, hp: Int, damage: Int, armor: Int) {
    def takeHit(damageScore: Int): Option[Player] = {
      val damageTaken = Math.max(damageScore - armor, 1)
      if (damageTaken >= hp) None
      else Some(Player(name, hp - damageTaken, damage, armor))
    }

    def equip(item: Item): Player = {
      Player(name, hp, damage + item.damage, armor + item.armor)
    }
  }

  def attackerWins(attacker: Player, defender: Player): Boolean = {
    val fogOfWar = defender.takeHit(attacker.damage)
    fogOfWar match {
      case None => true
      case Some(survivor) => !attackerWins(survivor, attacker)
    }
  }

  val player = Player("Player", 100, 0, 0)
  val boss = Player("Boss", 109, 8, 2)

  val part1 = (
    for {
      weapon <- weapons
      armor <- armor
      ring1 <- rings
      ring2 <- rings - ring1
      equippdPlayer = player.equip(weapon).equip(armor).equip(ring1).equip(ring2)
      if attackerWins(equippdPlayer, boss)
    } yield (weapon.cost + armor.cost + ring1.cost + ring2.cost, weapon, armor, ring1, ring2))
    .minBy(_._1)

  println("part1", part1)

  val part2 = (
    for {
      weapon <- weapons
      armor <- armor
      ring1 <- rings
      ring2 <- rings - ring1
      equippdPlayer = player.equip(weapon).equip(armor).equip(ring1).equip(ring2)
      if !attackerWins(equippdPlayer, boss)
    } yield (weapon.cost + armor.cost + ring1.cost + ring2.cost, weapon, armor, ring1, ring2))
    .maxBy(_._1)

  println("Part2", part2)
}