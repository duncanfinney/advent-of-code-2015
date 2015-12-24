object Day22 extends App {

  case class Spell(name: String, cost: Int, castCallback: (Spell, Player, Player) => (Player, Player), effect: Option[Effect]) {
    def possible(game: Game) = cost <= game.player.mana && effect.map(e => !game.effects.exists(r => r.turns > 1 && r.name == e.name)).getOrElse(true)

    def cast(caster: Player, opposition: Player) = castCallback(this, caster, opposition)
  }

  case class Effect(name: String, turns: Int, effectCallback: (Effect, Player, Player) => (Player, Player)) {
    def effect(caster: Player, opposition: Player) = effectCallback(this, caster, opposition)

    def deplete(): Effect = copy(turns = turns - 1)
  }

  case class Game(player: Player, boss: Player, usedMana: Int, effects: Seq[Effect]) {
    def playerWon = boss.hp <= 0

    def bossWon = player.hp <= 0

    def playerTurn(spell: Spell, hard: Boolean): Game = {
      val changedPlayer = if (hard) player.damage(1) else player

      if (hard && changedPlayer.hp <= 0) return this.copy(player = changedPlayer)

      val (effectedPlayer, effectedBoss) = runEffects(changedPlayer)
      val (castPlayer, castBoss) = spell.cast(effectedPlayer, effectedBoss)

      Game(castPlayer, castBoss, usedMana + spell.cost, spell.effect.foldLeft(depleteEffects())(_ :+ _))
    }

    def bossTurn() = {
      val (effectedPlayer, effectedBoss) = runEffects(player)

      this.copy(player = effectedPlayer.damage(boss.dmg), boss = effectedBoss, effects = depleteEffects())
    }

    private def runEffects(player: Player) = effects.foldLeft(player, boss)((s, e) => e.effect(s._1, s._2))

    private def depleteEffects() = effects.map(_.deplete()).filter(_.turns > 0)
  }

  case class Player(hp: Int, dmg: Int, armor: Int, mana: Int) {
    def damage(attack: Int) = this.copy(hp = hp - (attack - armor).max(1))

    def heal(heal: Int) = this.copy(hp = hp + heal)

    def changeMana(change: Int) = this.copy(mana = mana + change)

    def changeArmor(increase: Int) = this.copy(armor = armor + increase)
  }

  object GameSimulator {
    val spells = Seq(
      Spell("Magic Missile", 53, (s, c, o) => (c.changeMana(-s.cost), o.damage(4)), None),
      Spell("Drain", 73, (s, c, o) => (c.heal(2).changeMana(-s.cost), o.damage(2)), None),
      Spell("Shield", 113, (s, c, o) => (c.changeMana(-s.cost).changeArmor(7), o), Some(Effect("Shield Effect", 6, (e, c, o) => (if (e.turns == 1) c.changeArmor(-7) else c, o)))),
      Spell("Poison", 173, (s, c, o) => (c.changeMana(-s.cost), o), Some(Effect("Poison Effect", 6, (e, c, o) => (c, o.damage(3))))),
      Spell("Recharge", 229, (s, c, o) => (c.changeMana(-s.cost), o), Some(Effect("Recharge Effect", 5, (e, c, o) => (c.changeMana(101), o))))
    )

    def simulate: (Seq[Game], Int, Boolean, Boolean) => Int = (games: Seq[Game], bestGame: Int, playerTurn: Boolean, hardMode: Boolean) => (games, playerTurn) match {
      case (Seq(), _) => bestGame
      case (_, true) =>
        val gamePossibilities = games.flatMap(g => spells.filter(_.possible(g)).map(g.playerTurn(_, hardMode))).distinct
        val score: Int = getBestScore(bestGame, gamePossibilities)
        simulate(getGamesToCheck(bestGame, gamePossibilities), score, false, hardMode)
      case (_, false) =>
        val gamePossibilities = games.map(_.bossTurn()).distinct
        simulate(getGamesToCheck(bestGame, gamePossibilities), getBestScore(bestGame, gamePossibilities), true, hardMode)
    }

    def getGamesToCheck(bestGame: Int, gamePossibilities: Seq[Game]) = gamePossibilities.filter(g => !g.playerWon && !g.bossWon && g.usedMana < bestGame)

    def getBestScore(bestGame: Int, gamePossibilities: Seq[Game]) = gamePossibilities.filter(_.playerWon).sortBy(_.usedMana).headOption.map(_.usedMana).getOrElse(bestGame).min(bestGame)

    def run() = {
      val bossInput = scala.io.Source.fromFile("src/input/Day22.txt").getLines.toList

      val boss = Player(bossInput.head.split(": ")(1).toInt, bossInput(1).split(": ")(1).toInt, 0, 0)

      val minimumWin = simulate(Seq(Game(Player(50, 0, 0, 500), boss, 0, Seq())), Int.MaxValue, true, false)
      println(minimumWin)

      val minimumWinHard = simulate(Seq(Game(Player(50, 0, 0, 500), boss, 0, Seq())), Int.MaxValue, true, true)
      println(minimumWinHard)
    }

  }

  GameSimulator.run

}