package game_entities

import map_objects._
import scala.collection._
import scala.math.min
import scala.util.Random
import scala.math.{min, max}

// Base class for game characters
abstract class Character(init_pos: (Int, Int), b: GameBoard, hasLogs: Boolean = false)
    extends GameEntity(init_pos, b, hasLogs) {

  // Stats (must be positive)
  var baseMaxHP: Int = 10
  var baseDef: Int = 0
  var baseAtt: Int = 1
  var currentHP: Int = 10
  var statusList = new mutable.HashSet[Status]
  val activeEffects = new StatusResults
  var level = 1
  var xp: Int = 0
  var nextLevelCap: Int = 10

  def getDef(): Int = baseDef
  def getAtt(): Int = baseAtt
  def getMaxHP(): Int = baseMaxHP

  // Add an integer(positive or negative) to current HP and keep the value between 0 and getMaxHP()
  // Return true if the character died
  def addToHP(n: Int): Boolean = {
    currentHP = max(0, min(getMaxHP(), currentHP + n))
    if (currentHP == 0)
      die()
    return currentHP == 0
  }

  def levelUp(): Unit = {
    // Stat increase
    // +1 is used to avoid getting stuck because the base statistics are low
    baseAtt = ((baseAtt) * 1.2).toInt + 1
    baseDef = ((baseDef) * 1.2).toInt + 1
    val hpIncrease = ((baseMaxHP) * 1.2).toInt + 1 - baseMaxHP
    baseMaxHP += hpIncrease
    // On the newly acquired hp are added
    addToHP(hpIncrease)
    level = level + 1
    xp = 0
    nextLevelCap = (nextLevelCap * 1.3).toInt
    writeLog("You just leveled up. You feel stronger.")
  }

  // Level up multiple times
  def levelUp(n: Int): Unit = {
    for (i <- 1 to n)
      levelUp()
  }

  def getXp(n: Int): Unit = {
    var toAdd = n
    // xp amount can't exceed nextLevelCap, the remaining xp is added is the next iterations
    while (toAdd > 0) {
      val m = min(n, nextLevelCap - xp)
      toAdd = toAdd - m
      xp = xp + m
      if (xp == nextLevelCap)
        levelUp()
    }
  }

  // Move the character(C1) to a new position on the board
  // If there another character(C2) at this position, triggers the the current character action on the other character(C2)
  def move(nextPos: (Int, Int)): Unit = {
    if (!(pos == nextPos) && activeEffects.canMove)
      if (board.isFree(nextPos)) {
        board.entityMoved(this, nextPos)
        pos = nextPos
      } else if (board.hasEntity(nextPos)) {
        action(board.getEntity(nextPos))
      } else if (board.isTileInteractable(nextPos))
        board.interactWithTile(nextPos, this)
  }

  // Move the character to an adjacent square according to the specified direction
  def moveDir(dir: Direction.Value): Unit = move(Direction.nextPos(pos, dir))

  // Action of the current character on another character
  // (Usually triggered during collisions)
  def action(c: GameEntity): Unit

  // Hand to hand attack based on the character stats
  def attack(c: Character): Boolean = {
    val rnd = new Random
    val damage = max(0, (getAtt() + (rnd.nextInt(5) - 2)))
    return giveDamage(damage, c)
  }

  // Deals damage to another character (used to write logs about the attack and to test if there remains enemies after the attack)
  def giveDamage(damage: Int, c: Character): Boolean = {
    val (effectiveDamage, died) = c.takeDamage(this, damage)
    writeLog(name + " deals " + effectiveDamage.toString + " damage to " + c.name)
    if (died) {
      writeLog(name + " kills " + c.name)
      getXp(c.level)
    }
    return effectiveDamage > 0 && !died
  }

  // Compute effective damage based on defence stat and apply them to this character
  // Called by the opponent in order to effectively apply damages to its target
  // Return effective damage and if the character died or not
  def takeDamage(from: GameEntity, dam: Int): (Int, Boolean) = {
    if (currentHP == 0)
      return (0, false) // This character is already dead
    val effectiveDamage = max(0, dam - getDef())
    val died = addToHP(-effectiveDamage) // Decreases HP and keeps the value in
    writeLog(name + " receives " + effectiveDamage.toString + " damage from " + from.name)
    return (effectiveDamage, died)
  }

  // Called when a character dies
  def die(): Unit = {
    writeLog(name + " dies. Goodbye cruel world !")
    board.removeEntity(pos)
  }

  // Updates active effects for this turn and decreases the remaining time of each status
  def updateStatus(): Unit = {
    activeEffects.reset()
    statusList.foreach(status => status.applyEffect(activeEffects))
    statusList.retain(_.remainingTime != 0)
    addToHP(activeEffects.healthModifier)
  }

  // Removes the status that verify the predicate p
  def removeStatus(p: Status => Boolean): Unit = statusList.retain(s => !p(s))

}

// Shared trait for npc
// active attribute should change the npc behaviour during board update
trait AIControlled extends Character {
  var active: Boolean = false
  def activate(): Unit = active = true
  def deactivate(): Unit = active = false

  // Move toward the player
  def moveTowards(): Unit = {
    val sPath = board.shortestPath(pos, board.playerEntity.pos)
    sPath match {
      case Some(path) => move(path(1))
      case None       => None
    }
  }

  // Move away from the player
  def moveAway(): Unit = {
    val sPath = board.shortestPath(pos, board.playerEntity.pos)
    sPath match {
      case Some(path) =>
        board.oppositeFreeCell(pos, path(1)) match {
          case None    => ()
          case Some(p) => move(p)
        }
      case None => ()
    }
  }

  // Called during board update
  def act(visible: Boolean): Unit = updateStatus()
}

trait FleeingWhenDamaged extends Character with AIControlled {
  override def takeDamage(from: GameEntity, dam: Int): (Int, Boolean) = {
    activate
    return super.takeDamage(from, dam)
  }

  override def act(visible: Boolean): Unit = {
    updateStatus()
    moveAway()
  }
}
