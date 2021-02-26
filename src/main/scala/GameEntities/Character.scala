package GameEntities

import map_objects._
import scala.collection._
import scala.math.min
import scala.util.Random
import scala.math.{min, max}

// Base class for game characters
abstract class Character(init_pos: (Int, Int), b: GameBoard, hasLogs: Boolean = false)
    extends GameEntity(init_pos, b, hasLogs) {

  val baseMaxHP: Int = 10
  val baseDef: Int = 0
  val baseAtt: Int = 0
  var currentHP: Int = 10

  def getDef(): Int = baseDef
  def getAtt(): Int = baseAtt
  def getMaxHP(): Int = baseMaxHP

  // Move the character(C1) to a new position on the board
  // If there another character(C2) at this position, triggers the the current character action on the other character(C2)
  def move(nextPos: (Int, Int)): Unit = {
    if (!(pos == nextPos))
      if (board.isFree(nextPos)) {
        board.entityMoved(this, nextPos)
        pos = nextPos
      } else if (board.hasCharacter(nextPos)) {
        action(board.getCharacter(nextPos))
      }
  }

  // Move the character to an adjacent square according to the specified direction
  def moveDir(dir: Direction.Value): Unit = move(Direction.nextPos(pos, dir))

  // Action of the current character on another character
  // (Usually triggered during collisions)
  def action(c: Character): Unit

  // Hand to hand attack based on the character stats
  def attack(c: Character): Unit = {
    val rnd = new Random
    val damage = max(0, (getAtt() * (1 + 3 * rnd.nextGaussian())).toInt)
    val (effectiveDamage, died) = c.take_damage(this, damage)
    writeLog(name + " deals " + effectiveDamage.toString + " damages to " + c.name)
    if (died)
      writeLog(name + " kills " + c.name)
  }

  // Compute effective damage based on defense stat and apply them to this character
  // Called by the opponent in order to effectively apply damages to its target
  // Return effective damage
  def take_damage(from: Character, dam: Int): (Int, Boolean) = {
    val effectiveDamage = max(0, dam - getDef())
    currentHP -= effectiveDamage
    writeLog(name + "receives " + effectiveDamage.toString + "damages from" + from.name)
    if (currentHP <= 0) {
      die()
      return (effectiveDamage, true) // The second value indicates if the attack killed the character or not
    }
    return (effectiveDamage, false)
  }

  // Called when a character dies
  def die(): Unit = {
    writeLog(name + " dies. Goodbye cruel world !")
    board.removeCharacter(pos)
  }
}

// Shared trait for npc
// active attribute should change the npc behaviour during board update
trait AIControlled extends Character {
  var active: Boolean = false
  def activate(): Unit = active = true
  def deactivate(): Unit = active = false

  // Called during board update
  def act(): Unit = ()
}
