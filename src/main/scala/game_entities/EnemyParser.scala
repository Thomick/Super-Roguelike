package game_entities

import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.util.Random
import scala.io.Source
import items._

class EnemyConstructor {
  var name = ""
  var reward = 0
  var effects = new ArrayBuffer[String]
  var lootableItems = new ArrayBuffer[AbstractItem]
}

class EnemyParser(depth: Int) extends RegexParsers {

  val rnd = new Random

  val constructor = new EnemyConstructor
  val constant = "(-|+)?[1-9][0-9]+".r

  val char = "\"(\\w|\\d| |\\.)*\"".r

  def number: Parser[Int] = ("-?[1-9][0-9]+".r ^^ { _.toInt })

  def text: Parser[String] = ("\"(\\w|\\d| |\\.)*\"".r ^^ { s =>
    val str = s.toString()
    str.substring(1, str.length - 1)
  })

  def conjunction: Parser[Any] = "and"

  def disjonction: Parser[Any] = "or"

  def enemyType: Parser[Any] = "robot" | "turret" | "dog"

  def itemType: Parser[Any] = "armcannon"

  def item: Parser[AbstractItem] = itemType ~ opt(number) ^^ { case item ~ levelDifference =>
    val level = levelDifference match {
      case None    => depth
      case Some(n) => depth + n
    }
    val builtItem: AbstractItem = item match {
      case "armcannon" => new ArmCannon
    }
    if (builtItem.isInstanceOf[Upgradable])
      builtItem.asInstanceOf[Upgradable].upgrade(level)
    builtItem
  }

  def modifier: Parser[Any] = "named" ~ text | "loots " ~ item
  def modifierSequence: Parser[Any] = modifier ~ opt(conjunction ~ modifierSequence)

  def description: Parser[Any] = text ~ " of type " ~ enemyType ~ opt(conjunction ~ modifierSequence)

}
