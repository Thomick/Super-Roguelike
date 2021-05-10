package game_entities

import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.util.Random
import scala.io.Source
import items._
import map_objects.GameBoard

class EnemyConstructor(baseEnemy: Enemy) {
  var reward = 0
  var effects = new ArrayBuffer[String]
  var lootableItems = new ArrayBuffer[AbstractItem]

  def build(): Enemy = {
    baseEnemy.lootableItems = lootableItems
    return baseEnemy
  }
}

class EnemyParser(depth: Int, init_pos: (Int, Int), board: GameBoard) extends RegexParsers {

  val rnd = new Random

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

  def modifier: Parser[EnemyConstructor => Unit] = "loots" ~> item ^^ { case item =>
    constructor => constructor.lootableItems += item
  }

  def modifierSequence: Parser[List[EnemyConstructor => Unit]] = modifier ~ ((conjunction ~> modifier) *) ^^ {
    case x ~ y => x :: y
  }

  def description: Parser[Enemy] = text ~ ("of type" ~> enemyType) ~ opt(conjunction ~> modifierSequence) ^^ {
    case name ~ et ~ modifiers =>
      val constructor = et match {
        case "robot"  => new EnemyConstructor(new Robot(init_pos, board, name))
        case "turret" => new EnemyConstructor(new Turret(init_pos, board, name))
        case "dog"    => new EnemyConstructor(new Dog(init_pos, board, name))
      }
      modifiers match {
        case None       => ()
        case Some(mods) => mods.foreach(f => f(constructor))
      }
      constructor.build()
  }

}

object EnemyGenerator {
  def generateEnemy(filename: String, depth: Int, init_pos: (Int, Int), board: GameBoard): Enemy = {
    val parser = new EnemyParser(depth, init_pos, board)
    val file = Source.fromFile("src/main/resources/enemies/" + filename + ".des")
    val fileIt = file.getLines()
    val nbPreset = fileIt.count(_ == "$")
    val rnd = new Random
    val selectedPreset = rnd.nextInt(nbPreset) + 1
    var count = 0
    while (count <= selectedPreset) {
      if (fileIt.next() == "$") {
        count = count + 1
      }
    }
    val enemyDescription = new StringBuilder
    var lastStr = fileIt.next()
    while (lastStr != "%") {
      enemyDescription ++= " " + lastStr
      lastStr = fileIt.next()
    }
    val enemy = parser.parseAll(parser.description, enemyDescription.toString()).get
    file.close
    return enemy
  }

}
