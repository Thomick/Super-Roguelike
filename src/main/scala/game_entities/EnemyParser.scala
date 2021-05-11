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
  val lootableItems = new ArrayBuffer[(AbstractItem, Int)]
  var totalWeight = 0

  def build(): Enemy = {
    if (totalWeight > 0)
      lootableItems.foreach(t => baseEnemy.lootableItems += ((t._1, t._2.toDouble / totalWeight)))
    return baseEnemy
  }

  def addItem(item: Option[AbstractItem], weight: Int) = {
    totalWeight += weight
    item match {
      case None        => ()
      case Some(value) => lootableItems += ((value, weight))
    }
  }
}

class EnemyParser(depth: Int, init_pos: (Int, Int), board: GameBoard) extends RegexParsers {

  val rnd = new Random

  val constant = """(-|\+)?(0|[1-9][0-9]*)""".r

  val char = """"(\w|\d| |\.)*\"""".r

  def number: Parser[Int] = constant ^^ { _.toInt }

  def text: Parser[String] = ("\"(\\w|\\d| |\\.)*\"".r ^^ { s =>
    val str = s.toString()
    str.substring(1, str.length - 1)
  })

  def enemyType: Parser[Any] = "robot" | "turret" | "dog"

  def itemType: Parser[Any] = "armcannon" | "nothing"

  def item: Parser[(Option[AbstractItem], Int)] = itemType ~ opt(number) ~ opt("with weight" ~> number) ^^ {
    case "nothing" ~ _ ~ weight =>
      weight match {
        case None    => (None: Option[AbstractItem], 1)
        case Some(w) => (None: Option[AbstractItem], w)
      }
    case item ~ levelDifference ~ weight =>
      val level = levelDifference match {
        case None        => depth
        case Some(value) => depth + value
      }
      val w = weight match {
        case None        => 1
        case Some(value) => value
      }
      val builtItem: AbstractItem = item match {
        case "armcannon" => new ArmCannon
      }
      if (builtItem.isInstanceOf[Upgradable])
        builtItem.asInstanceOf[Upgradable].upgrade(level)
      (Some(builtItem), w)
  }

  def modifier: Parser[EnemyConstructor => Unit] = "loots" ~> rep1sep(item, "or") ^^ { case items =>
    constructor => items.foreach((a => constructor.addItem(a._1, a._2)))
  }

  def description: Parser[Enemy] = text ~ ("of type" ~> enemyType) ~ ("and" ~> repsep(modifier, "and")) ^^ {
    case name ~ et ~ modifiers =>
      val constructor = et match {
        case "robot"  => new EnemyConstructor(new Robot(init_pos, board, name))
        case "turret" => new EnemyConstructor(new Turret(init_pos, board, name))
        case "dog"    => new EnemyConstructor(new Dog(init_pos, board, name))
      }
      modifiers.foreach(f => f(constructor))
      constructor.build()
  }

}

object EnemyGenerator {
  def generateEnemy(filename: String, depth: Int, init_pos: (Int, Int), board: GameBoard): Enemy = {
    val parser = new EnemyParser(depth, init_pos, board)
    val file = Source.fromFile("src/main/resources/enemies/normal.edf")
    val fileIt = file.getLines()
    val nbPreset = 1
    val rnd = new Random
    val selectedPreset = rnd.nextInt(nbPreset) + 1
    var count = 0
    while (count < selectedPreset) {
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
    val enemy = parser.parse(parser.description, enemyDescription.toString()).get
    file.close
    return enemy
  }

}
