package game_entities

import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.util.Random
import scala.io.Source
import items._
import map_objects.GameBoard
import scala.collection.immutable
import scala.math.max

class EnemyConstructor(enemyType: String, name: String) {
  var reward = 0
  var effects = new ArrayBuffer[(String, Int, Int)]
  val lootableItems = new ArrayBuffer[(AbstractItem, Int)]
  var totalWeight = 0
  val rnd = new Random
  var bonusDef = 0
  var bonusAtt = 0
  var bonusHealth = 0
  var level = 0

  def build(init_pos: (Int, Int), board: GameBoard): Enemy = {
    val baseEnemy = enemyType match {
      case "robot"  => new Robot(init_pos, board, name)
      case "turret" => new Turret(init_pos, board, name)
      case "dog"    => new Dog(init_pos, board, name)
    }
    baseEnemy.levelUp(level)
    if (totalWeight > 0)
      lootableItems.foreach(t => baseEnemy.lootableItems += ((t._1, t._2.toDouble / totalWeight)))
    baseEnemy.reward = max(0, reward + rnd.nextInt(6) - 3)
    baseEnemy.baseAtt = baseEnemy.baseAtt + bonusAtt
    baseEnemy.baseDef = baseEnemy.baseDef + bonusDef
    baseEnemy.baseMaxHP = baseEnemy.baseMaxHP + bonusHealth
    baseEnemy.currentHP = baseEnemy.baseMaxHP
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

class EnemyParser(depth: Int) extends RegexParsers {

  val rnd = new Random

  val constant = """(-|\+)?(0|[1-9][0-9]*)""".r

  val char = """"(\w|\d| |\.)*\"""".r

  def number: Parser[Int] = constant ^^ { _.toInt }

  def text: Parser[String] = ("\"(\\w|\\d| |\\.)*\"".r ^^ { s =>
    val str = s.toString()
    str.substring(1, str.length - 1)
  })

  def enemyType: Parser[String] = "robot" | "turret" | "dog"

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

  def statModifier: Parser[EnemyConstructor => Unit] =
    ("strength" ~> number ^^ { case n => constructor: EnemyConstructor => constructor.bonusAtt += n })
      .|("defense" ~> number ^^ { case n => constructor: EnemyConstructor => constructor.bonusDef += n })
      .|("health" ~> number ^^ { case n => constructor: EnemyConstructor => constructor.bonusHealth += n })
      .|("level" ~> number ^^ { case n => constructor: EnemyConstructor => constructor.level += n })

  def modifier: Parser[EnemyConstructor => Unit] =
    ("loots" ~> rep1sep(item, "or") ^^ { case items =>
      constructor: EnemyConstructor => items.foreach((a => constructor.addItem(a._1, a._2)))
    })
      .|("reward" ~> number ^^ { case n => constructor: EnemyConstructor => constructor.reward += n })
      .|("with" ~> statModifier ^^ { case s => s })

  def description: Parser[((Int, Int), GameBoard) => Enemy] =
    text ~ ("of type" ~> enemyType) ~ ("and" ~> repsep(modifier, "and")) ^^ { case name ~ et ~ modifiers =>
      val constructor = new EnemyConstructor(et, name)
      constructor.level = depth
      modifiers.foreach(f => f(constructor))
      constructor.build
    }

}

object EnemyGenerator {
  def generateEnemy(filename: String, depth: Int): ((Int, Int), GameBoard) => Enemy = {
    val parser = new EnemyParser(depth)
    var file = Source.fromFile(s"src/main/resources/enemies/${filename}.edf")
    val countIt = file.getLines()
    val nbPreset = countIt.count(_ == "$")
    file.close
    file = Source.fromFile(s"src/main/resources/enemies/${filename}.edf")
    val fileIt = file.getLines()
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
