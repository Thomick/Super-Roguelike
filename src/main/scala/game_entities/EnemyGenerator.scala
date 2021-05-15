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
  var totalItemWeight = 0
  var totalEffectWeight = 0
  val rnd = new Random
  var bonusDef = 0
  var bonusAtt = 0
  var bonusHealth = 0
  var level = 0
  var image = ""

  def build(init_pos: (Int, Int), board: GameBoard): Enemy = {
    val baseEnemy = enemyType match {
      case "ranged"    => new MovingRangedEnemy(init_pos, board, name)
      case "fixranged" => new RangedEnemy(init_pos, board, name)
      case "melee"     => new MeleeEnemy(init_pos, board, name)
    }
    baseEnemy.levelUp(level)
    if (totalItemWeight > 0)
      lootableItems.foreach(t => baseEnemy.lootableItems += ((t._1, t._2.toDouble / totalItemWeight)))
    if (totalEffectWeight > 0)
      effects.foreach(t => baseEnemy.effects += ((t._1, t._2, t._3.toDouble / totalEffectWeight)))
    baseEnemy.reward = max(0, reward + rnd.nextInt(6) - 3)
    baseEnemy.baseAtt = baseEnemy.baseAtt + bonusAtt
    baseEnemy.baseDef = baseEnemy.baseDef + bonusDef
    baseEnemy.baseMaxHP = baseEnemy.baseMaxHP + bonusHealth
    baseEnemy.currentHP = baseEnemy.baseMaxHP
    if (image != "")
      baseEnemy.image = s"src/main/resources/enemy_sprites/${image}.png"
    return baseEnemy
  }

  def addItem(item: Option[AbstractItem], weight: Int) = {
    totalItemWeight += weight
    item match {
      case None        => ()
      case Some(value) => lootableItems += ((value, weight))
    }
  }
  def addEffect(effectType: String, duration: Int, weight: Int) = {
    totalEffectWeight += weight
    effectType match {
      case "nothing" => ()
      case et        => effects += ((et, duration, weight))
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

  def enemyType: Parser[String] = "ranged" | "fixranged" | "melee"

  def effectType: Parser[String] = "burning" | "regeneration" | "stunned" | "bleeding" | "nothing"

  def effect: Parser[(String, Int, Int)] =
    effectType ~ opt("for" ~> number <~ "turns") ~ opt("with weight" ~> number) ^^ { case et ~ duration ~ weight =>
      (et, duration.getOrElse(2), weight.getOrElse(1))
    }

  def itemType: Parser[Any] = "morphin" | "ironhelmet" | "laserchainsaw" | "bandage" | "armcannon" | "lasereyes" | "cowboyhat" | "heavyjacket" | "knuckles" | "poweredhammer" | "nothing"

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
        case "morphin"       => new Morphin
        case "ironhelmet"    => new IronHelmet
        case "laserchainsaw" => new LaserChainsaw
        case "bandage"       => new Bandage
        case "armcannon"     => new ArmCannon
        case "lasereyes"     => new LaserEyes
        case "cowboyhat"     => new CowboyHat
        case "heavyjacket"   => new HeavyJacket
        case "knuckles"      => new Knuckles
        case "poweredhammer" => new PoweredHammer
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

  def modifier: Parser[EnemyConstructor => Any] =
    ("loots" ~> rep1sep(item, "or") ^^ { case items =>
      constructor: EnemyConstructor => items.foreach((a => constructor.addItem(a._1, a._2)))
    })
      .|("reward" ~> number ^^ { case n => constructor: EnemyConstructor => constructor.reward += n })
      .|("with" ~> statModifier ^^ { case s => s })
      .|("applies" ~> effect ^^ { case (et, d, w) => constructor: EnemyConstructor => constructor.addEffect(et, d, w) })
      .|("looks like" ~> text ^^ { case s => constructor: EnemyConstructor => constructor.image = s })

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
