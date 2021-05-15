package generator

import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.util.Random
import scala.io.Source

import items._
import game_entities._
import map_objects._

class Room {
  var floorcells = Vector[(Int, Int)]()
  var wallcells = Vector[(Int, Int)]()
  var levers = Vector[(Int, Int)]()
  var lockedDoors = Vector[(Int, Int)]()
  var locks = Vector[(Int, Int)]()
  var elevators = Vector[(Int, Int)]()
  var bosses = Vector[(((Int, Int), GameBoard) => GameEntity, (Int, Int))]()
  var entities = Vector[(((Int, Int), GameBoard) => GameEntity, (Int, Int))]()
  var items = Vector[(AbstractItem, (Int, Int))]()
  var possibleExits = Vector[(Direction.Value, (Int, Int))]()

  def addFloorcell(pos: (Int, Int)): Unit = {
    floorcells = pos +: floorcells
  }

  def addWallcell(pos: (Int, Int)): Unit = {
    wallcells = pos +: wallcells
  }

  def addLever(pos: (Int, Int)): Unit = {
    levers = pos +: levers
  }

  def addLockedDoor(pos: (Int, Int)): Unit = {
    lockedDoors = pos +: lockedDoors
  }

  def addLock(pos: (Int, Int)): Unit = {
    locks = pos +: locks
  }

  def addElevator(pos: (Int, Int)): Unit = {
    elevators = pos +: elevators
  }

  def addBoss(pos: (Int, Int), entity: ((Int, Int), GameBoard) => GameEntity): Unit = {
    bosses = (entity, pos) +: bosses
  }

  def addEntity(pos: (Int, Int), entity: ((Int, Int), GameBoard) => GameEntity): Unit = {
    entities = (entity, pos) +: entities
  }

  def addItem(pos: (Int, Int), item: AbstractItem): Unit = {
    items = (item, pos) +: items
  }

  def addPossibleExit(dir: Direction.Value, pos: (Int, Int)): Unit = {
    possibleExits = (dir, pos) +: possibleExits
  }
}

class RoomParser(depth: Int) extends RegexParsers {
  val room = new Room

  val speChar = Map[Char, Vector[(Int, Int)]]()

  val rnd = new Random

  def levelUp(c: Character, n: Int): Character = {
    c.levelUp(n)
    return c
  }

  def sub2D(pos1: (Int, Int), pos2: (Int, Int)): (Int, Int) = (pos1._1 - pos2._1, pos1._2 - pos2._2)

  def readGrid(entrance: Direction.Value, it: Iterator[String]): Unit = {
    var line = it.next()
    var temp = line.substring(7).split(",")
    val width = temp(0).toInt
    val height = temp(1).toInt
    var entrancepos = (width / 2, height / 2)
    var initialDirection = Direction.Nop
    var exit = Vector[(Direction.Value, (Int, Int))]()
    line = it.next()
    while (!(line == "%")) {
      temp = line.substring(4).split(",")
      val dir = line.apply(0) match {
        case 'N' => Direction.Up
        case 'E' => Direction.Right
        case 'S' => Direction.Down
        case 'W' => Direction.Left
        case _   => Direction.Nop
      }
      if (initialDirection == Direction.Nop && entrance != Direction.Nop) {
        initialDirection = dir
        entrancepos = Direction.turnBasisVector(initialDirection, (temp(0).toInt, temp(1).toInt), entrance)
      } else {
        room.addPossibleExit(
          Direction.turnBasis(initialDirection, dir, entrance),
          sub2D(Direction.turnBasisVector(initialDirection, (temp(0).toInt, temp(1).toInt), entrance), entrancepos)
        )
      }
      line = it.next()
    }
    for (i <- height - 1 to 0 by -1) {
      line = it.next()
      for (j <- 0 to line.length - 1) {
        val relativepos = sub2D(Direction.turnBasisVector(initialDirection, (j, i), entrance), entrancepos)
        line.apply(j) match {
          case '#'              => room.addWallcell(relativepos._1, relativepos._2)
          case '.'              => room.addFloorcell(relativepos._1, relativepos._2)
          case ' ' | '\n' | '/' => ()
          case _ =>
            speChar.get(line.apply(j)) match {
              case Some(v) => speChar(line.apply(j)) = relativepos +: v
              case None    => speChar(line.apply(j)) = Vector[(Int, Int)](relativepos)
            }
        }
      }
    }
  }

  def fillRemainingSpeChar(): Unit = {
    for (v <- speChar.values) {
      v.foreach(room.addFloorcell)
    }
  }

  val constant = "[1-9][0-9]*".r

  val specialCharacter = "[a-z]|[A-Z]".r

  def number: Parser[Int] = (constant ^^ { _.toInt }) | ("depth" ^^ { s => depth })

  def conjunction: Parser[Any] = "and"

  def element: Parser[((Int, Int)) => Unit] = character | item | ("wall" ^^ { s => p => room.addWallcell(p) }) | ("lever" ^^ { s => p => room.addLever(p) }) | ("lockedDoor" ^^ { s => p => room.addLockedDoor(p) }) | ("lock" ^^ { s => p => room.addLock(p) }) | ("elevator" ^^ { s => p => room.addElevator(p) })

  def enemyDifficulty: Parser[String] = "easy" | "normal" | "hard" | "boss"

  def enemy: Parser[((Int, Int)) => Unit] = enemyDifficulty ^^ { s => p =>
    if (s == "boss") {
      room.addBoss(p, EnemyGenerator.generateEnemy(s, depth))
    } else {
      room.addEntity(p, EnemyGenerator.generateEnemy(s, depth))
    }
  }

  def itemType: Parser[Any] = "morphin" | "ironhelmet" | "laserchainsaw" | "bandage" | "armcannon" | "lasereyes" | "cowboyhat" | "heavyjacket" | "knuckles" | "poweredhammer" | "item"

  def item: Parser[((Int, Int)) => Unit] = itemType ^^ {
    case "morphin"       => p => room.addItem(p, new Morphin)
    case "ironhelmet"    => p => room.addItem(p, new IronHelmet)
    case "laserchainsaw" => p => room.addItem(p, new LaserChainsaw)
    case "bandage"       => p => room.addItem(p, new Bandage)
    case "armcannon"     => p => room.addItem(p, new ArmCannon)
    case "lasereyes"     => p => room.addItem(p, new LaserEyes)
    case "cowboyhat"     => p => room.addItem(p, new CowboyHat)
    case "heavyjacket"   => p => room.addItem(p, new HeavyJacket)
    case "knuckles"      => p => room.addItem(p, new Knuckles)
    case "poweredhammer" => p => room.addItem(p, new PoweredHammer)
    case _ =>
      rnd.nextInt(10) match {
        case 0 => p => room.addItem(p, new Morphin)
        case 1 => p => room.addItem(p, new IronHelmet)
        case 2 => p => room.addItem(p, new LaserChainsaw)
        case 3 => p => room.addItem(p, new Bandage)
        case 4 => p => room.addItem(p, new ArmCannon)
        case 5 => p => room.addItem(p, new LaserEyes)
        case 6 => p => room.addItem(p, new CowboyHat)
        case 7 => p => room.addItem(p, new HeavyJacket)
        case 8 => p => room.addItem(p, new Knuckles)
        case 9 => p => room.addItem(p, new PoweredHammer)
      }
  }

  def character: Parser[((Int, Int)) => Unit] = enemy | ("vending machine" ^^ { s => p => room.addEntity(p, ((pos, b) => new Shopkeeper(pos, b))) }) | ("computer" ^^ { s => p => room.addEntity(p, (pos, b) => new Computer(pos, b)) })

  def expr: Parser[Any] = number ~ (specialCharacter ^^ { _.apply(0) }) ~ element ^^ { case n ~ c ~ e =>
    var shuffledVector = rnd.shuffle(speChar(c))
    speChar(c) = shuffledVector.patch(0, Nil, n)
    for (i <- 0 to n - 1) {
      e.apply(shuffledVector(i))
    }
  }

  def program: Parser[Any] = expr ~ opt(conjunction ~ program)

}
object RoomGenerator {
  def generateRoom(depth: Int, entrance: Direction.Value, filename: String): Room = {
    val parser = new RoomParser(depth)
    val file = Source.fromFile(s"src/main/resources/rooms/${filename}.rdf")
    val fileIt = file.getLines()
    val nbPresetRooms = fileIt.next().toInt
    val rnd = new Random
    val selectedPreset = rnd.nextInt(nbPresetRooms)
    var count = 0
    while (count <= selectedPreset) {
      if (fileIt.next() == "$") {
        count = count + 1
      }
    }
    parser.readGrid(entrance, fileIt)
    if ((fileIt.next())(0) == '%') {
      parser.parseAll(parser.program, fileIt.next())
    }
    parser.fillRemainingSpeChar()
    file.close
    return parser.room
  }

}
