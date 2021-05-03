package generator

import map_objects._
import scala.util.parsing.combinator._
import scala.collection.mutable._
import scala.util.Random
import items._
import game_entities._

class Room {
  var floorcells = Vector[(Int, Int)]()
  var wallcells = Vector[(Int, Int)]()
  var entities = Vector[(((Int,Int),GameBoard)=>GameEntity, (Int,Int))]()
  var items = Vector[(AbstractItem, (Int, Int))]()
  var possibleExits = Vector[(Direction.Value,(Int,Int))]()

  def addFloorcell(pos : (Int,Int)): Unit = {
    floorcells = pos +: floorcells
  }

  def addWallcell(pos : (Int,Int)): Unit = {
    wallcells = pos +: wallcells
  }

  def addEntity(pos : (Int,Int), entity : ((Int,Int),GameBoard)=>GameEntity): Unit = {
    entities = (entity, pos) +: entities
  }

  def addItem(pos : (Int,Int), item : AbstractItem): Unit = {
    items = (item, pos) +: items
  }

  def addPossibleExit(dir : Direction.Value, pos : (Int,Int)): Unit = {
    possibleExits = (dir,pos) +: possibleExits
  }
}

class RoomParser(depth : Int) extends RegexParsers {
  val room = new Room

  val speChar = Map[Char, Vector[(Int, Int)]]()

  val rnd = new Random

  def levelUp(c : Character, n : Int): Character = {
    c.levelUp(n)
    return c
  }

  def readGrid(entrance : Direction.Value, it : Iterator[String]): Unit = {
    val height = it.next().toInt
    var entrancex = 0
    var entrancey = 0
    entrance match {
      case Direction.Up => entrancex = it.next().toInt
                 entrancex = it.next().toInt
                 it.drop(6)
      case Direction.Right => it.drop(2)
                    entrancex = it.next().toInt
                    entrancex = it.next().toInt
                    it.drop(4)
      case Direction.Down => it.drop(4)
                   entrancex = it.next().toInt
                   entrancex = it.next().toInt
                   it.drop(2)
      case Direction.Left => it.drop(6)
                   entrancex = it.next().toInt
                   entrancex = it.next().toInt
    }
    var line = ""
    for (i <- height-1 to 0 by -1) {
      line = it.next()
      for (j<- 0 to line.length -1) {
        line.apply(j) match {
          case '#' => room.addWallcell(i-entrancex, j-entrancey)
          case '.' => room.addFloorcell(i-entrancex, j-entrancey)
          case ' ' | '\n' => ()
          case _ => speChar.get(line.apply(j)) match {
            case Some(v) => speChar(line.apply(j)) = (i-entrancex, j-entrancey) +: v
            case None => speChar(line.apply(j)) = Vector[(Int, Int)]((i-entrancex, j-entrancey))
          }
        }
      }
    }
  }

  def fillRemainingSpeChar(): Unit = {
    for(v <- speChar.values) {
      v.foreach(room.addFloorcell)
    }
  }

  val constant = "[1-9][0-9]+".r 

  val specialCharacter = "[a-z]|[A-Z]".r

  def number: Parser[Int] = (constant ^^ {_.toInt}) | ("depth" ^^ { s => depth})

  def conjunction: Parser[Any] = "and"

  def element: Parser[((Int,Int))=>Unit] = character | item | ("wall" ^^ {s => p => room.addWallcell(p)})

  def enemyType: Parser[Any] = "robot" | "turret" | "dog"

  def enemy: Parser[((Int,Int))=>Unit] = number ~ opt(enemyType) ^^ {
    case n ~ None =>
      rnd.nextInt(3) match {
        case 0 => p => room.addEntity(p,(pos,b) => levelUp(new Robot(pos,b),n-1))
        case 1 => p => room.addEntity(p,(pos,b) => levelUp(new Dog(pos,b),n-1))
        case 2 => p => room.addEntity(p,(pos,b) => levelUp(new Turret(pos,b),n-1))
      }
    case n ~ Some("robot") => p => room.addEntity(p,(pos,b) => levelUp(new Robot(pos,b),n-1))
    case n ~ Some("dog") => p => room.addEntity(p,(pos,b) => levelUp(new Dog(pos,b),n-1))
    case n ~ Some("turret") => p => room.addEntity(p,(pos,b) => levelUp(new Turret(pos,b),n-1) )
  }

  def itemType: Parser[Any] = "armcannon"

  def item: Parser[((Int,Int))=>Unit] = number ~ opt(itemType) ^^ {
    case n ~ None =>
      rnd.nextInt(3) match {
        case 0 => p => room.addItem(p,new Morphin)
        case 1 => p => room.addItem(p,new IronHelmet)
        case 2 => p => room.addItem(p,new LaserChainsaw)
      }
    case n ~ Some("armcannon") => p => room.addItem(p,new ArmCannon)
  }

  def character: Parser[((Int,Int))=>Unit] = enemy  | ("vending machine" ^^ {s => p => room.addEntity(p,((pos,b)=> new Shopkeeper(pos,b)))}) | ("computer" ^^ {s => p => room.addEntity(p,(pos,b)=> new Computer(pos,b))})

  def expr: Parser[Any] = number ~ (specialCharacter ^^ { _.apply(0) } ) ~ element ^^ {
    case n ~ c ~ e => 
      var shuffledVector = rnd.shuffle(speChar(c))
      speChar(c) = shuffledVector.zipWithIndex.filter(x => x._2 >= n).map(x=>x._1).toVector // We remove the n first element of the vector
      for (i <- 0 to n-1) {
        e.apply(shuffledVector(i))
      }
  }

  def program: Parser[Any] = expr ~opt(conjunction ~ program)

}
object RoomGenerator {
  val test = 0
}

