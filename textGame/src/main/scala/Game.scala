package textGame

import scala.io.StdIn._

class Game {
  import Domain._
  import Logic._

  object Domain {

    case class Player(name: String, var x: Int, var y: Int)

    object Player {
      def begin(name: String) = Player(name, 0, 0)
    }

    case class Field(grid: Vector[Vector[String]])

    object Field {
      def mk20x20 =
        Field(Vector.fill(20, 20)("-"))
    }

    case class GameWorld(player: Player, field: Field)
  }

  sealed trait DirectionMovement
  case object Up    extends DirectionMovement
  case object Down  extends DirectionMovement
  case object Left  extends DirectionMovement
  case object Right extends DirectionMovement

  sealed trait Command
  case object Help extends Command
  case object Show extends Command
  case object Move extends Command
  case object Quit extends Command

  def parseDirection(direction: String): Option[DirectionMovement] =
    direction match {
      case "up"    => Some(Up)
      case "down"  => Some(Down)
      case "right" => Some(Right)
      case "left"  => Some(Left)
      case _ => {
        println("Unknown direction")
        None
      }
    }

  def parseCommand(command: String): Option[Command] =
    command match {
      case "help" => Some(Help)
      case "show" => Some(Show)
      case "move" => Some(Move)
      case "quit" => Some(Quit)
      case _ =>
        println("Unknown command")
        gameStep()
        None
    }

  def readCommand(): Array[String] =
    readLine().trim.toLowerCase.split("\\s+")

  def executeCommand(command: Command, input: Array[String]): Unit =
    command match {
      case Help => {
        printHelp()
        gameStep()
      }
      case Show => {
        printWorld()
        gameStep()
      }
      case Move => {
        move(input)
        gameStep()
      }
      case Quit => printQuit()
    }

  object Logic {

    val enter = System.getProperty("line.separator")

    var world: GameWorld = null

    def initWorld(): Unit = {
      world = GameWorld(Player.begin(askName()), Field.mk20x20)
      println("Use commands to play")
    }

    def askName(): String = {
      println("What is your name?")
      val name = readLine().trim
      println(s"Hello, $name, welcome to the game!")
      name
    }

    def gameLoop(): Unit =
      gameStep()

    def gameStep(): Unit = {

      val line = readLine()

      if (line.length > 0) {
        val words = line.trim.toLowerCase.split("\\s+")

        for {
          command <- parseCommand(words(0))
        } executeCommand(command, words)
      }
    }

    def movePlayer(direction: DirectionMovement): Unit =
      direction match {
        case Up =>
          if (world.player.y == 0) println("Invalid direction")
          else {
            world = world.copy(player = world.player.copy(y = world.player.y - 1))
          }
        case Down =>
          if (world.player.y >= 19) println("Invalid direction")
          else {
            world = world.copy(player = world.player.copy(y = world.player.y + 1))
          }
        case Left =>
          if (world.player.x == 0) println("Invalid direction")
          else {
            world = world.copy(player = world.player.copy(x = world.player.x - 1))
          }
        case Right =>
          if (world.player.x >= 19) println("Invalid direction")
          else {
            world = world.copy(player = world.player.copy(x = world.player.x + 1))
          }
      }

    def move(array: Array[String]): Unit =
      if (array.length == 2) {
        for {
          direction <- parseDirection(array(1))
        } movePlayer(direction)

      } else {
        println("Missing direction")
      }

    def printWorld(): Unit =
      println(renderWorld)

    def printQuit(): Unit =
      println(s"Bye bye ${world.player.name}!")

    def printHelp(): Unit = {
      val value =
        s"""|
            |Valid commands:
            |
            | help
            | show
            | move <up|down|left|right>
            | quit
            |""".stripMargin
      println(value)
    }

    def renderWorld: String = {
      val x       = world.player.x
      val y       = world.player.y
      val grid    = world.field.grid
      val updated = grid.updated(x, grid(x).updated(y, "x"))

      enter + updated.map(_.mkString(" ")).mkString(enter) + enter
    }
  }

  def run(): Unit = {
    initWorld()
    gameLoop()
  }
}
