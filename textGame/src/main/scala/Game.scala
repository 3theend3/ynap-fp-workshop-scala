package textGame

import scala.io.StdIn._

class Game {
  import Domain._
  import Logic._

  object Domain {

    sealed trait Box

    case object EmptyBox extends Box {
      override def toString: String = "-"
    }

    case object PlayerBox extends Box {
      override def toString: String = "x"
    }

    case class Position(x: Int, y: Int)

    case class Player(name: String, position: Position)

    object Player {
      def begin(name: String) = Player(name, Position(0, 0))
    }

    case class Field(grid: Vector[Vector[Box]])

    object Field {
      def mk20x20 =
        Field(Vector.fill(20, 20)(EmptyBox))
    }

    case class GameWorld(player: Player, field: Field)
  }

  object Logic {

    val enter = System.getProperty("line.separator")

    sealed trait DirectionMovement
    case object Up    extends DirectionMovement
    case object Down  extends DirectionMovement
    case object Left  extends DirectionMovement
    case object Right extends DirectionMovement

    sealed trait Command
    case object Help                              extends Command
    case object Show                              extends Command
    case class Move(direction: DirectionMovement) extends Command
    case object Quit                              extends Command

    sealed trait ParseResult
    case class Valid(command: Command) extends ParseResult
    case object UnknownCommand         extends ParseResult
    case object UnknownDirection       extends ParseResult
    case object MissingDirection       extends ParseResult
    case object Empty                  extends ParseResult

    def parseCommand(line: String): ParseResult =
      if (line.length > 0) {
        val words = line.trim.toLowerCase.split("\\s+")
        words(0) match {
          case "show" => Valid(Show)
          case "move" =>
            if (words.length == 2) {
              words(1) match {
                case "up"    => Valid(Move(Up))
                case "down"  => Valid(Move(Down))
                case "right" => Valid(Move(Right))
                case "left"  => Valid(Move(Left))
                case _       => UnknownDirection
              }
            } else {
              MissingDirection
            }
          case "quit" => Valid(Quit)
          case "help" => Valid(Help)
          case _ =>
            UnknownCommand
        }
      } else Empty

    def readCommand(): Array[String] =
      readLine().trim.toLowerCase.split("\\s+")

    def executeCommand(command: Command, world: GameWorld): (GameDecision, GameWorld) =
      command match {
        case Help            => (ContinuePrinting(possibleCommands()), world)
        case Show            => (ContinuePrinting(printWorld(world)), world)
        case Move(direction) => move(direction, world)
        case Quit            => (Stop(printQuit(world)), world)
      }

    def initWorld(): GameWorld = {
      val world = GameWorld(Player.begin(askName()), Field.mk20x20)
      world
    }

    def askName(): String = {
      println("What is your name?")
      val name = readLine().trim
      println(s"Hello, $name, welcome to the game!")
      name
    }

    def gameLoop(world: GameWorld): Unit =
      gameStep(world) match {
        case (ContinuePrinting(value), gameWorld) => println(value); gameLoop(gameWorld)
        case (Continue, gameWorld)                => gameLoop(gameWorld)
        case (Stop(x), _)                         => println(x)
      }

    sealed trait GameDecision
    case object Continue                        extends GameDecision
    case class ContinuePrinting(string: String) extends GameDecision
    case class Stop(string: String)             extends GameDecision

    def gameStep(world: GameWorld): (GameDecision, GameWorld) = {
      val line = readLine()
      parseCommand(line) match {
        case Valid(command)   => executeCommand(command, world)
        case UnknownCommand   => (ContinuePrinting("Unknown command"), world)
        case UnknownDirection => (ContinuePrinting("Unknown direction"), world)
        case MissingDirection => (ContinuePrinting("Missing direction"), world)
        case Empty            => (Continue, world)
      }
    }

    def movePlayer(direction: DirectionMovement, world: GameWorld): GameWorld =
      direction match {
        case Up =>
          world.copy(player = world.player.copy(position = world.player.position.copy(y = world.player.position.y - 1)))
        case Down =>
          world.copy(player = world.player.copy(position = world.player.position.copy(y = world.player.position.y + 1)))
        case Left =>
          world.copy(player = world.player.copy(position = world.player.position.copy(x = world.player.position.x - 1)))
        case Right =>
          world.copy(player = world.player.copy(position = world.player.position.copy(x = world.player.position.x + 1)))
      }

    def validateMovement(world: GameWorld): Option[GameWorld] =
      if (world.player.position.x == world.field.grid.size
          || world.player.position.x < 0
          || world.player.position.y == world.field.grid.size
          || world.player.position.y < 0) {
        None
      } else Some(world)

    def move(direction: DirectionMovement, world: GameWorld): (GameDecision, GameWorld) =
      validateMovement(movePlayer(direction, world)) match {
        case Some(gameWorld) => (Continue, gameWorld)
        case None            => (ContinuePrinting("Invalid direction"), world)
      }

    def printWorld(world: GameWorld): String =
      renderWorld(world)

    def printQuit(world: GameWorld): String =
      s"Bye bye ${world.player.name}!"

    def possibleCommands(): String =
      s"""|
          |Valid commands:
          |
          | help
          | show
          | move <up|down|left|right>
          | quit
          |""".stripMargin

    def renderWorld(world: GameWorld): String = {
      val x       = world.player.position.x
      val y       = world.player.position.y
      val grid    = world.field.grid
      val updated = grid.updated(x, grid(x).updated(y, PlayerBox))

      enter + updated.map(_.mkString(" ")).mkString(enter) + enter
    }
  }

  def run(): Unit = {
    val world = initWorld()
    println("Use commands to play")

    gameLoop(world)
  }
}
