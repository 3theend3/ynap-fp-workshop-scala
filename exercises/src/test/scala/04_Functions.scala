package exercises

import minitest._

/*
 * ADT models data while Function models behaviour.
 * A function is simply something that accepts an input value
 * and produces an output value.
 * In more accademic terms it connects a Domain to a Codomain.
 * Functions are described/documented by it's type definition.
 *
 *  f:  InType => OutType
 */

object FunctionsTests extends SimpleTestSuite {

  /*
   * TODO: implements functions marked with `???`
   */

  val asString: Double => String = in => in.toString

  val parseString: String => Int = in => in.toInt

  val reciprocal: Int => Double = in => 1.0 / in

  val reciprocalString: String => String =
    //asString(reciprocal(parseString(in)))
    //asString compose reciprocal compose parseString
    parseString.andThen(reciprocal).andThen(asString)

  def andThen[A, B, C](f: A => B, g: B => C): A => C =
    in => g(f(in))

  //test("from string to string throught reciprocal") {
  //  ignore("use existing function to compute a reciprocal in string")
  //  assertEquals(reciprocalString("42"), "0.023809523809523808")
  //}
  case class Rover(direction: CardinalDirection, position: Position)
  case class Planet(xLength: Int, yLength: Int, rover: Rover)
  case class Position(x: Int, y: Int)

  sealed trait CardinalDirection
  case object N extends CardinalDirection
  case object E extends CardinalDirection
  case object W extends CardinalDirection
  case object S extends CardinalDirection

  def turnRight(roverDirection: CardinalDirection): CardinalDirection =
    roverDirection match {
      case S => W
      case W => N
      case E => S
      case N => E
    }

  def turnLeft(roverDirection: CardinalDirection): CardinalDirection =
    roverDirection match {
      case S => E
      case W => S
      case E => N
      case N => W
    }

  sealed trait TurnSideCommand
  case object Left  extends TurnSideCommand
  case object Right extends TurnSideCommand

  def turn(planet: Planet, turnSide: TurnSideCommand): Planet =
    turnSide match {
      case Left  => planet.copy(rover = planet.rover.copy(direction = turnLeft(planet.rover.direction)))
      case Right => planet.copy(rover = planet.rover.copy(direction = turnRight(planet.rover.direction)))
    }

  sealed trait MoveCommand
  case object Forward  extends MoveCommand
  case object Backward extends MoveCommand

  val positionRecalculation: Planet => Position = planet =>
    planet.rover.position.copy(
      y =
        if (planet.rover.position.y == planet.yLength) 0
        else if (planet.rover.position.y < 0) planet.yLength - 1
        else planet.rover.position.y,
      x =
        if (planet.rover.position.x == planet.xLength) 0
        else if (planet.rover.position.x < 0) planet.xLength - 1
        else planet.rover.position.x
  )

  def forward(planet: Planet): Planet =
    planet.copy(rover = planet.rover.copy(position = planet.rover.direction match {
      case S => planet.rover.position.copy(y = planet.rover.position.y - 1)
      case N => planet.rover.position.copy(y = planet.rover.position.y + 1)
      case E => planet.rover.position.copy(x = planet.rover.position.x + 1)
      case W => planet.rover.position.copy(x = planet.rover.position.x - 1)
    }))

  val forwardAndRecalculatePosition: Planet => Position = (forward _).andThen(positionRecalculation)

  def backward(direction: CardinalDirection, position: Position): Position =
    direction match {
      case N => position.copy(y = position.y - 1)
      case S => position.copy(y = position.y + 1)
      case E => position.copy(x = position.x - 1)
      case W => position.copy(x = position.x + 1)
    }

  def move(planet: Planet, direction: MoveCommand): Planet =
    direction match {
      case Forward =>
        planet.copy(
          rover = planet.rover
            .copy(position = forwardAndRecalculatePosition(planet))
        )
      case Backward =>
        planet.copy(
          rover = planet.rover
            .copy(
              position = positionRecalculation(
                planet
                  .copy(rover = planet.rover.copy(position = backward(planet.rover.direction, planet.rover.position)))
              )
            )
        )

    }
  //list commands:
  // l: turn left
  // r: turn right
  // f: move forward
  // b: move backward
  def executeCommand(planet: Planet, command: Char): Planet =
    command match {
      case 'l' => turn(planet, Left)
      case 'r' => turn(planet, Right)
      case 'f' => move(planet, Forward)
      case 'b' => move(planet, Backward)
      case _   => planet
    }

  var rover     = Rover(N, Position(0, 0))
  var planet    = Planet(10, 10, rover)
  var commands0 = "ffflb"
  var commands1 = "frfrfrf"
  var commands2 = "b"
  var commands3 = "ffffffffff"
  var commands4 = "llbbbbbbbbbb"
  var commands5 = "lf"
  var commands6 = "rffffffffff"

  test("Rover position") {
    assertEquals(commands0.toList.foldLeft(planet)(executeCommand).rover.position, Position(1, 3))
    assertEquals(commands1.toList.foldLeft(planet)(executeCommand).rover.position, Position(0, 0))
    assertEquals(commands2.toList.foldLeft(planet)(executeCommand).rover.position, Position(0, 9))
    assertEquals(commands3.toList.foldLeft(planet)(executeCommand).rover.position, Position(0, 0))
    assertEquals(commands4.toList.foldLeft(planet)(executeCommand).rover.position, Position(0, 0))
    assertEquals(commands5.toList.foldLeft(planet)(executeCommand).rover.position, Position(9, 0))
    assertEquals(commands6.toList.foldLeft(planet)(executeCommand).rover.position, Position(0, 0))
  }
}
