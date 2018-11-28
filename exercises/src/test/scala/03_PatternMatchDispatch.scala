package exercises

import minitest._

/*
 * Pattern match enable the structural recurtion
 * a fancy name to express a way to distch logic
 * by type and data. It goes hand in hand with ADT
 * specially Sum Type. Think, how we can implement
 * some special logic `foo` for an "exclusive-or"
 * data type?
 */

object PatternMatchDispatch extends SimpleTestSuite {

  /*
   * TODO: rewrite the dispatch logic
   *       from polymorphic dispatch (a fundamental OOP technique)
   *       to pattern match dispatch.
   *       Keep tests green.
   */

  sealed trait Direction {
  }
  case object N extends Direction {
  }
  case object E extends Direction {
  }
  case object W extends Direction {
  }
  case object S extends Direction {
  }

  def turnRight(direction: Direction): Direction =
    direction match {
      case S => W
      case W => N
      case E => S
      case N => E
    }

  def turnLeft(direction: Direction): Direction =
    direction match {
      case S => E
      case W => S
      case E => N
      case N => W
    }

  test("turn right") {
    assertEquals(turnRight(N), E)
    assertEquals(turnRight(E), S)
    assertEquals(turnRight(S), W)
    assertEquals(turnRight(W), N)
  }

  test("turn left") {
    assertEquals(turnLeft(N), W)
    assertEquals(turnLeft(W), S)
    assertEquals(turnLeft(S), E)
    assertEquals(turnLeft(E), N)
  }
}
