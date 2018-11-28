package exercises

import minitest._

/*
 * Functions can't always return a value.
 * In this scenario they are called: partial functions.
 * We can convert them into total functions
 * with the introduction of effects.
 *
 *  f:  InType => Effect[OutType]
 */

object MaybeTests extends SimpleTestSuite {

  /*
   * TODO: remove all nulls
   */
  sealed trait Result[+A]
  case class Yes[A](value: A) extends Result[A]
  case class No()             extends Result[Nothing]

  case class Qty(value: Int)

  def toQty(value: String): Result[Qty] =
    if (value.matches("^[0-9]+$"))
      Yes(Qty(value.toInt))
    else No()

  test("valid qty") {
    assertEquals(toQty("100"), Some(Qty(100)))
  }

  test("invalid qty") {
    assertEquals(toQty("asd"), No())
    assertEquals(toQty("1 0 0"), No())
    assertEquals(toQty(""), No())
    assertEquals(toQty("-10"), No())
  }
}
