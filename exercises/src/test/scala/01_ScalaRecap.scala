package exercises

import minitest._

/*
 * Our most used Scala features:
 * - Case class
 * - Companion Object
 * - Apply function
 * - Type parameter
 * - Trait as interface/mixin
 * - Implicit parameter
 * - Extension class
 * - Pattern match
 */

object ScalaRecap extends SimpleTestSuite {

  /*
   * TODO: one test at a time,
   *       read `ignore()` description
   *       uncomment the code,
   *       and add the code to get a green test
   *
   * ADD YOUR CODE HERE INSIDE THE OBJECT
   */

  case class Person(name: String, age: Int) {
    def toMap: Map[String, String] =
      Map(
      "name" -> this.name,
      "age"  -> this.age.toString
    )


    def makeYounger(implicit n: Int): Person = copy(age = age - n)


    def eat[A <: Fruit](fruit: A) : String = fruit.eatenBy(name)

    def apply(param: String) : String = {
      param+" mi chiamo "+ name +"!"
    }
  }
  object Person{
    def create(params: String) : Person = {
      val result = params.split(", ")
      Person(result(0),result(1).toInt)
    }
    def apply(par: String): Person = create(par)
  }

  trait Fruit{
    def stringify: String
    def eatenBy(person: String): String = {
      person + " ate " + this.stringify
    }
  }
  case class Apple() extends Fruit {
    def stringify: String = "an apple"
  }
  case class Banana() extends Fruit {
    def stringify: String = "a banana"
  }

  test("define case class") {
  //  ignore("define a case class w/ two fields: name and age")
     val result = Person("foo", 56)
     assertEquals(result, Person("foo", 56))
  }

  test("define the case class's companion object") {
    //ignore("define a companion object w/ a custom factory method")
     val result = Person.create("foo, 56")
     assertEquals(result, Person("foo", 56))
  }

  test("case class apply") {
    //ignore("add an apply function on Person class")
     val result = Person("foo", 56)("Ciao,")
     assertEquals(result, "Ciao, mi chiamo foo!")
  }

  test("companion object apply") {
    //ignore("add an apply function on Person object")
     val result = Person.apply("foo, 56").apply("Ciao,")
     assertEquals(result, "Ciao, mi chiamo foo!")
  }

  test("case class update") {
    //ignore("add 100 years to the person")
     val p      = Person("foo", 56)
     val result = p.copy(age = p.age + 100)
     assertEquals(result.age, 156)
  }

  test("trait as interface") {
    //ignore("add a Fruit trait w/ two subclass Apple and Banana")
     assert(Apple().isInstanceOf[Fruit])
     assert(Banana().isInstanceOf[Fruit])

    //ignore("add empty stringify function to Fruit and implement it in Apple and Banana")
     assertEquals(Apple().stringify, "an apple")
     assertEquals(Banana().stringify, "a banana")
  }

  test("trait as mixin") {
    //ignore("add function w/ default implementation to Fruit trait")
     assertEquals(Apple().eatenBy("foo"), "foo ate an apple")
     assertEquals(Banana().eatenBy("bar"), "bar ate a banana")
  }

  test("type parameter") {
    //ignore("add generic function to Person class")
     val p      = Person("foo", 56)
     val result = p.eat(Apple())
     assertEquals(result, "foo ate an apple")
  }

  test("implicit parameter") {
    //ignore("add a function w/ an implicit parameter to the Person class")
     implicit val years = 30
     val p              = Person("foo", 56)
     val result         = p.makeYounger
     assertEquals(result.age, 26)
  }

  test("extension class") {
    //ignore("add a function to Person via implicit class extension")
     val p      = Person("foo", 56)
     val result = p.toMap
     assertEquals(result, Map("name" -> "foo", "age" -> "56"))
  }

  def isFake(p: Any): Boolean =
    p match {
      case Person("foo", _) => true
      case Person("bar", _) => true
      case Person(_, age) if age < 0 => true
      case _ => false
    }

  test("pattern match") {
    //ignore("add a function to Person object that...")
    //ignore("...return true when name is foo")
    //ignore("...return true when name is bar")
    //ignore("...return true when age is negative")
    //ignore("...otherwise return false")
     assert(isFake(Person("foo", 10)))
     assert(isFake(Person("bar", 10)))
     assert(isFake(Person("matte", -10)))
     assert(!isFake(Person("matte", 10)))
  }
}
