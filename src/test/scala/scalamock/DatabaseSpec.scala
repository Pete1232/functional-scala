package scalamock

import org.mockito.ArgumentMatchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{MustMatchers, OneInstancePerTest, WordSpec}
import org.mockito.Mockito.{times, when}
import org.scalacheck.{Gen, Prop}
import org.scalatest.prop.Checkers

case class ExampleModel(id: String, name: String, age: Int)

// defaults to false
trait Database {
  def insert(s: String): String

  def find(s: String): String

  def find(s: String, t: String): String

  def findList[T](x: List[T]): String

  def findAll(xs: String*): String

  def makeModel(id: String, name: String, age: Int): ExampleModel
}

// TODO async testing once bug in ScalaMock is fixed (hopefully release 3.5.0 but maybe not until 4.0.0)
// TODO mock case class (release 3.5.0)
// TODO Detailed logging (re-added release 3.5.0)
class DatabaseSpec extends WordSpec with MustMatchers with MockFactory with OneInstancePerTest with Checkers {

  // global mock using OneInstancePerTest
  val dbScalaMock = stub[Database]

  val dbMockitoMock = MockitoSugar.mock[Database]

  "mocking out a method in a trait with ScalaMock" in {
    dbScalaMock.insert _ when * returns "apple"

    dbScalaMock.insert("test") mustBe "apple"
  }
  "mocking out a method in a trait with Mockito" in {
    when(dbMockitoMock.insert(ArgumentMatchers.any())) thenReturn "apple"

    dbMockitoMock.insert("test") mustBe "apple"
  }
  "mocking out a method in a trait with specific parameters with ScalaMock" in {
    dbScalaMock.insert _ when "apple" returns "apple"

    dbScalaMock.insert("apple") mustBe "apple"
    dbScalaMock.insert("test") mustBe null
  }
  "mocking out a method in a trait with specific parameters with Mockito" in {
    when(dbMockitoMock.insert("apple")) thenReturn "apple"

    dbMockitoMock.insert("apple") mustBe "apple"
    dbMockitoMock.insert("test") mustBe null
  }
  "mocking out an overloaded method in a trait with ScalaMock" in {
    (dbScalaMock.find(_: String)) when * returns "apple"
    (dbScalaMock.find(_: String, _: String)) when(*, *) returns "banana"

    dbScalaMock.find("test") mustBe "apple"
    dbScalaMock.find("test", "test") mustBe "banana"
  }
  "mocking out an overloaded method in a trait with Mockito" in {
    when(dbMockitoMock.find(ArgumentMatchers.any())) thenReturn "apple"
    when(dbMockitoMock.find(ArgumentMatchers.any(), ArgumentMatchers.any())) thenReturn "banana"

    dbMockitoMock.find("test") mustBe "apple"
    dbMockitoMock.find("test", "test") mustBe "banana"
  }
  "mocking out a polymorphic method in a trait with ScalaMock" in {
    dbScalaMock.findList[Int] _ when List(1, 2, 3) returns "apple"
    dbScalaMock.findList[String] _ when List("apple") returns "banana"

    dbScalaMock.findList(List(1, 2, 3)) mustBe "apple"
    dbScalaMock.findList(List("apple")) mustBe "banana"
  }
  "mocking out a polymorphic method in a trait with Mockito" in {
    when(dbMockitoMock.findList(List(1, 2, 3))) thenReturn "apple"
    when(dbMockitoMock.findList(List("apple"))) thenReturn "banana"

    dbMockitoMock.findList(List(1, 2, 3)) mustBe "apple"
    dbMockitoMock.findList(List("apple")) mustBe "banana"
  }
  "mocking errors with ScalaMock" ignore {
    // record then verify (or mockito) style
    dbScalaMock.insert _ when * returns "apple"

    dbScalaMock.insert("apple")

    dbScalaMock.insert _ verify * never()


    // could also use expectation first style (conflicts with MockitoSugar)
    // all on one line
    //  val dbScalaMock = mock[Database]
    //  dbScalaMock.insert _ expects * returning "apple" once
  }
  //    Expected:
  //      inAnyOrder {
  //        <stub-1> Database.insert(*) any number of times (called once)
  //        <stub-1> Database.insert(*) never (called once - UNSATISFIED)
  //        }
  //
  //        Actual:
  //        <stub-1> Database.insert(apple)

  "mocking errors with Mockito" ignore {
    import org.mockito.Mockito.verify

    when(dbMockitoMock.insert(ArgumentMatchers.any())) thenReturn "apple"

    dbMockitoMock.insert("apple")

    verify(dbMockitoMock, times(0)).insert("apple")
  }
  //    Never wanted here:
  //      -> at scalamock.DatabaseSpec$$anonfun$5$$anonfun$apply$mcV$sp$10.apply(DatabaseSpec.scala:115)
  //    But invoked here:
  //      -> at scalamock.DatabaseSpec$$anonfun$5$$anonfun$apply$mcV$sp$10.apply(DatabaseSpec.scala:113)
  "verifying ordering with ScalaMock" in {
    dbScalaMock.insert _ when * returns "apple"
    (dbScalaMock.find(_: String)) when * returns "banana"

    // defaults to inAnyOrder
    // note inAnyOrder and inSequence can be nested for more complicated orderings
    inSequence {
      (dbScalaMock.find(_: String)) verify "red" once()
      dbScalaMock.insert _ verify "yellow" twice()
      inAnyOrder {
        (dbScalaMock.find(_: String)) verify "green" once()
        (dbScalaMock.find(_: String)) verify "purple" once()
      }
    }

    dbScalaMock.find("red") mustBe "banana"
    dbScalaMock.insert("yellow") mustBe "apple"
    dbScalaMock.insert("yellow") mustBe "apple"
    // different order to how they are listed above
    dbScalaMock.find("purple") mustBe "banana"
    dbScalaMock.find("green") mustBe "banana"

    // expectation first - skip the set-up step
    //  inSequence{
    //    dbScalaMock.insert _ expects "red" returning "apple" once()
    //    dbScalaMock.insert _ expects "yellow" returning "banana" twice()
    //    dbScalaMock.insert _ expects "green" returning "apple" once()
    // }
  }
  //      Expected:
  //        inAnyOrder {
  //          <stub-1> Database.insert(*) any number of times (called twice)
  //          <stub-1> Database.find(*) any number of times (called twice)
  //          inSequence {
  //          <stub-1> Database.find(red) once (called once)
  //          <stub-1> Database.insert(yellow) twice (called once - UNSATISFIED)
  //          <stub-1> Database.find(green) once (called once)
  //          }
  //          }
  //
  //          Actual:
  //          <stub-1> Database.find(red)
  //          <stub-1> Database.insert(yellow)
  //          <stub-1> Database.find(green)
  //          <stub-1> Database.insert(yellow)
  "verifying ordering with Mockito" in {
    when(dbMockitoMock.insert(ArgumentMatchers.any())) thenReturn "apple"
    when(dbMockitoMock.find(ArgumentMatchers.any())) thenReturn "banana"

    val order = org.mockito.Mockito.inOrder(dbMockitoMock)

    dbMockitoMock.find("red") mustBe "banana"
    dbMockitoMock.insert("yellow") mustBe "apple"
    dbMockitoMock.insert("yellow") mustBe "apple"
    dbMockitoMock.find("green") mustBe "banana"

    order.verify(dbMockitoMock).find("red")
    order.verify(dbMockitoMock, times(2)).insert("yellow")
    order.verify(dbMockitoMock).find("green")
  }
  //  Verification in order failure
  //    Wanted but not invoked:
  //    database.find("green");
  //  -> at scalamock.DatabaseSpec$$anonfun$7.apply(DatabaseSpec.scala:170)
  //  Wanted anywhere AFTER following interaction:
  //    database.insert("yellow");
  "returning dynamic values (call handlers) with ScalaMock" in {
    check (
      Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr, Gen.choose(0, 100)) { (generatedId, generatedName, generatedAge) =>
        dbScalaMock.makeModel _ when(*, *, *) onCall ((id: String, name: String, age: Int) => ExampleModel(id, name, age))

        dbScalaMock.makeModel(generatedId, generatedName, generatedAge) == ExampleModel(generatedId, generatedName, generatedAge)
      }
    )
  }
  // cannot find an example of this with Mockito???
  "predicate matching with ScalaMock" in {
    // Scala _ syntax works
    (dbScalaMock.find (_: String, _: String)) when where { _.length > _.length} returns "apple"
    (dbScalaMock.find (_: String, _: String)) when where { (s, t) => s.length < t.length} returns "banana"

    dbScalaMock.find("nine", "ten") mustBe "apple"
    dbScalaMock.find("ten", "eleven") mustBe "banana"
    dbScalaMock.find("four", "five") mustBe null
  }
  // again couldn't find an example of doing this with Mockito???
  "repeated parameters with ScalaMock" in {
    dbScalaMock.findAll _ when Seq("foo") returns "bar"
    dbScalaMock.findAll _ when Seq("Hello", ", ", "World") returns "!"

    dbScalaMock.findAll("foo") mustBe "bar"
    dbScalaMock.findAll("Hello", ", ", "World") mustBe "!"
  }
  "repeated parameters with Mockito" in {
    when(dbMockitoMock.findAll("foo")) thenReturn "bar"
    when(dbMockitoMock.findAll("Hello", ", ", "World")) thenReturn "!"

    dbMockitoMock.findAll("foo") mustBe "bar"
    dbMockitoMock.findAll("Hello", ", ", "World") mustBe "!"
  }
}
