package collections

import org.scalatest.{FlatSpec, MustMatchers}
import SimpleStream._

class SimpleStreamSpec extends FlatSpec with MustMatchers {
  "SimpleStream#apply" must "return Cons when called with values" in {
    apply(5, 6, 7) match {
      case Cons(_,_) =>
      case _ => fail("The stream must not be empty")
    }
  }
  it must "build a stream containing the correct values" in {
    apply(5, 6, 7) match {
      case Cons(h,t) => {
        h() mustBe 5
        t() match {
          case Cons(h,t) => {
            h() mustBe 6
            t() match {
              case Cons(h, t) => {
                h() mustBe 7
                t() mustBe Empty
              }
              case Empty => fail("The third stream must not be Empty")
            }
          }
          case Empty => fail("The second stream must not be Empty")
        }
      }
      case Empty => fail("The first stream must not be Empty")
    }
  }

  "SimpleStream#headOption" must "return the head of the stream if it exists" in {
    apply(5, 6, 7).headOption mustBe Some(5)
  }
  it must "return None otherwise" in {
    apply().headOption mustBe None
  }

  "SimpleStream#tail" must "return the tail of the stream" in {
    SimpleStream(5, 6, 7).tail.toList mustBe List(6, 7)
    SimpleStream(5).tail mustBe Empty
    Empty.tail mustBe Empty
  }

  "SimpleStream#toList" must "convert the stream to a list" in {
    SimpleStream(5, 6, 7).toList mustBe List(5, 6, 7)
  }

  "SimpleStream#take" must "return the first n elements of a SimpleStream" in {
    SimpleStream(5, 6, 7).take(1).toList mustBe List(5)
    SimpleStream(5, 6, 7).take(2).toList mustBe List(5, 6)
    SimpleStream(5, 6, 7).take(3).toList mustBe List(5, 6, 7)
    SimpleStream(5, 6, 7).take(0) mustBe Empty
  }

  "SimpleStream#drop" must "drop the first n elements of a SimpleStream" in {
    SimpleStream(5, 6, 7).drop(1).toList mustBe List(6, 7)
    SimpleStream(5, 6, 7).drop(2).toList mustBe List(7)
    SimpleStream(5, 6, 7).drop(3) mustBe Empty
    SimpleStream(5, 6, 7).drop(0).toList mustBe List(5, 6, 7)
  }

  "SimpleStream#takeWhile" must "take all values until a predicate is not satisfied" in {
    SimpleStream(5, 6, 7).takeWhile(_ != 7).toList mustBe List(5, 6)
    SimpleStream(5, 6, 7).takeWhile(_ => false) mustBe Empty
  }

  "SimpleStream#exists" must "return true if any value of the stream satisfies the predicate" in {
    SimpleStream(5, 6, 7).exists(_ == 7) mustBe true
  }
  it must "return false otherwise" in {
    SimpleStream(5, 6, 7).exists(_ == 8) mustBe false
    Empty.exists(_ == 7) mustBe false
  }

  "SimpleStream#foldRight" must "fold the stream according to the given function" in {
    SimpleStream(5, 6, 7).foldRight(1)((x, y) => x + y) mustBe 19
  }

  "SimpleStream#forAll" must "return true if all elements of a stream match a predicate" in {
    SimpleStream(5, 10, 15).forAll(_ % 5 == 0) mustBe true
  }
  it must "return false otherwise" in {
    SimpleStream(5, 6, 15).forAll(_ % 5 == 0) mustBe false
  }

  "SimpleStream#map" must "convert the elements of the stream according to the given function" in {
    SimpleStream(5, 6, 7).map(_ + 10).toList mustBe List(15, 16, 17)
  }

  "SimpleStream#append" must "append the given element to the end of the stream" in {
    SimpleStream(5, 6).append(7).toList mustBe List(5, 6, 7)
  }

  "SimpleStream#flatMap" must "map each element to a stream, and append to the result stream" in {
    SimpleStream(5, 6, 7, 8, 9).flatMap(a => SimpleStream(a, a)).toList mustBe List(5, 5, 6, 6, 7, 7, 8, 8, 9, 9)
  }
}
