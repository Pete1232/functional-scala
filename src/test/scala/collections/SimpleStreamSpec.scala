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
}
