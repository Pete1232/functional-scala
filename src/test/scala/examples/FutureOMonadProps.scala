package examples

import examples.Types.FutureOption
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}
import org.scalatest.{AsyncWordSpec, MustMatchers}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

abstract class FutureOMonadProps[A, B, C](implicit arb: Arbitrary[A],
                                          arbFunction: Arbitrary[(A) => FutureOption[B]],
                                          arbFunction2: Arbitrary[(B) => FutureOption[C]])
  extends Properties("FutureOMonad") {
  property("flatMap must be associative") = forAll { (f: (A) => FutureOption[B], g: (B) => FutureOption[C], testValue: A) =>

    val lhs = // a.flatMap(f).flatMap(g)
      FutureO.flatMap(
        FutureO.flatMap(FutureO.unit(testValue))(a => f(a))
      )(b => g(b))

    val rhs = // a.flatMap(f.flatMap(g))
      FutureO.flatMap(FutureO.unit(testValue))(a =>
        FutureO.flatMap(f(a))(b =>
          g(b)))
    compareFutureO(lhs, rhs)
  }

  property("unit must be an identity under composition with flatMap") = forAll { (testValue: A) =>
    //a.flatMap(unit) == a
    compareFutureO(
      FutureO.flatMap(FutureO.unit(testValue))(a => FutureO.unit(a)),
      FutureO.unit(testValue)
    )
  }

  property("unit must be an identity under composition with flatMap and any function") = forAll { (f: (A) => FutureOption[B], testValue: A) =>
    //a.flatMap(f) == f(a)
    compareFutureO(
      FutureO.flatMap(FutureO.unit(testValue))(a => f(a)),
      f(testValue)
    )
  }

  private def compareFutureO[T](lhs: FutureOption[T], rhs: FutureOption[T]): Boolean = {
    Await.ready(lhs, Duration.Inf)
    Await.ready(rhs, Duration.Inf)
    (lhs.value, rhs.value) match {
      case (Some(Success(l)), Some(Success(r))) => l == r
      case (Some(Failure(l)), Some(Failure(r))) => l.getMessage == r.getMessage
      case _ => lhs == rhs //must fail - note will never be None (reserved for Future not completed)
    }
  }
}

// note for work: replace these (random) types with a test for each model we use
//object FutureOMonadStringProps extends FutureOMonadProps[String, Int, Boolean]

class FutureOMonadStringSpec extends AsyncWordSpec with MustMatchers {
  "FutureOMonad" must {
    "define map and flatMap methods" in {
      val alice = FutureO.unit("Alice")
      val bob = FutureO.unit("Bob")

      FutureO.flatMap(alice)(a =>
        FutureO.map(bob)(b =>
          a + " and " + b
        )
      ).map(_ mustBe Some("Alice and Bob"))
    }
  }
  it must {
    "function correctly in a for-comprehension" in {
      val alice: FutureO[String] = FutureO("Alice")
      val bob: FutureO[String] = FutureO("Bob")

      val result = for {
        a <- alice
        b <- bob
      } yield a + " and " + b

      result.map(_ mustBe Some("Alice and Bob"))
    }
  }
}
