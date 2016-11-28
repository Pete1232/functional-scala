package examples

import examples.Types.FutureO
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

abstract class FutureOMonadProps[A, B, C](implicit arb: Arbitrary[A],
                                          arbFunction: Arbitrary[(A) => FutureO[B]],
                                          arbFunction2: Arbitrary[(B) => FutureO[C]])
  extends Properties("FutureOMonad") {
  property("flatMap must be associative") = forAll { (f: (A) => FutureO[B], g: (B) => FutureO[C], testValue: A) =>

    val lhs = // a.flatMap(f).flatMap(g)
      FutureOMonad.flatMap(
        FutureOMonad.flatMap(FutureOMonad.unit(testValue))(a => f(a))
      )(b => g(b))

    val rhs = // a.flatMap(f.flatMap(g))
      FutureOMonad.flatMap(FutureOMonad.unit(testValue))(a =>
        FutureOMonad.flatMap(f(a))(b =>
          g(b)))
    compareFutureO(lhs, rhs)
  }

  property("unit must be an identity under composition with flatMap") = forAll { (testValue: A) =>
    //a.flatMap(unit) == a
    compareFutureO(
      FutureOMonad.flatMap(FutureOMonad.unit(testValue))(a => FutureOMonad.unit(a)),
      FutureOMonad.unit(testValue)
    )
  }

  property("unit must be an identity under composition with flatMap and any function") = forAll { (f: (A) => FutureO[B], testValue: A) =>
    //a.flatMap(f) == f(a)
    compareFutureO(
      FutureOMonad.flatMap(FutureOMonad.unit(testValue))(a => f(a)),
      f(testValue)
    )
  }

  private def compareFutureO[T](lhs: FutureO[T], rhs: FutureO[T]): Boolean = {
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
object FutureOMonadStringProps extends FutureOMonadProps[String, Int, Boolean]
