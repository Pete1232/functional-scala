package functional_state

import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, MustMatchers}
import org.mockito.Mockito._

class RNGSpec extends FlatSpec with MustMatchers with MockitoSugar {
  val mockSimpleRNG = mock[SimpleRNG]
  when(mockSimpleRNG.nextInt).thenReturn((Int.MinValue, SimpleRNG(42)))

  "SimpleRNG#nextInt" must "return (16159453, SimpleRNG(1059025964525)) when seeded with 42" in {
    val simpleRNG = SimpleRNG(42)
    simpleRNG.nextInt mustBe (16159453, SimpleRNG(1059025964525L))
    simpleRNG.nextInt mustBe (16159453, SimpleRNG(1059025964525L))
    simpleRNG.nextInt mustBe (16159453, SimpleRNG(1059025964525L))
  }
  it must "return (-1281479697, SimpleRNG(197491923327988)) when seeded with 42 and called twice" in {
    SimpleRNG(42).nextInt._2.nextInt mustBe (-1281479697, SimpleRNG(197491923327988L))
  }

  "SimpleRNG#nonNegativeInt" must "return (16159453, SimpleRNG(1059025964525)) when seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.nonNegativeInt() mustBe (16159453, SimpleRNG(1059025964525L))
  }
  it must "return (1281479697, SimpleRNG(197491923327988)) when seeded with 42 and called twice" in {
    SimpleRNG(42).nonNegativeInt()
      ._2.nonNegativeInt() mustBe (1281479697, SimpleRNG(197491923327988L))
  }
  it must "return (16159453, SimpleRNG(1059025964525)) when evaluating (Int.MinValue, SimpleRNG(42))" in {
    SimpleRNG(21).nonNegativeInt(mockSimpleRNG) mustBe (16159453, SimpleRNG(1059025964525L))
    SimpleRNG(42).nonNegativeInt(mockSimpleRNG) mustBe (16159453, SimpleRNG(1059025964525L))
    SimpleRNG(84).nonNegativeInt(mockSimpleRNG) mustBe (16159453, SimpleRNG(1059025964525L))
  }

  "SimpleRNG#double" must "return the passed value divided by Int.MaxValue" in {
    val rng = SimpleRNG(42)
    rng.double(rng)._1 mustBe 16159453.toDouble/Int.MaxValue
  }

  "SimpleRNG#intDouble" must "return (16159453, -1281479697/Int.MaxValue) when seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.intDouble(rng)._1 mustBe (16159453, -1281479697.toDouble/Int.MaxValue)
  }
  it must "return state SimpleRNG(197491923327988L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.intDouble(rng)._2 mustBe SimpleRNG(197491923327988L)
  }

  "SimpleRNG#doubleInt" must "return (-1281479697/Int.MaxValue, 16159453) when seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.doubleInt(rng)._1 mustBe (-1281479697.toDouble/Int.MaxValue, 16159453)
  }
  it must "return state SimpleRNG(197491923327988L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.doubleInt(rng)._2 mustBe SimpleRNG(197491923327988L)
  }

  "SimpleRNG#double3" must "return (16159453/Int.MaxValue, -1281479697/Int.MaxValue, -340305902/Int.MaxValue) when seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.double3(rng)._1 mustBe (16159453.toDouble/Int.MaxValue, -1281479697.toDouble/Int.MaxValue, -340305902.toDouble/Int.MaxValue)
  }
  it must "return state SimpleRNG(259172689157871L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.double3(rng)._2 mustBe SimpleRNG(259172689157871L)
  }

  "SimpleRNG#ints" must "return List(16159453, -1281479697, -340305902) when called with count=3 and seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.ints(3)(rng)._1 mustBe List(16159453, -1281479697, -340305902)
  }
  it must "return state SimpleRNG(259172689157871L) when called with count=3 and seeded with 42" in {
    val rng = SimpleRNG(42)
    rng.ints(3)(rng)._2 mustBe SimpleRNG(259172689157871L)
  }

  "SimpleRNG#unit" must "return a constant value with the rng unchanged" in {
    val rng = SimpleRNG(42)
    rng.unit(5)(rng) mustBe (5, rng)
  }

  "SimpleRNG#map" must "transition the state according to the given transistion" in {
    val rng = SimpleRNG(42)
    rng.map[Int, Int](_.nextInt)(a => a)(rng) mustBe (16159453, SimpleRNG(1059025964525L))
  }
  it must "apply the given map to the random value returned" in {
    val rng = SimpleRNG(42)
    rng.map(_.nextInt)(a => 2*a)(rng) mustBe (32318906, SimpleRNG(1059025964525L))
  }
}
