package functional_state

import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, MustMatchers}
import org.mockito.Mockito._
import RNG._

class RNGSpec extends FlatSpec with MustMatchers with MockitoSugar {
  val mockSimpleRNG = mock[SimpleRNG]
  when(mockSimpleRNG.nextInt).thenReturn((Int.MinValue, SimpleRNG(42)))

  "SimpleRNG#nextInt" must "return (16159453, SimpleRNG(1059025964525)) when seeded with 42" in {
    val simpleRNG = SimpleRNG(42)
    simpleRNG.nextInt mustBe(16159453, SimpleRNG(1059025964525L))
    simpleRNG.nextInt mustBe(16159453, SimpleRNG(1059025964525L))
    simpleRNG.nextInt mustBe(16159453, SimpleRNG(1059025964525L))
  }
  it must "return (-1281479697, SimpleRNG(197491923327988)) when seeded with 42 and called twice" in {
    SimpleRNG(42).nextInt._2.nextInt mustBe(-1281479697, SimpleRNG(197491923327988L))
  }

  "RNG#nonNegativeInt" must "return (16159453, SimpleRNG(1059025964525)) when seeded with 42" in {
    val rng = SimpleRNG(42)
    nonNegativeInt(rng) mustBe(16159453, SimpleRNG(1059025964525L))
  }
  it must "return (1281479697, SimpleRNG(197491923327988)) when seeded with 42 and called twice" in {
    val rng = SimpleRNG(42)
    nonNegativeInt(nonNegativeInt(rng)._2)
      .mustBe(1281479697, SimpleRNG(197491923327988L))
  }
  it must "return (16159453, SimpleRNG(1059025964525)) when evaluating (Int.MinValue, SimpleRNG(42))" in {
    nonNegativeInt(mockSimpleRNG) mustBe(16159453, SimpleRNG(1059025964525L))
    nonNegativeInt(mockSimpleRNG) mustBe(16159453, SimpleRNG(1059025964525L))
    nonNegativeInt(mockSimpleRNG) mustBe(16159453, SimpleRNG(1059025964525L))
  }

  "RNG#double" must "return the passed value divided by Int.MaxValue" in {
    val rng = SimpleRNG(42)
    double(rng)._1 mustBe 16159453.toDouble / Int.MaxValue
  }

  "RNG#intDouble" must "return (16159453, -1281479697/Int.MaxValue) when seeded with 42" in {
    val rng = SimpleRNG(42)
    intDouble(rng)._1 mustBe(16159453, -1281479697.toDouble / Int.MaxValue)
  }
  it must "return state SimpleRNG(197491923327988L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    intDouble(rng)._2 mustBe SimpleRNG(197491923327988L)
  }

  "RNG#doubleInt" must "return (-1281479697/Int.MaxValue, 16159453) when seeded with 42" in {
    val rng = SimpleRNG(42)
    doubleInt(rng)._1 mustBe(-1281479697.toDouble / Int.MaxValue, 16159453)
  }
  it must "return state SimpleRNG(197491923327988L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    doubleInt(rng)._2 mustBe SimpleRNG(197491923327988L)
  }

  "RNG#double3" must "return (16159453/Int.MaxValue, -1281479697/Int.MaxValue, -340305902/Int.MaxValue) when seeded with 42" in {
    val rng = SimpleRNG(42)
    double3(rng)._1 mustBe(16159453.toDouble / Int.MaxValue, -1281479697.toDouble / Int.MaxValue, -340305902.toDouble / Int.MaxValue)
  }
  it must "return state SimpleRNG(259172689157871L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    double3(rng)._2 mustBe SimpleRNG(259172689157871L)
  }

  "RNG#ints" must "return List(16159453, -1281479697, -340305902) when called with count=3 and seeded with 42" in {
    val rng = SimpleRNG(42)
    ints(3)(rng)._1 mustBe List(16159453, -1281479697, -340305902)
  }
  it must "return state SimpleRNG(259172689157871L) when called with count=3 and seeded with 42" in {
    val rng = SimpleRNG(42)
    ints(3)(rng)._2 mustBe SimpleRNG(259172689157871L)
  }

  "RNG#unit" must "return a constant value with the rng unchanged" in {
    val rng = SimpleRNG(42)
    unit(5)(rng) mustBe(5, rng)
  }

  "RNG#map" must "transition the state according to the given transistion" in {
    val rng = SimpleRNG(42)
    map[Int, Int](_.nextInt)(a => a)(rng) mustBe(16159453, SimpleRNG(1059025964525L))
  }
  it must "apply the given map to the random value returned" in {
    val rng = SimpleRNG(42)
    map(_.nextInt)(a => 2 * a)(rng) mustBe(32318906, SimpleRNG(1059025964525L))
  }
  it must "be possible to call map directly on an rng" in {
    val rng = SimpleRNG(42)
    rng.map(_.nextInt)(a => 2 * a) mustBe(32318906, SimpleRNG(1059025964525L))
  }

  "RNG#randToResult" must "implicitly convert a (result, rng) to its result" in {
    val rng = SimpleRNG(42)
    rng.map(_.nextInt)(a => a) + 1 mustBe 16159454
  }

  "RNG#randToRNG" must "implicitly convert a (result, rng) to its rng" in {
    val rng = SimpleRNG(42)
    rng.map(_.nextInt)(a => a).nextInt mustBe (-1281479697, SimpleRNG(197491923327988L))
  }
}
