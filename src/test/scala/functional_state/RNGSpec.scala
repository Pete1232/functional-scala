package functional_state

import functional_state.RNG._
import org.scalatest.{FlatSpec, MustMatchers}

//Note that the state is NEVER mentioned in the client code (i.e. the tests)
class RNGSpec extends FlatSpec with MustMatchers {

  object MockRNG extends SimpleRNG(42) {
    override def nextInt: (Int, RNG) = (Int.MinValue, SimpleRNG(42))
  }

  "SimpleRNG#nextInt" must "return (16159453, SimpleRNG(1059025964525)) when seeded with 42" in {
    val simpleRNG = SimpleRNG(42)
    simpleRNG.nextInt mustBe(16159453, SimpleRNG(1059025964525L))
    simpleRNG.nextInt mustBe(16159453, SimpleRNG(1059025964525L))
    simpleRNG.nextInt mustBe(16159453, SimpleRNG(1059025964525L))
    nextInt.runWith(simpleRNG) mustBe(16159453, SimpleRNG(1059025964525L))
  }
  it must "return (-1281479697, SimpleRNG(197491923327988)) when seeded with 42 and called twice" in {
    //    Additional api due to definition of nextInt
    //    Could hide away if desired, or even add a similar api for the other methods
    //    This is less powerful (it runs immediately so no mapping) but is cleaner and easier to understand at a glance
    //    Note this is basically the api for scala.util.Random
    SimpleRNG(42).nextInt._2.nextInt mustBe(-1281479697, SimpleRNG(197491923327988L))
  }

  "RNG#nonNegativeInt" must "return (16159453, SimpleRNG(1059025964525)) when seeded with 42" in {
    val rng = SimpleRNG(42)
    nonNegativeInt.runWith(rng) mustBe(16159453, SimpleRNG(1059025964525L))
  }
  it must "return (1281479696, SimpleRNG(197491923327988)) when seeded with 42 and called twice" in {
    val rng = SimpleRNG(42)
    nonNegativeInt.runWith(nonNegativeInt.runWith(rng))
      .mustBe(1281479696, SimpleRNG(197491923327988L))
    //    rng.nonNegativeInt.nonNegativeInt
    //      .mustBe(1281479696, SimpleRNG(197491923327988L))
  }
  it must "return (16159453, SimpleRNG(1059025964525)) when evaluating (Int.MinValue, SimpleRNG(42))" in {
    nonNegativeInt.runWith(MockRNG) mustBe(Int.MaxValue, SimpleRNG(42))
  }

  "RNG#double" must "return the passed value divided by Int.MaxValue" in {
    val rng = SimpleRNG(42)
    double.runWith(rng)._1 mustBe 16159453.toDouble / Int.MaxValue
  }

  "RNG#intDouble" must "return (16159453, -1281479697/Int.MaxValue) when seeded with 42" in {
    val rng = SimpleRNG(42)
    intDouble.runWith(rng)._1 mustBe(16159453, -1281479697.toDouble / Int.MaxValue)
  }
  it must "return state SimpleRNG(197491923327988L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    intDouble.runWith(rng)._2 mustBe SimpleRNG(197491923327988L)
  }

  "RNG#doubleInt" must "return (-1281479697/Int.MaxValue, 16159453) when seeded with 42" in {
    val rng = SimpleRNG(42)
    doubleInt.runWith(rng)._1 mustBe(16159453.toDouble / Int.MaxValue, -1281479697)
  }
  it must "return state SimpleRNG(197491923327988L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    doubleInt.runWith(rng)._2 mustBe SimpleRNG(197491923327988L)
  }

  "RNG#double3" must "return (16159453/Int.MaxValue, -1281479697/Int.MaxValue, -340305902/Int.MaxValue) when seeded with 42" in {
    val rng = SimpleRNG(42)
    double3.runWith(rng)._1 mustBe(16159453.toDouble / Int.MaxValue, -1281479697.toDouble / Int.MaxValue, -340305902.toDouble / Int.MaxValue)
  }
  it must "return state SimpleRNG(259172689157871L) when seeded with 42" in {
    val rng = SimpleRNG(42)
    double3.runWith(rng)._2 mustBe SimpleRNG(259172689157871L)
  }

  "RNG#ints" must "return List(16159453, -1281479697, -340305902) when called with count=3 and seeded with 42" in {
    val rng = SimpleRNG(42)
    ints(3).runWith(rng)._1 mustBe List(16159453, -1281479697, -340305902)
  }
  it must "return state SimpleRNG(259172689157871L) when called with count=3 and seeded with 42" in {
    val rng = SimpleRNG(42)
    ints(3).runWith(rng)._2 mustBe SimpleRNG(259172689157871L)
  }

  "RNG#unit" must "return a constant value with the rng unchanged" in {
    val rng = SimpleRNG(42)
    unit(5).runWith(rng) mustBe(5, rng)
  }

  "RNG#map" must "transition the state according to the given transistion" in {
    val rng = SimpleRNG(42)
    nextInt.map(a => a).runWith(rng) mustBe(16159453, SimpleRNG(1059025964525L))
  }
  it must "apply the given map to the random value returned" in {
    val rng = SimpleRNG(42)
    nextInt.map(a => 2 * a).runWith(rng) mustBe(32318906, SimpleRNG(1059025964525L))
  }
  it must "be possible to call map directly on an rng" in {
    val rng = SimpleRNG(42)
    nextInt.map(a => 2 * a).runWith(rng) mustBe(32318906, SimpleRNG(1059025964525L))
  }

  "RNG#randToResult" must "implicitly convert a (result, rng) to its result" in {
    val rng = SimpleRNG(42)
    nextInt.map(a => a).runWith(rng) + 1 mustBe 16159454
  }

  "RNG#randToRNG" must "implicitly convert a (result, rng) to its rng" in {
    val rng = SimpleRNG(42)
    nextInt.map(a => a).runWith(rng).nextInt mustBe(-1281479697, SimpleRNG(197491923327988L))
  }

  "RNG#nonNegativeLessThan" must "return 3 when seeded with 42" in {
    val rng = SimpleRNG(42)
    nonNegativeLessThan(10).runWith(rng)._1 mustBe 3
  }
  it must "return  when seeded with 21" in {
    val rng = SimpleRNG(21)
    nonNegativeLessThan(10).runWith(rng)._1 mustBe 6
  }
}
