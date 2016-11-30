package prop

import functional_state.RNG.SimpleRNG
import functional_state.{RNG, State}
import org.scalatest.{FlatSpec, MustMatchers}

class PropSpec extends FlatSpec with MustMatchers {

  val falseProp = new Prop {
    override def check: Boolean = false
  }
  val trueProp = new Prop {
    override def check: Boolean = true
  }

  "Prop" must "contain a check function" in {
    falseProp.check
  }
  it must "contain a combinator" in {
    falseProp.&&(falseProp)
  }

  "Prop#&&" must "perform an AND on two checks" in {
    (falseProp && falseProp).check mustBe false
    (falseProp && trueProp).check mustBe false
    (trueProp && falseProp).check mustBe false
    (trueProp && trueProp).check mustBe true
  }

  "Gen#choose" must "generate an integer in the range" in {
    val rng = SimpleRNG(42)
    def intInRange(start: Int, end: Int) = Gen(RNG.nonNegativeInt).choose(start, end).sample.runWith(rng)

    // definitely should be rewritten using the property testing library
    0 to 9 must contain(intInRange(0, 10)._1)
    0 to 1 must contain(intInRange(0, 2)._1)
    15 to 99 must contain(intInRange(15, 100)._1)
  }
  it should "not work if the type is not int" in {
    val rng = SimpleRNG(42)
    intercept[MatchError] {
      Gen(RNG.ints(5)).choose(0, 10).sample.runWith(rng)
    }
    intercept[MatchError] {
      Gen(RNG.double).choose(0, 10).sample.runWith(rng)
    }
  }

  "Gen#listOfN" must "return a generator for a list of A" in {
    val rng = SimpleRNG(42)
    val list = Gen(RNG.nextInt).listOfN(5, Gen(RNG.nextInt))
    val resultList = list.sample.runWith(rng)._1
    resultList.length mustBe 5
    resultList mustBe List(16159453, -1281479697, -340305902, -2015756020, 1770001318)

    val listList = Gen(RNG.nextInt).listOfN(5, Gen(RNG.ints(2)))
    val resultListList = listList.sample.runWith(rng)._1
    resultListList.length mustBe 5
    resultListList.forall(_.length == 2) mustBe true
    resultListList.head mustBe (List(16159453, -1281479697))
  }
  it must "be able to dynamically generate the list size" in {
    val rng = SimpleRNG(42)
    val list = Gen(RNG.nextInt).listOfN(5)
    val resultList = list.sample.runWith(rng)._1
    resultList.length mustBe 5
    resultList mustBe List(16159453, -1281479697, -340305902, -2015756020, 1770001318)
  }

  "Gen#chooseN" must "generate a list of integers in a range" in {
    val rng = SimpleRNG(42)
    def intsInRange(start: Int, end: Int, n: Int = 2) = Gen(RNG.nonNegativeInt).chooseN(start, end, n).sample.runWith(rng)

    intsInRange(0, 9)._1.forall(i => (0 to 10).contains(i)) mustBe true
    intsInRange(0, 2)._1.forall(i => (0 to 1).contains(i)) mustBe true
    intsInRange(15, 100)._1.forall(i => (15 to 99).contains(i)) mustBe true

    intsInRange(0, 9, 10)._1.length mustBe 10
    intsInRange(0, 9, 10)._1.forall(i => (0 to 10).contains(i)) mustBe true
  }

  "Gen#string" must "return a random string of the given length" in {
    val rng = SimpleRNG(42)
    val string = Gen(RNG.nonNegativeInt).string(50).sample.runWith(rng)._1
    string.length mustBe 50
  }

  "Gen#flatMap" must "flatMap" in {
    val rng = SimpleRNG(42)
    Gen(RNG.nonNegativeInt).flatMap(a => Gen(State.unit(a))).sample.runWith(rng)._1 mustBe 16159453
  }

  "Gen#union" must "not combine two generators of different type" in {
    "Gen(State.unit(5)).union(Gen(State.unit(5)), Gen(State.unit(\"\")))" mustNot compile
  }
}
