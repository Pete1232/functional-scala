package prop

import functional_state.RNG
import functional_state.RNG.SimpleRNG
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
    def intList(start: Int, end: Int) = new Gen(RNG.nextInt).choose(start, end).sample.runWith(rng)

    // definitely should be rewritten using the property testing library
    0 to 9 must contain(intList(0, 10)._1)
    0 to 1 must contain(intList(0, 2)._1)
    15 to 99 must contain(intList(15, 100)._1)
  }
  it should "not work if the type is not int" in {
    val rng = SimpleRNG(42)
    intercept[MatchError] {
      new Gen(RNG.ints(5)).choose(0, 10).sample.runWith(rng)
    }
    intercept[MatchError] {
      new Gen(RNG.double).choose(0, 10).sample.runWith(rng)
    }
  }
}
