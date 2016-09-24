package parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{FlatSpec, MustMatchers}
import parallelism.Par._

// interesting consequence of the process described in the book is that it was a long time
// before I had any passing tests (it encourages api design before writing any implementation)
class ParallelismSpec extends FlatSpec with MustMatchers {
  // why does this need 5 threads to run?
  implicit val ec: ExecutorService = Executors.newFixedThreadPool(5)

  "Paralleller#sum" must "add up the given list of ints" in {
    Par.run(Parallelism.sum(Vector(1, 2, 3, 4, 5))).get mustBe 15
  }

  "Par" must "have some way to wrap a value" in {
    // don't need a test for this - just confirm the mechanism exists (i.e. it compiles)
    unit(5)
  }
  it must "have a way to retrieve the value" in {
    Par.run(unit(5)).get() mustBe 5
  }

  "Par#asyncF" must "convert a function to a Par" in {
    val squareF = Par.asyncF((i: Int) => i * i)
    Par.run(squareF(5)).get() mustBe 25
  }

  "Par#map2" must "combine two Par" in {
    Par.run(map2(unit(Seq(1, 2)), unit(Seq(1, 2)))(_ ++ _)).get() mustBe Seq(1, 2, 1, 2)
  }
  it must "work in infix notation" in {
    Par.run(unit(Seq(1, 2)).map2(unit(Seq(1, 2)))(_ ++ _)).get() mustBe Seq(1, 2, 1, 2)
  }
}
