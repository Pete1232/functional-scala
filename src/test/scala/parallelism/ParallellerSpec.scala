package parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{FlatSpec, MustMatchers}

// interesting consequence of the process described in the book is that it was a long time
// before I had any passing tests (it encourages api design before writing any implementation)
class ParallellerSpec extends FlatSpec with MustMatchers {
  // why does this need 5 threads to run?
  implicit val ec: ExecutorService = Executors.newFixedThreadPool(5)

  "Paralleller#sum" must "add up the given list of ints" in {
    Par.run(Paralleller.sum(Vector(1, 2, 3, 4, 5))).get mustBe 15
  }

  "Par" must "have some way to wrap a value" in {
    // don't need a test for this - just confirm the mechanism exists (i.e. it compiles)
    Par.unit(5)
  }
  it must "have a way to retrieve the value" in {
    Par.run(Par.unit(5)).get() mustBe 5
  }
}
