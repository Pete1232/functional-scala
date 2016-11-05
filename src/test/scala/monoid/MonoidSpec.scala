package monoid

import org.scalatest.{FlatSpec, MustMatchers}

class MonoidSpec extends FlatSpec with MustMatchers{
  "Monoids#lefterFold" must "foldLeft" in {
    MonoidExamples.foldLefter(List(1, 2, 3, 4, 5))(0)(_ + _) mustBe 15
    MonoidExamples.foldLefter(List("a", "b", "c"))("")(_ + _) mustBe "abc"
    MonoidExamples.foldLefter(List(3))(5)(_ % _) mustBe 3
  }
  "Monoids#righterFold" must "foldRight" in {
    MonoidExamples.foldRighter(List(1, 2, 3, 4, 5))(0)(_ + _) mustBe 15
    MonoidExamples.foldRighter(List("a", "b", "c"))("")(_ + _) mustBe "abc"
    MonoidExamples.foldRighter(List(3))(5)(_ % _) mustBe 2
  }
}
