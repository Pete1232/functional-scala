package monoid

import org.scalatest.{FlatSpec, MustMatchers}

class MonoidSpec extends FlatSpec with MustMatchers{
  "Monoids#lefterFold" must "foldLeft" in {
    Monoids.foldLefter(List(1, 2, 3, 4, 5))(0)(_ + _) mustBe 15
    Monoids.foldLefter(List("a", "b", "c"))("")(_ + _) mustBe "abc"
    Monoids.foldLefter(List(3))(5)(_ % _) mustBe 3
  }
  "Monoids#righterFold" must "foldRight" in {
    Monoids.foldRighter(List(1, 2, 3, 4, 5))(0)(_ + _) mustBe 15
    Monoids.foldRighter(List("a", "b", "c"))("")(_ + _) mustBe "abc"
    Monoids.foldRighter(List(3))(5)(_ % _) mustBe 2
  }
}
