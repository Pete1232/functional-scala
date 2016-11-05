package monoid

import monoid.MonoidExamples._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

abstract class MonoidProperties[A](m: Monoid[A])(implicit arbitrary: Arbitrary[A]) extends Properties(m.getClass.getSimpleName){
  import m._
  property("op must be associative") = forAll{ (a: A, b: A, c: A) =>
    op(op(a, b), c) == op(a, op(b, c))
  }

  property("id must be an identity under op") = forAll{ (a: A) =>
    op(a, id) == a
  }

  property("concatenate must use op to fold a list") = forAll{ (as: List[A]) =>
    m.concatenate(as) == as.foldLeft(m.id)((x, y) => op(x, y))
  }

  property("foldMap must use op to fold a list after mapping the values") = forAll{ (as: List[A]) =>
    m.foldMap(as)((a) => a) == as.foldLeft(m.id)((x, y) => op(x, y))
  }
}

object IntUnderAdditionMonoidProps extends MonoidProperties(IntUnderAdditionMonoid)

object IntUnderMultiplicationMonoidProps extends MonoidProperties(IntUnderMultiplicationMonoid)

object BooleanOrMonoidProps extends MonoidProperties(BooleanOrMonoid)

object BooleanAndMonoidProps extends MonoidProperties(BooleanAndMonoid)

object OptionIntMonoidProps extends MonoidProperties(new OptionMonoid[Int]())
object OptionStringMonoidProps extends MonoidProperties(new OptionMonoid[String]())

object DualIntUnderAdditionMonoidProps extends MonoidProperties(new DualMonoid(IntUnderAdditionMonoid))
object DualBooleanOrMonoidProps extends MonoidProperties(new DualMonoid(BooleanOrMonoid))
object DualOptionIndMonoidProps extends MonoidProperties(new DualMonoid(new OptionMonoid[Int]()))

// comparing two function values will not work
// instead need to generate a test value for each function
abstract class EndoMonoidProperties[A](m: EndoMonoid[A])(implicit arb: Arbitrary[A], arbFunction: Arbitrary[(A) => A]) extends Properties("EndoMonoid") {
  import m._
  property("op must be associative") = forAll{ (a: (A) => A, b: (A) => A, c: (A) => A, testValue: A) =>
    op(op(a, b), c)(testValue) == op(a, op(b, c))(testValue)
  }

  property("id must be an identity under op") = forAll{ (a: (A) => A, testValue: A) =>
    op(a, id)(testValue) == a(testValue)
  }
}

object EndoIntMonoidProps extends EndoMonoidProperties(new EndoMonoid[Int])
object EndoStringMonoidProps extends EndoMonoidProperties(new EndoMonoid[String])
object EndoOptionIntMonoidProps extends EndoMonoidProperties(new EndoMonoid[Option[Int]])
