package monoid

import monoid.MonoidExamples._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

abstract class MonoidProps[A](m: Monoid[A])(implicit arbitrary: Arbitrary[A]) extends Properties(m.getClass.getSimpleName){
  import m._
  property("op must be associative") = forAll{ (a: A, b: A, c: A) =>
    op(op(a, b), c) == op(a, op(b, c))
  }

  property("id must be an identity under op") = forAll{ (a: A) =>
    op(a, id) == a
  }
}

object IntUnderAdditionMonoidProps extends MonoidProps(IntUnderAdditionMonoid)

object IntUnderMultiplicationMonoidProps extends MonoidProps(IntUnderMultiplicationMonoid)

object BooleanOrMonoidProps extends MonoidProps(BooleanOrMonoid)

object BooleanAndMonoidProps extends MonoidProps(BooleanAndMonoid)