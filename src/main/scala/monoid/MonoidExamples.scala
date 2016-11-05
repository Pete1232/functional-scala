package monoid

object MonoidExamples {

  object IntUnderAdditionMonoid extends Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def id: Int = 0
  }

  object IntUnderMultiplicationMonoid extends Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def id: Int = 1
  }

  object BooleanOrMonoid extends Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def id: Boolean = false
  }

  object BooleanAndMonoid extends Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def id: Boolean = true
  }

  class OptionMonoid[A] extends Monoid[Option[A]]{
    override def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1 orElse a2

    override def id: Option[A] = None
  }

  def optionMonoid[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] =
        a1 orElse a2

      override def id: Option[A] = None
    }

  class DualMonoid[A](m: Monoid[A]) extends Monoid[A] {
    override def op(x: A, y: A): A = m.op(y, x)

    override val id = m.id
  }

  class EndoMonoid[A] extends Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1.andThen(a2)

    override def id: (A) => A = (a) => a
  }
}
