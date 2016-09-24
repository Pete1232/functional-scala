package monoid

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def id: A
}

object Monoids {
  val intAddition: Monoid[Int] =
    new Monoid[Int]() {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def id: Int = 0
    }

  val intMultiplication: Monoid[Int] =
    new Monoid[Int]() {
      override def op(a1: Int, a2: Int): Int = a1 * a2

      override def id: Int = 1
    }

  val booleanOr: Monoid[Boolean] =
    new Monoid[Boolean]() {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

      override def id: Boolean = false
    }

  val booleanAnd: Monoid[Boolean] =
    new Monoid[Boolean]() {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

      override def id: Boolean = true
    }

  def optionMonoid[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] =
        a1 orElse a2

      override def id: Option[A] = None
    }

  def dual[A](m: Monoid[A]): Monoid[A] =
    new Monoid[A] {
      override def op(x: A, y: A): A = m.op(y, x)

      override val id = m.id
    }

  def endoMonoid[A]: Monoid[A => A] =
    new Monoid[(A) => A] {
      override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1.andThen(a2)

      override def id: (A) => A = (a) => a
    }

  //  TODO go back to chapter 2, read about property based testing and do 10.4

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.id)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.id)((b, a) => m.op(b, f(a)))

  def foldLefter[A, B](as: List[A])(z: B)(op: (A, B) => B): B =
    foldMap(as, dual(endoMonoid[B]))(op.curried)(z)

  def foldRighter[A, B](as: List[A])(z: B)(op: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => op(_, a))(z)

  // List(1,2,3).foldLeft(id)( (b, a) => f(a) andThen b  )
  // List(2,3).reduceLeft( f(1) andThen id )
  // List(3).reduceLeft( f(2) andThen ( f(1) andThen id )
  // f(3) andThen ( f(2) andThen ( f(1) andThen id ) )

  // ((3 => 3 + b) andThen (2 => 2 + b) andThen (1 => 1 + b) andThen (b => b))(0)
  // = 0 => (3 + 0 andThen 2 + b andThen 1 + b andThen b)
  // = 3 + 2 + 1 + 0
}
