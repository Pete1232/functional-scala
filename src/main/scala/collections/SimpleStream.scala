package collections

// Stream == Lazy List
sealed trait SimpleStream[+A] {
  //  program at the interface

  def headOption: Option[A] =
    emptyOrCons(
      None,
      (h, t) => Some(h())
    )

  def tail: SimpleStream[A] = drop(1)

  def toList: List[A] =
    emptyOrCons(
      Nil,
      (h, t) => List(h()) ++ t().toList
    )

  def take(n: Int): SimpleStream[A] =
    emptyOrCons(
      Empty,
      (h, t) => if (n > 0) Cons(h, () => t().take(n - 1)) else Empty
    )

  def drop(n: Int): SimpleStream[A] =
    emptyOrCons(
      Empty,
      (h, t) => {
        if (n > 0) t().headOption match {
          case None => Empty
          case Some(hd) => Cons(() => hd, () => t().tail).drop(n - 1)
        } else this
      }
    )

  def takeWhile(p: A => Boolean): SimpleStream[A] =
    foldRight(Empty: SimpleStream[A])((h, stream) => if (p(h)) SimpleStream.cons(h, stream) else Empty)

  def exists(p: A => Boolean): Boolean =
    emptyOrCons(
      false,
      //      t is lazy, so if p(h()) then t is never evaluated
      (h, t) => p(h()) || t().exists(p)
    )

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    emptyOrCons(
      z,
      (h, t) => f(h(), t().foldRight(z)(f))
    )

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, y) => p(x) && y)

  def map[B](m: A => B): SimpleStream[B] =
    foldRight(Empty: SimpleStream[B])((a, b) => SimpleStream.cons(m(a), b))

  //  http://docs.scala-lang.org/tutorials/tour/variances.html
  //  Also see Programming in Scala (3rd ed) 19.4 and 19.5
  //  Method parameters flip the positions classification; in this case from + to -
  //  Basically the Scala compiler cannot guarantee covariance, but it can be guaranteed by setting a lower bound
  //  e.g. appending a more specific type than expected, like Apple instead of Fruit, would fail to compile
  //  A is a lower bound for B. i.e. B is a supertype of A
  def append[B >: A](b: => B): SimpleStream[B] =
    foldRight(SimpleStream(b))((b, bs) => SimpleStream.cons(b, bs))

  def flatMap[B](f: A => SimpleStream[B]): SimpleStream[B] =
    foldRight(Empty: SimpleStream[B]) { (a, bs) =>
      f(a).foldRight(bs)((a, bs) => SimpleStream.cons(a, bs))
    }

  //  was repeating this in every implementation
  private def emptyOrCons[B](onEmpty: => B, onCons: (() => A, () => SimpleStream[A]) => B) = this match {
    case Empty => onEmpty
    case Cons(h, t) => onCons(h, t)
  }
}

case object Empty extends SimpleStream[Nothing]

// case class params cannot be called by name,
// so need to define as functions (the () => bit)
case class Cons[+A](h: () => A, t: () => SimpleStream[A]) extends SimpleStream[A]

object SimpleStream {
  //  'smart constructor' for Cons
  //  by-name so not to evaluate until needed
  def cons[A](hd: => A, tl: => SimpleStream[A]): SimpleStream[A] = {
    //  values are cached so forcing multiple times returns the cached value instead of recalculating
    //  (useful when the passed value is a function rather than a simple value)
    lazy val head = hd
    lazy val tail = tl
    //  then the call to the actual constructor
    Cons(() => head, () => tail)
  }

  //  'smart constructor' for Empty
  //  looks useless but actually helps with type inference
  def empty[A]: SimpleStream[A] = Empty

  def apply[A](as: A*): SimpleStream[A] =
  //  helper method for building a SimpleStream
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): SimpleStream[A] = cons(a, constant(a))

  def from(n: Int): SimpleStream[Int] = cons(n, from(n + 1))

  def fib(a: Int = 1, b: Int = 0): SimpleStream[Int] = cons(a + b, fib(b, a + b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): SimpleStream[A] = {
    f(z).map { as => {
      val (a, s) = (as._1, as._2)
      cons(a, cons(a, unfold(s)(f)))
    }
    }.getOrElse(Empty)
  }
}
