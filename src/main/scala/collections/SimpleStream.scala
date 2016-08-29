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
    emptyOrCons(
      Empty,
      (h, t) => {
//        headOption cannot be None here
        if(p(h())) Cons(() => headOption.get, () => t().takeWhile(p)) else Empty
      }
    )

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
}
