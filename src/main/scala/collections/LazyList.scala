package collections

// Stream == Lazy List
sealed trait SimpleStream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
//      have to 'force' h to evaluate explicitly with h()
    case Cons(h, t) => Some(h())
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
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
