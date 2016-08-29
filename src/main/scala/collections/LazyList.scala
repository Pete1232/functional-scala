package collections

sealed trait SimpleStream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
}
case object Empty extends SimpleStream[Nothing]
case class Cons[+A](h: () => A, t: () => SimpleStream[A]) extends SimpleStream[A]

object SimpleStream {
  def cons[A](hd: A, tl: SimpleStream[A]): SimpleStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: SimpleStream[A] = Empty

  def apply[A](as: A*): SimpleStream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
