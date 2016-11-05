package monoid

trait Monoid[B] {
  def op(a1: B, a2: B): B

  def id: B

  def concatenate(as: List[B]): B =
    as.foldLeft(this.id)(this.op)

  def foldMap[A](as: List[A])(f: A => B): B =
    as.foldLeft(this.id)((b, a) => this.op(b, f(a)))

  def foldMapV[A](v: IndexedSeq[A])(f: A => B): B = {
    v.length match {
      case 0 => this.id
      case 1 => f(v.head)
      case _ =>
        val splitSeq: (IndexedSeq[A], IndexedSeq[A]) = v.splitAt(v.length / 2)
        op(foldMapV(splitSeq._1)(f), foldMapV(splitSeq._2)(f))
    }
  }
}
