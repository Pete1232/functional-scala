package monoid

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def id: A

  def concatenate(as: List[A]): A =
    as.foldLeft(this.id)(this.op)

  def foldMap[B](as: List[B])(f: B => A): A =
    as.foldLeft(this.id)((b, a) => this.op(b, f(a)))
}
