package prop

import functional_state.{RNG, State}

trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop =
    new Prop { override def check = Prop.this.check && p.check }
}

case class Gen[A](sample: State[RNG,A]){
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    new Gen(sample.map {
      case i: Int => i % (stopExclusive - start) + start
    })
}
