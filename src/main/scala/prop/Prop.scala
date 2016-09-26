package prop

import functional_state.{RNG, State}

trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop =
    new Prop { override def check = Prop.this.check && p.check }
}

case class Gen[A](sample: State[RNG,A]){
  /*
    Primitives
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    new Gen(sample.map {
      case i: Int => i % (stopExclusive - start) + start
    })

  def unit[A](a: => A): Gen[A] = new Gen(State.unit(a))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    new Gen(
      RNG.sequence(
        List.fill(n)(g.sample)
      )
    )

  def genToOption(g: Gen[A]): Gen[Option[A]] =
    new Gen(g.sample.map(Some(_)))

  def optionToGen(maybeG: Gen[Option[A]]): Gen[A] =
    new Gen(maybeG.sample.map(_.get))

  /*
     Non-primitives
   */
  //  def boolean: Gen[Boolean] = new Gen(RNG.nextInt.map(_ >= 0))
  def boolean: Gen[Boolean] = new Gen(choose(0, 2).sample.map(_ == 1))

  def chooseN(start: Int, stopExclusive: Int, N: Int): Gen[List[Int]] =
    listOfN(N, choose(start, stopExclusive))

  def string(length: Int): Gen[String] =
    new Gen(listOfN(length, choose(32, 127)).sample.map(_.map(_.toChar.toString).reduceLeft(_ + _)))
}
