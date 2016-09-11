package functional_state

import functional_state.State.Rand

trait RNG{
  def nextInt: (Int, RNG)
}

object RNG {
  implicit def randToResult[A](rand: (A, RNG)): A = rand._1

  implicit def randToRNG[A](rand: (A, RNG)): RNG = rand._2

//  def map[S,A,B](s: State[S, A])(f: A => B): State[S, B]
//  replacing the implementation here is fine as far as map is concerned
//  however - it will break type inference for the client methods
//  either:
//       (bad) specify the type every time we call map
//       (good) implement this in another interface and inherit

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(State.unit[RNG, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    ra.map2(rb)((_, _))

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      //    seed values as in java.util.Random - chosen to satisfy some randomness tests
      //    https://www.math.utah.edu/~beebe/java/random/README is quite interesting
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  //      impl from fpinscala github page
  //      (wasn't sure about how to keep this random - and this isn't perfect)
  //      iterate negative by 1 and change the sign
  def nonNegativeInt: Rand[Int] =
    State[RNG, Int](_.nextInt).map{ i =>
      if (i < 0) -(i + 1) else i
    }

  def double: Rand[Double] =
    State[RNG, Int](_.nextInt).map(_.toDouble / Int.MaxValue)

  def intDouble: Rand[(Int, Double)] =
    State[RNG, Int](_.nextInt).flatMap{ i =>
      State[RNG, Double](double.run).map{ j =>
        (i, j)
      }
    }

  def doubleInt: Rand[(Double, Int)] =
    State[RNG, Double](double.run).flatMap{ i =>
      State[RNG, Int](_.nextInt).map{ j =>
        (i, j)
      }
    }

  def double3: Rand[(Double, Double, Double)] =
    State[RNG, Double](double.run).flatMap{ i =>
      State[RNG, Double](double.run).flatMap{ j =>
        State[RNG, Double](double.run).map{ k =>
          (i, j, k)
      }
    }
  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(State[RNG, Int](_.nextInt)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    State[RNG, Int](nonNegativeInt.run).flatMap{ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        // state had already been transitioned by nonNegativeInt
        State.unit(mod)
      else nonNegativeLessThan(n)
    }
}
