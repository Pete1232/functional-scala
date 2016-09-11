package functional_state

trait RNG {
  import functional_state.RNG.Rand

  def nextInt: (Int, RNG)

  def map[A, B](s: Rand[A])(f: A => B): (B, RNG) =
    RNG.map(s)(f)(this)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): (C, RNG) =
    RNG.map2(ra, rb)(f)(this)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): (B, RNG) =
    RNG.flatMap(f)(g)(this)
}

object RNG {
  //  such functions are called a state action or state transition
  //  state (rng) is transitioned by the method (as well as returning a result here)
  type Rand[+A] = RNG => (A, RNG)

  implicit def randToResult[A](rand: (A, RNG)): A = rand._1

  implicit def randToRNG[A](rand: (A, RNG)): RNG = rand._2

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
//      generate a random a with f
      val (a, rng2) = f(rng)
//      take that a and choose a Rand[B] based on its value
      g(a)(rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val (list, r) =
        fs.foldLeft(List[A](), rng) { (state, rand) =>
          // these next three lines are very similar to map2; which hints that something isn't as good as it could be
          // see comments after the function
          // (didn't spot the repetition until I cross-checked against the solutions)
          val (thisList, thisRng) = state
          val (nextA, nextRng) = rand(thisRng)
          (thisList :+ (nextA), nextRng)
        }
      (list, r)
    }
//  the neater fp in scala solution for reference
//  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

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
    map(_.nextInt) { i =>
      if (i < 0) -(i + 1) else i
    }

  def double: Rand[Double] =
    map(_.nextInt)(_.toDouble / Int.MaxValue)

  def intDouble: Rand[(Int, Double)] =
    both(_.nextInt, double(_))

  def doubleInt: Rand[(Double, Int)] =
    both(double(_), _.nextInt)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val first = double(rng)
    val second = double(first._2)
    val third = double(second._2)
    ((first._1, second._1, third._1), third._2)
  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  def nonNegativeLessThan(n: Int): Rand[Int] = rng =>
    rng.flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        // state had already been transitioned by nonNegativeInt
        unit(mod)
      else nonNegativeLessThan(n)
    }
}
