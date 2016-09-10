package functional_state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  //  such functions are called a state action or state transition
  //  state (rng) is transitioned by the method (as well as returning a result here)
  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val random = rng.nextInt
    if(random._1 == Int.MinValue) random._2.nextInt else random.copy(_1 = math.abs(random._1))
  }

  def double(rng: RNG): (Double, RNG) = {
    val random = rng.nextInt
    (random._1.toDouble/Int.MaxValue, random._2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val first = rng.nextInt
    val second = double(first._2)
    ((first._1, second._1), second._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    (intDouble(rng)._1.swap, intDouble(rng)._2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val first = double(rng)
    val second = double(first._2)
    val third = double(second._2)
    ((first._1, second._1, third._1), third._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def listBuilder(count: Int, thisList: List[Int] = List())(rng: RNG): (List[Int], RNG) = {
      if(count > 0) {
        listBuilder(count - 1, thisList :+ rng.nextInt._1)(rng.nextInt._2)
      }
      else {
        //        If not using this rng it should be passed on to the next caller (so no nextInt here)
        (thisList, rng)
      }
    }
    listBuilder(count)(rng)
  }
}
