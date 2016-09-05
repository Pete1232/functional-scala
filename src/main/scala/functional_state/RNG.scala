package functional_state

trait RNG {
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG = this): (Int, RNG)
  def double(rng: RNG = this): (Double, RNG)
  def intDouble(rng: RNG): ((Int,Double), RNG)
  def doubleInt(rng: RNG): ((Double,Int), RNG)
  def double3(rng: RNG): ((Double,Double,Double), RNG)
  def ints(count: Int)(rng: RNG): ((List[Int]), RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    //    seed values as in java.util.Random - chosen to satisfy some randomness tests
    //    https://www.math.utah.edu/~beebe/java/random/README is quite interesting
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  override def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val random = rng.nextInt
    if(random._1 == Int.MinValue) random._2.nextInt else random.copy(_1 = math.abs(random._1))
  }

  override def double(rng: RNG): (Double, RNG) = {
    val random = rng.nextInt
    (random._1.toDouble/Int.MaxValue, random._2)
  }

  override def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val first = rng.nextInt
    val second = double(first._2)
    ((first._1, second._1), second._2)
  }

  override def doubleInt(rng: RNG): ((Double, Int), RNG) =
    (intDouble(rng)._1.swap, intDouble(rng)._2)

  override def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val first = double(rng)
    val second = double(first._2)
    val third = double(second._2)
    ((first._1, second._1, third._1), third._2)
  }

  override def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
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
