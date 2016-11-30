import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

// want to combine two Future[Option] values
val fo1 = Future.successful(Some("Alice"))
val fo2 = Future.successful(Some("Bob"))

val fo1and2: Future[Option[String]] =
fo1.flatMap( maybeAlice =>
  fo2.map( maybeBob =>
    maybeAlice.flatMap( alice =>
      maybeBob.map( bob =>
        alice + " and " + bob
      )
    )
  )
)

Await.result(fo1and2, Duration.Inf).get

// looks horrible - what if we wanted to combine 3?
val fo3 = Future.successful(Some("Charlie"))

val fo1and2and3: Future[Option[String]] =
fo1.flatMap( maybeAlice =>
  fo2.flatMap( maybeBob =>
    fo3.map(maybeCharlie =>
      maybeAlice.flatMap(alice =>
        maybeBob.flatMap(bob =>
          maybeCharlie.map(charlie =>
            alice + " and " + bob + " and " + charlie
          )
        )
      )
    )
  )
)

Await.result(fo1and2and3, Duration.Inf).get

// still works but would be nice to use for-comprehension

val forComp =
for{
  alice <- fo1
  bob <- fo2
  charlie <- fo3
} yield alice + " and " + bob + " and " + charlie

Await.result(forComp, Duration.Inf)

// but this only unwraps the Future
// need our own class to combine them

object FutureO {
  // uplift _any_ value to a FutureO
  def unit[A](a: => A): FutureO[A] =
    FutureO(Future.successful(Option(a)))

  // for a for-comprehension we need a flatMap
  def flatMap[A,S](foa: FutureO[A])(f: (A) => FutureO[S]): FutureO[S] =
    foa flatMap f

  // and we also need a map
  // note this can be defined in terms of just flatMap and unit
  def map[A, S](foa: FutureO[A])(f: A => S): FutureO[S] =
    flatMap(foa)(a => unit(f(a)))
}

// construct a FutureO from a Future[Option]
// case class gives us a free apply method to wrap a Future[Option]
//case class FutureO[A](fo: Future[Option[A]]){
//  // self type (basically this)
//  self =>
//
//  def flatMap[S](f: (A) => FutureO[S]): FutureO[S] =
//    FutureO(
//      self.fo.flatMap{
//        case Some(a) => f(a).fo
//        case _ => Future.successful(None)
//      }
//    )
//
//  // take the implementation from the companion object
//  def map[S](f: A => S): FutureO[S] =
//    FutureO.map(self)(f)
//
//  // if we wanted to use guards (if statements) in the for-
//  // comprehension then we'd need a filter method too
//  def filter(p: A => Boolean): FutureO[A] = ???
//}

// so now what can we do?

// uplift a String to FutureO with unit
val futureO1 = FutureO.unit("Alice")
// or wrap an existing Future[Option] with apply
val futureO2 = FutureO(Future.successful(Option("Bob")))

val forComp2 =
for{
  alice <- futureO1
  bob <- futureO2
} yield alice + " and " + bob + " again!"

// back to a Future[Option] when we're done
Await.result(forComp2.fo, Duration.Inf).get

// there's a lot more we could do with just this model
// map2
// sequence
// fold

// taking a look back at the types

object FutureOAgain {
  def unit[A](a: => A): FutureO[A] = ???

  def flatMap[A,S](foa: FutureO[A])(f: (A) => FutureO[S]): FutureO[S] = ???

  def map[A, S](foa: FutureO[A])(f: A => S): FutureO[S] =
    flatMap(foa)(a => unit(f(a)))
}

// what if we replaced FutureO with another type?

object Box {
  def unit[A](a: => A): Box[A] = ???

  def flatMap[A,S](foa: Box[A])(f: (A) => Box[S]): Box[S] = ???

  def map[A, S](foa: Box[A])(f: A => S): Box[S] =
    flatMap(foa)(a => unit(f(a)))
}
case class Box[A](a: A)

// in particular look at map - its defined _exactly_ the same for both
// let's make a generic trait to avoid repetition

trait Monad[F[_]]{
  def unit[A](a: => A): F[A]
  def flatMap[A, S](ma: F[A])(f: A => F[S]): F[S]
  def map[A, S](ma: F[A])(f: A => S): F[S] =
    flatMap(ma)(a => unit(f(a)))
}

// and now we can try it out

object FutureOMonad extends Monad[FutureO] {
  override def unit[A](a: => A): FutureO[A] =
    FutureO(Future.successful(Option(a)))

  override def flatMap[A, B](foa: FutureO[A])(f: (A) => FutureO[B]): FutureO[B] =
    foa flatMap f
}

case class FutureO[A](fo: Future[Option[A]]){
  self =>

  def flatMap[S](f: (A) => FutureO[S]): FutureO[S] =
    FutureO(
      self.fo.flatMap{
        case Some(a) => f(a).fo
        case _ => Future.successful(None)
      }
    )

  def map[S](f: A => S): FutureO[S] =
    FutureOMonad.map(self)(f)
}

// so we get our map implementation for free!
// in fact anything we can define in terms of just unit and flatMap we only have to define once in Monad
