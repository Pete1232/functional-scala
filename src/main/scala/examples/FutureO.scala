package examples

import examples.Types.FutureOption
import monad.Monad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Types {
  type FutureOption[A] = Future[Option[A]]
}

object FutureO extends Monad[FutureOption] {
  override def unit[A](a: => A): FutureOption[A] =
    Future.successful(Option(a))

  override def flatMap[A, B](ma: FutureOption[A])(f: (A) => FutureOption[B]): FutureOption[B] =
    ma.flatMap {
      case Some(a) => f(a)
      case _ => Future.successful(None)
    }
}

case class FutureO[S](s: S) {
  def flatMap[B](f: (S) => FutureOption[B]): FutureOption[B] =
    FutureO.flatMap(FutureO.unit(s))(f)

  def map[B](f: S => B): FutureOption[B] =
    FutureO.map(FutureO.unit(s))(f)

  def apply[A](a: A): FutureOption[A] = FutureO.unit(a)
}