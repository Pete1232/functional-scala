package examples

import examples.Types.FutureO
import monad.Monad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Types {
  type FutureO[A] = Future[Option[A]]
}

object FutureOMonad extends Monad[FutureO] {
  override def unit[A](a: => A): FutureO[A] =
    Future.successful(Option(a))

  override def flatMap[A, B](ma: FutureO[A])(f: (A) => FutureO[B]): FutureO[B] =
    ma.flatMap {
      case Some(a) => f(a)
      case _ => Future.successful(None)
    }
}
