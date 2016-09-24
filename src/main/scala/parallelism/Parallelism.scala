package parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import parallelism.Par.Par

import scala.language.implicitConversions

object Parallelism {

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      // returning Par[Int] means I need to be able to compose two Par
      // book calls this function map2
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
}

object Par {
  // will be a pure data structure - delegate execution to run
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    def isDone: Boolean = a.isDone || b.isDone
    def isCancelled: Boolean = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) && b.cancel(evenIfRunning)

    def get(): C = get(Long.MaxValue, TimeUnit.DAYS)

    def get(timeout: Long, units: TimeUnit): C = {
      // taken from the solutions
      // thinking about it if this isn't here we have to re-evaluate the future on every call to get!
      @volatile var cache: Option[C] = None
      // get the requested timeout in nanoseconds
      cache match {
        case Some(s) => s
        case _ =>
          val timer = TimeUnit.NANOSECONDS.convert(timeout, units)
          val startTime = System.nanoTime()
          val evalA = a.get(timer, TimeUnit.NANOSECONDS)
          val afterA = System.nanoTime() - startTime
          val evalB = b.get(timer - afterA, TimeUnit.NANOSECONDS)
          val result = f(evalA, evalB)
          cache = Some(result)
          result
      }

    }
  }

  // creates a 'unit of parallelism' (not a formal term)
  // takes an unevaluated A and immediately returns its value
  def unit[A](a: A): Par[A] = ExecutorService => UnitFuture(a)

  // lazy implementation of unit
  // can evaluate strictly thanks to fork - and will evaluate concurrently (delegated to run via fork)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // doesn't just unwrap - it will need to know something about how to execute in parallel
  // this will be referentially transparent - but feels a bit weird
  // (not sure why, probably just because it is choosing the execution strategy - but this doesn't affect the program)
  def run[A](a: Par[A])(implicit s: ExecutorService): Future[A] = a(s)

  // combine two Par
  // def map2[A](a1: Par[A], a2: Par[A])(op: (A, A) => A): Par[A] = ???
  // what if the two Par had different types? As long as we have an f that can compose them it doesn't matter!
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    Map2Future(af, bf, f)
  }

  // move a Par to a separate logical thread
  // don't fully understand this - review after deciding on an implementation
  // from the book - Marks a computation for concurrent evaluation by run - how will this not have side effect?
  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })
  }

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // certainly not perfect - but the example in the solutions doesn't compile when used
  // this also means the implicit conversion must be imported manually
  // seems it would be better for Par to just be a class
  implicit def parToParMap[A](p: Par[A]): ParWrap[A] = new ParWrap(p)

  class ParWrap[A](p: Par[A]){
    def map2[B,C](par: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, par)(f)
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))
}
