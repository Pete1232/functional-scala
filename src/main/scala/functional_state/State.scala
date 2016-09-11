package functional_state

import State._

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(
      s => {
//        flatten - obtain the result a using s and iterate the state to s1
        val (a, s1) = run(s)
//        map - calculate a new result and iterate the state again based on the previous result a
        g(a)
          .run(s1)
      }
    )

  def map[B](g: A => B): State[S, B] =
    flatMap(a => unit(g(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  //  such functions are called a state action or state transition
  //  state (rng) is transitioned by the method (as well as returning a result here)
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}