package chapter6

import State._

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a =>State( s => (f(a), s)))
     
  def flatMap[B](g: A => State[S, B]): State[S, B] = State( s => {
      val (a, s1) = run(s)
      g(a).run(s1)
    })

//  def map2[S, A, B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = { rng =>
//    val r = flatMap(a => rb.flatMap(b => State.unit(f(a, b))))
//    r(rng)
//  }

}

object State {
  type Rand[A] = State[RNG, A]

  //All the subsequent functions return a function which takes a RNG and returns an (A(or B or C), RNG) pair.
  def unit[S, A](a: A): State[S, A] = State(run = s => (a, s))

   def nonNegativeInt(rng: RNG): (Int, RNG) = {
     val (i, r) = rng.nextInt

     if (i < 0) (-(i + 1), r)
     else (i, r)
   }

  def double(rng: RNG): Rand[Double] = {
    val (i, r) = nonNegativeInt(rng)
    if (i > 0) State(s => (1.toDouble / i.toDouble, r))
    else double(r)
  }

}
