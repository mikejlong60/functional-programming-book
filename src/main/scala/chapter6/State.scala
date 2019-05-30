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

  //Mike - I needed this so I took it from the answer key.
  //This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }
}
