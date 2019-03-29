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

//  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = rng => {
//    val r = flatMap(s)(a => unit(f(a)))
//    r(rng)
//  }

//  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] =
//    rng => {
//      val (a, r1) = f(rng)
//      val r = g(a)(r1)
//      r
//    }

//  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = { rng =>
//    val r = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
//    r(rng)
//  }



//  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldLeft(unit((List()))((f, acc) => map2(f, acc)((a, b) => b :: a))

   def nonNegativeInt(rng: RNG): (Int, RNG) = {
     val (i, r) = rng.nextInt

     if (i < 0) (-(i + 1), r)
     else (i, r)
   }
   def nonNegativeInt2(rng: RNG): Rand[Int] = {
     val (i, r) = rng.nextInt

     if (i < 0) unit(-(i + 1))//, r)
     else unit(i)
   }

  def double(rng: RNG): Rand[Double] = {
    val (i, r) = nonNegativeInt(rng)
    if (i > 0) State(s => (1.toDouble / i.toDouble, r))
    else double(r)
  }

}


