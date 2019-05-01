package chapter8

import chapter6.{ RNG, SimpleRNG, State}


case class Gen[A](sample: State[RNG, A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(run = RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def chooseThatRunsTooLong(start: Int, stopExclusive: Int): Gen[Int] = {   

    val rng = SimpleRNG(System.currentTimeMillis)

    @annotation.tailrec
    def intInRange(rng: RNG): (Int, RNG) = {
      val r = rng.nextInt
      println("dude:"+r._1 + " start:"+start  + " stopExclusive:"+ stopExclusive)
      if (r._1 >=  start && r._1 < stopExclusive) r
      else intInRange(r._2)
    }

    val g = intInRange(rng)
    Gen(State.unit(g._1))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean]  = Gen(State(run = RNG.nonNegativeInt).map(n => (n % 2 == 0)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???

}
