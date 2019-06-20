package chapter8

import chapter6.{ RNG, SimpleRNG, State}


case class Gen[+A](sample: State[RNG, A]) {

  def unsized: SGen[A] = {
    val sizer: ( (Int) => Gen[A] ) = n  => this
    SGen(sizer)
  }
}
 
object Gen {
  
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(run = RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def chooseThatRunsTooLong(start: Int, stopExclusive: Int): Gen[Int] = {   

    val rng: SimpleRNG = SimpleRNG(System.currentTimeMillis)

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

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(sample = State.sequence(List.fill(n)(g.sample)))

  //Just like listOfN but uses another generator(n) to decide how big a list to make.
  def listOfN2[A](n: Gen[Int], g: Gen[A]): Gen[List[A]] = {
    val r = n.sample.flatMap(nn =>  State.sequence(List.fill(nn)(g.sample)))
    Gen(sample=r)
  }
  
  def map[A, B](a: Gen[A])(f: A => B): Gen[B] = {
    val r = a.sample.map(a => f(a))
    Gen(r)
  }
  def flatMap[A, B](a: Gen[A])( f: A => Gen[B] ): Gen[B] = {
    val r = a.sample.flatMap(aa =>  f(aa).sample)
   Gen(r)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
    Gen(boolean.sample.flatMap{first =>
      if (first) g1.sample
      else g2.sample
    })

  def weighted[A](g1: (Gen[A], Int), g2: (Gen[A], Int)): Gen[A] = {
    val k = List.fill(g1._2)(g1._1) ++ List.fill(g2._2)(g2._1)
    val len = k.length
    val fff = Gen.choose(0, len)
    val r = fff.sample.flatMap(n => k(n).sample)
     Gen(r)
  }

}
