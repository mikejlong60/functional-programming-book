package chapter8

import chapter6.{ RNG, SimpleRNG, State}
import chapter7.Par 
import chapter7.Par._

case class Gen[+A](sample: State[RNG, A]) {

  def unsized: SGen[A] = {
    val sizer: ( (Int) => Gen[A] ) = n  => this
    SGen(sizer)
  }

  def map[B](f: A => B): Gen[B] = {
    val r = sample.map(a => f(a))
    Gen(r)
  }

  def flatMap[B]( f: A => Gen[B] ): Gen[B] = {
    val r = sample.flatMap(aa =>  f(aa).sample)
   Gen(r)
  }

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))
}
 
object Gen {
  
  def id[A](a: A): A = a

  def choose(start: Double, stopExclusive: Double): Gen[Double] = Gen(State(run = RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def choose(start: Int, stopExclusive: Int): Gen[Int]  = choose(start.toFloat, stopExclusive.toFloat).map(fl=> fl.toInt)

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

  def parIntGen(start: Double, stopExclusive: Double) = Gen.choose(start, stopExclusive) map (Par.unit(_))

  def parListOfN[A](n: Gen[Int], g: Gen[A]) = listOfN2(n, g) map (Par.unit(_))

  def takeWhileDropWhileF(n: Gen[Int]): Gen[(Int) => Boolean] = n map (i =>  ((x: Int) => {
    println(s"x: $x, i: $i")
    x  >= i
  } ))


  //This generates a function that verifies the behavior of a particular API method on List.
   def filterFTest[A](n: Gen[A])(l: List[A]): Gen[(A) => Boolean] = n map (i =>  ((x: A) => {
     val dropped = l.filter(ii => ii  != i)
     val kept = l.filter(ii   => ii == i )
     val actual = (dropped ++ kept)
     actual.toSet == l.toSet
   } ))

  //This generates a function showing the relationship between exists and filter on List
   def existsFTest[A](n: Gen[A])(l: List[A]): Gen[(A) => Boolean] = n map (i =>  ((x: A) => {
     val filtered = l.filter(ii => ii  == i)
     val atLeastOne = l.exists(ii   => ii == i )
     if (filtered.size > 0) atLeastOne == true
     else atLeastOne == false
   } ))

 }
