package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  //All the subsequent functions return a function which takes a RNG and returns an (A(or B or C), RNG) pair.
  val int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def mapOld[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val r = flatMap(s)(a => unit(f(a)))
    r(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      val r = g(a)(r1)
      r
    }

  def map2Old[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val r1 = ra(rng)
    val r2 = rb(r1._2)
    (f(r1._1, r2._1), r2._2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val r = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
    r(rng)
  }


  //This doesn't quite work because it use the same RNG each time which makes the same number each time.  So you need to pTass
  //along the state with each calculation.
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
        val ggg = fs.map(f => f(rng)._1)
        (ggg, rng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldLeft(unit(List[A]()))((f, acc) => map2(f, acc)((a, b) => b :: a))

//  def nonNegativeInt: Rand[Int] = { rng =>
//    val r = rng.nextInt
//    if (r._1 >= 0) r
//    else nonNegativeInt(r._2)
//  }

   def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
     if (i < 0) (-(i + 1), r)
     else (i, r)
   }

  def double: Rand[Double] = { rng =>
    val h = nonNegativeInt(rng)
    if (h._1 > 0) (1.toDouble / h._1.toDouble, h._2)
    else double(h._2)
  }

  def intDouble: Rand[(Int, Double)] = { rng =>
    val h = nonNegativeInt(rng)
    val i = double(h._2)
    ((h._1, i._1), i._2)
  }



  def double3: Rand[(Double, Double, Double)] = { rng =>
    val i = double(rng)
    val j = double(i._2)
    val k = double(j._2)
    ((i._1, j._1, k._1), k._2)
  }

  def ints(count: Int): Rand[List[Int]] = { rng =>
    if (count == 0) (List.empty[Int], rng)
    else {
      val l = nonNegativeInt(rng)
      (l._1 :: (ints(count - 1)(l._2))._1, l._2)
    }
  }

  def doubleInt: Rand[(Double, Int)] =  { rng => 
    val h = intDouble(rng)
    ((h._1._2, h._1._1), h._2)
  }

  val nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i  - (i %2))

  val doubleMap: Rand[Double] = map(nonNegativeInt)(i => 
    if (i > 0) (1.toDouble / i.toDouble)
    else 0
    )


}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
} 



