package chapter6

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 

class SimpleRNGTest extends PropSpec with PropertyChecks with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 10, maxSize = 20, maxDiscarded=2000)
  property("Generating two random numbers using the same generator produces the same number ") {
    forAll { x: Int =>
      val rng = SimpleRNG(x)
      val (n1, rng2) = rng.nextInt
      val (n2, rng3) = rng.nextInt
      n1 should be (n2)
    }
  }

  @annotation.tailrec
  final def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val r = rng.nextInt
    if (r._1 >= 0) r
    else nonNegativeInt(r._2)
  }

  property("Generate a non-negative random number") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = nonNegativeInt(rng: RNG)
      actual._1 should be >= (0)
    }
  }

  @annotation.tailrec
  final def double(rng: RNG): (Double, RNG) = {
    val h = nonNegativeInt(rng)
    if (h._1 > 0) (1.toDouble / h._1.toDouble, h._2)
    else double(h._2)
  }

  property("Generate a non-negative double between 0 and 1") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = double(rng: RNG)
      actual._2 should not be (rng)
      actual._1.toInt should be  (0)//Just truncate the Double. I am having trouble getting Scalacheck to deal with doubles.  Should instead use bigdecimal or a precise type for rational numbers.  But that would complicate too much.
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val h = nonNegativeInt(rng)
    val i = double(h._2)
    ((h._1, i._1), i._2)
  }

  property("Generate pairs of random numbers") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = intDouble(rng)
      actual._2 should not be (rng)
      actual._1._1 should be >= (0) 
      actual._1._2.toInt should be  (0)
    }
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val h = intDouble(rng)
    ((h._1._2, h._1._1), h._2)
  }

  property("Generate pairs of random numbers in opposite order") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = doubleInt(rng)
      actual._2 should not be (rng)
      actual._1._2 should be >= (0) 
      actual._1._1.toInt should be  (0)
    }
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val i = double(rng)
    val j = double(i._2)
    val k = double(j._2)
    ((i._1, j._1, k._1), k._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List.empty[Int], rng)
    else {
      val l = nonNegativeInt(rng)
      (l._1 :: (ints(count - 1)(l._2))._1, l._2)
    }
  }

  property("Generate triple of random numbers") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = double3(rng)
      actual._2 should not be (rng)
      actual._1._1.toInt should be  (0)
      actual._1._2.toInt should be  (0)
      actual._1._3.toInt should be  (0)
    }
  }

  property("Generate list of random Ints") {
    forAll {x: Short =>
      whenever(x > 0 && x < 4000) {
        val rng = SimpleRNG(x)
        val actual = ints(x)(rng)//Because this is not tail recursive it blows up.  The book solution shows a tail-recursive version with an inner function.
        actual._1.size should be (x)
      }
    }
  }


  import RNG._

  val nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i  - (i %2))

  val doubleMap: Rand[Double] = map(nonNegativeInt)(i => 
    if (i > 0) (1.toDouble / i.toDouble)
    else 0
    )

  property("Generate a non-negative double between 0 and 1 with map") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = doubleMap(rng)
      actual._2 should not be (rng)
      actual._1.toInt should be  (0)
    }
  }
 
  property("Generate an Int and  Double pair using map2") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val fff = map2(nonNegativeInt, doubleMap)((_,_))
      val actual = fff(rng)
      actual._2 should not be (rng)
      actual._1._1 should be >= (0)
      actual._1._2.toInt should be  (0)
    }
  }

  property("Generate a Double  Double pair using map2") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val fff = map2(doubleMap, doubleMap)((_, _))
      val actual = fff(rng)
      actual._2 should not be (rng)
      actual._1._1.toInt should be (0)
      actual._1._2.toInt should be  (0)
    }
  }

}
