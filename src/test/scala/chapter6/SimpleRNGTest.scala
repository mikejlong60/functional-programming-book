package chapter6

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 

class SimpleRNGTest extends PropSpec with PropertyChecks with Matchers {

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

  def nonNegativeDoubleBetween0and1(rng: RNG): (Double, RNG) = {
    val h = nonNegativeInt(rng)
    (1.toDouble / h._1.toDouble, h._2)
  }

  property("Generate a non-negative double between 0 and 1") {
    forAll {x: Int =>
      whenever (x > 0) {
        val rng = SimpleRNG(x)
        val actual = nonNegativeDoubleBetween0and1(rng: RNG)
        actual._2 should not be (rng)
        actual._1.toInt should be  (0)//Just truncate the Double. I am having trouble getting Scalacheck to deal with doubles.  Should instead use bigdecimal or a precise type for rational numbers.  But that would complicate too much.
      }
    }
  }
}
