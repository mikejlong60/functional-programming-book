package chapter6

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalacheck.Gen

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
}
