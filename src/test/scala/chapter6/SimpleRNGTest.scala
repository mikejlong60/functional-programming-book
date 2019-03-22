package chapter6

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 
import RNG._

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

  property("Generate a non-negative random number") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = nonNegativeInt(rng: RNG)
      actual._1 should be >= (0)
    }
  }


  property("Generate a non-negative double between 0 and 1") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = double(rng: RNG)
      actual._2 should not be (rng)
      actual._1.toInt should be  (0)//Just truncate the Double. I am having trouble getting Scalacheck to deal with doubles.  Should instead use bigdecimal or a precise type for rational numbers.  But that would complicate too much.
    }
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

  property("Generate pairs of random numbers in opposite order") {
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = doubleInt(rng)
      actual._2 should not be (rng)
      actual._1._2 should be >= (0) 
      actual._1._1.toInt should be  (0)
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

  property("Generate a sequence of random numbers") {
    forAll{x: Int =>
      whenever(x > 0 && x < 4000) {
        val rng = SimpleRNG(x)
        val actual = ints(x)(rng)

      }

    }
  }

}
