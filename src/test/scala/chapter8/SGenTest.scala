package chapter8

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks
import chapter6.{SimpleRNG, RNG, State}


class SGenTest extends PropSpec with PropertyChecks with Matchers {

  property("Use SGen Map to produce a new SGen from unit") {
    forAll{ n: Int =>
      val a = Gen.unit(n)
      val b = a.unsized
      val c = b.map(x => x * 1000)
      val d = c(n)
      val rng = SimpleRNG(n)
      val e = d.sample.run(rng)
      e._1 should be (n * 1000)
    }
  }

  property("Use flatMap to produce a new SGen from unit") {
    forAll{ n: Int =>
      val a = Gen.unit(n)
      val b = a.unsized
      val intToString: (Int => Gen[String]) = a => Gen.unit(a.toString)
      val c = b.flatMap(x => intToString(x).unsized)
      val d = c(n)
      val rng = SimpleRNG(n)
      val e = d.sample.run(rng)
      e._1 should be (n.toString)
    }
  }

  property("Generate a list of length n using the generator g") {
    forAll{(n : Short) =>
      whenever(n > 0) {
        val rng = SimpleRNG(System.currentTimeMillis())
        val a = Gen.choose(19, 21)
        val b = a.unsized
        val c = b.listOf(a)
        val d = c(n)
        val result = d.sample.run(rng)
        result._1 should (contain  (19) or contain(20) or contain(21)  and have size (n))
      }
    }
  }
}


