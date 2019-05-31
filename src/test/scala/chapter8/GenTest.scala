package chapter8

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks
import chapter6.{SimpleRNG, RNG, State}


class GenTest extends PropSpec with PropertyChecks with Matchers {

  property("Run choose within a range") {
    forAll{(start: Short, stopExclusive: Int) =>
      whenever (stopExclusive > start) {
        val actual = Gen.choose(start, stopExclusive)
        val rng = SimpleRNG(start)
        val fff =actual.sample.run(rng)
        fff._1 should be >= (start.toInt)
        fff._1 should be <  stopExclusive
      }
    }
  }

  property("Run unit which always generates a value of A") {
    forAll{a: Int =>
      val actual = Gen.unit(a)
      val rng = SimpleRNG(System.currentTimeMillis)
      actual.sample.run(rng)._1 should be (a)
    }
  }

  property("Run boolean which always generates a random boolean value") {
    forAll{a: Int =>
      val rng = SimpleRNG(a)
      val actual = Gen.boolean.sample.run(rng)._1
      val ex = RNG.nonNegativeInt(rng)
      val expected =  ex._1 % 2 == 0
      actual should be (expected)
    }
  }

  property("Generate a list of length n using the generator g") {
    forAll{(n : Short, x: Int) =>
      whenever(n > 0) {
        val rng = SimpleRNG(x)
        val g: Gen[List[Int]] = Gen.listOfN(n, Gen.choose(1, 5))
        val result: List[Int]  = g.sample.run(rng)._1
        result should (contain  (1) or contain(2) or contain(3) or contain (4) or contain (5)  and have size (n))
      }
    }
  }

  property("Use Map to produce a new generator") {
    forAll{ n: Int =>
      val a = Gen.unit(n)
      val r = Gen.map(a)(s => s.toString)
      val rng = SimpleRNG(n)
      val x = r.sample.run(rng)
      x._1 should be (n.toString)
    }
  }

  property("Use flatMap to produce a new generator") {
    forAll{ n: Int =>
      val a = Gen.unit(n)
      val r = Gen.flatMap(a)(s => Gen.unit(s.toString))
      val rng = SimpleRNG(n)
      val x = r.sample.run(rng)
      x._1 should be (n.toString)
    }
  }
}


