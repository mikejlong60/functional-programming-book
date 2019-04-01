package chapter6

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 
import State._

class StateTest extends PropSpec with PropertyChecks with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 10, maxSize = 20, maxDiscarded=2000)

  property("Generating two random numbers using the same generator produces the same number ") {
    forAll { x: Int =>
      val rng = SimpleRNG(x)
      val (n1, rng2) = rng.nextInt
      val (n2, rng3) = rng.nextInt
      n1 should be (n2)
      rng should not be (rng3)
    }
  }

  property("Generate a non-negative random number") {
    forAll(minSuccessful(10000)) {x: Int =>
      val rng = SimpleRNG(x)
      val (n1, rng2) = nonNegativeInt(rng: RNG)
      n1 should be >= (0)
      rng2 should not be (rng)
    }
  }

  property("Generate a non-negative double between 0 and 1") {
    forAll(minSuccessful(10000)) {x: Int =>
      val rng = SimpleRNG(x)
      val actual = double(rng)
      val (i, r) = actual.run(rng)
      r should not be (rng)
      i.toInt should be  (0)
    }
  }

  property("Use map") {
    forAll {x: Int =>
      whenever (x < 4000000) {
      val rng = SimpleRNG(x)
      val (i, r) = rng.nextInt
      val s = State(run = (s: RNG) => (i, r))
      val (i2,r2) = s.map(x => x + 1200).run(rng)
      i2 should be  (i + 1200)
      }
    }
  }

  property("Use flatMap") {
    forAll {x: Int =>
      whenever (x < 4000000) {
      val rng = SimpleRNG(x)
      val (i, r) = rng.nextInt
      val s = State(run = (s: RNG) => (i, r))
      val (i2,r2) = s.flatMap(x => unit(x + 1200)).run(rng)
      i2 should be  (i + 1200)
      }
    }
  }

  property("Chain map and FlatMap") {
    forAll {x: Int =>
      whenever (x < 4000000) {
      val rng = SimpleRNG(x)
      val (i, r) = rng.nextInt
      val s = State(run = (s: RNG) => (i, r))
      val (i2,r2) = s.map(x => x + 1200).flatMap(x => unit(x + 1200)).flatMap(x => unit(x + 10)).run(rng)
      i2 should be  (i + 2400 + 10)
      }
    }
  }
}
